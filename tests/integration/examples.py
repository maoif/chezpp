#!/usr/bin/env python3
"""Run actual ChezPP examples against temporary files and loopback listeners.

Run from the repository root: python3 tests/integration/examples.py [http|core|crypto|benchmarks]
Requires curl with HTTP/2 and openssl for the HTTP group. No public hosts are contacted.
"""
import concurrent.futures
import contextlib
import csv
import hashlib
import io
import json
import os
import re
from pathlib import Path
import socket
import signal
import subprocess
import sys
import tempfile
import time

def summary(message):
    if os.environ.get('CHEZPP_EXAMPLE_TEST_VERBOSE'):
        print(message)


ROOT = Path(__file__).resolve().parents[2]
os.chdir(ROOT)


def run(*args, ok=True, **kwargs):
    result = subprocess.run(args, capture_output=True, timeout=20, **kwargs)
    if ok and result.returncode:
        raise AssertionError((args, result.returncode, result.stdout, result.stderr))
    return result


def example(path, *args, **kwargs):
    return run('./chez++', '--script', path, *args, **kwargs)


def reserve_port():
    with socket.socket() as sock:
        sock.bind(('127.0.0.1', 0))
        return sock.getsockname()[1]


@contextlib.contextmanager
def server(script, directory, *args):
    port = reserve_port()
    process = subprocess.Popen(['./chez++', '--script', script, f'--dir={directory}',
                                f'--port={port}', *args], stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
    try:
        for _ in range(100):
            if process.poll() is not None:
                raise AssertionError(process.communicate())
            try:
                with socket.create_connection(('127.0.0.1', port), .1):
                    break
            except OSError:
                time.sleep(.02)
        else:
            raise AssertionError('listener startup timed out')
        yield port
    finally:
        process.send_signal(signal.SIGINT)
        try:
            _, log = process.communicate(timeout=5)
            assert b'file server closed' in log, log
        except subprocess.TimeoutExpired:
            process.kill()
            process.communicate()


def http():
    with tempfile.TemporaryDirectory(prefix='chezpp-example-http-') as name:
        directory = Path(name)
        data = bytes(range(256)) * 4096
        (directory / 'payload').write_bytes(data)
        (directory / 'empty').touch()
        (directory / 'escape').symlink_to('/etc/passwd')
        (directory / 'subdir').mkdir()
        cert, key = directory / 'cert.pem', directory / 'key.pem'
        run('openssl', 'req', '-x509', '-newkey', 'rsa:2048', '-nodes', '-days', '1',
            '-subj', '/CN=localhost', '-addext', 'subjectAltName=DNS:localhost,IP:127.0.0.1',
            '-keyout', str(key), '-out', str(cert))
        for script, proto, scheme, tls in [
            ('http-file-server.ss', '--http1.1', 'http', []),
            ('http2-file-server.ss', '--http2-prior-knowledge', 'http', []),
            ('https-file-server.ss', '--http2', 'https',
             [f'--cert={cert}', f'--key={key}'])]:
            with server('examples/net/' + script, directory, *tls) as port:
                base = f'{scheme}://127.0.0.1:{port}'
                curl = ['curl', '--noproxy', '*', '--max-time', '8', '-sS', proto]
                if tls:
                    curl += ['--cacert', str(cert)]
                def get(path):
                    return run(*curl, '--path-as-is', '-w', '\n%{http_code}:%{http_version}',
                               base + path).stdout
                response = get('/payload')
                expected = b'2' if script != 'http-file-server.ss' else b'1.1'
                assert response == data + b'\n200:' + expected, response[-80:]
                assert get('/empty') == b'\n200:' + expected
                # Error cases: directories, missing files, traversal, and symlink escape.
                for path in ['/subdir', '/missing', '/../etc/passwd', '/%2e%2e/etc/passwd',
                             '/escape', '/%2fetc/passwd']:
                    assert get(path).split(b'\n')[-1].startswith((b'400:', b'404:')), path
                assert run(*curl, '-X', 'POST', '-w', '%{http_code}', base + '/payload').stdout == b'405'
                assert run(*curl, '-X', 'POST', '-d', '', '-w', '%{http_code}',
                           base + '/payload').stdout == b'405'
                with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
                    assert all(x == response for x in executor.map(get, ['/payload'] * 8))
                # Cancel an incomplete request, then verify the listener still serves traffic.
                with socket.create_connection(('127.0.0.1', port)) as connection:
                    connection.sendall(b'GET /payload HTTP/1.1\r\n')
                assert get('/payload') == response
                if tls:
                    # Error case: an untrusted certificate must not be silently accepted.
                    assert run('curl', '--noproxy', '*', '-sS', '--max-time', '3',
                               base + '/payload', ok=False).returncode != 0
                    assert run(*curl, '--http1.1', '-w', '%{http_version}',
                               base + '/payload').stdout == data + b'1.1'
                else:
                    output = directory / 'download'
                    output.unlink(missing_ok=True)
                    version = 'h2' if script.startswith('http2') else 'http/1.1'
                    example('examples/net/wget.ss', f'--uri={base}/payload',
                            f'--output={output}', f'--version={version}')
                    assert output.read_bytes() == data
        summary('HTTP examples: HTTP/1, cleartext H2, HTTPS/H2, trust, path rejection, concurrent requests, wget passed')


def help_commands():
    inventory = Path('tests/examples.ss').read_text().split('(define static-entry-point*')[0]
    for path in re.findall(r'"((?:examples|benchmarks)/[^" ]+\.ss)"', inventory):
        if path.startswith('examples/net/'):
            continue  # Network help has its own exhaustive entry-point inventory.
        for flag in ['-h', '--help']:
            result = example(path, flag)
            assert result.stdout and not result.stderr, (path, flag, result)
    summary('Library examples and benchmarks: both CLI help flags passed')


def core():
    for path, arguments in {
        'parser/xml': ['--file=tests/data/xml-basic.xml'],
        'parser/json': ['--file=tests/data/json5-example.json5'],
        'parser/toml': ['--file=tests/data/pyproj.toml'],
        'parser/scheme': ['--file=examples/data/sample.scm'],
        'parser/elf': ['--file=.chezscheme-install/bin/scheme'],
        'navigator/traversal': [], 'transducer/pipeline': [], 'iter/operations': [],
        'glob/files': ['--limit=2'], 'containers/treeset': [],
        'containers/treemap': ['--values=9,8'], 'containers/heap': [],
        'containers/dlist': [], 'containers/array': ['--size=0'],
    }.items():
        assert example('examples/' + path + '.ss', *arguments).stdout
    # Error cases: each parser rejects a missing file and a malformed input.
    with tempfile.TemporaryDirectory(prefix='chezpp-example-parser-') as name:
        for parser, invalid in {'xml': b'<bad', 'json': b'{', 'toml': b'[',
                                'scheme': b'(', 'elf': b'not ELF'}.items():
            path = Path(name) / parser
            path.write_bytes(invalid)
            for file in [path, Path(name) / 'missing']:
                assert example(f'examples/parser/{parser}.ss', f'--file={file}',
                               ok=False).returncode != 0
    summary('Core examples: parsers, data processing, and containers passed')


def crypto():
    with tempfile.TemporaryDirectory(prefix='chezpp-example-crypto-') as name:
        directory = Path(name)
        source = directory / 'input'
        source.write_bytes(bytes(range(256)) * 20)
        env = dict(os.environ, CHEZPP_EXAMPLE_PASSWORD='test-only-secret')
        for method in ['aes-256-gcm', 'chacha20-poly1305']:
            encrypted, decrypted = directory / method, directory / (method + '.plain')
            example('examples/crypto/encrypt-file.ss', f'--input={source}',
                    f'--output={encrypted}', f'--method={method}', env=env)
            example('examples/crypto/encrypt-file.ss', '--mode=decrypt',
                    f'--input={encrypted}', f'--output={decrypted}', env=env)
            assert decrypted.read_bytes() == source.read_bytes()
            # Error cases: wrong password, changed metadata/payload/tag, and truncation.
            for index, changed in enumerate([b'wrong-password', b'header', b'payload', b'tag', b'truncated']):
                raw = bytearray(encrypted.read_bytes())
                bad_env = env.copy()
                if changed == b'wrong-password':
                    bad_env['CHEZPP_EXAMPLE_PASSWORD'] = 'wrong'
                elif changed == b'header': raw[raw.index(b'\n') + 1] ^= 1
                elif changed == b'payload': raw[-30] ^= 1
                elif changed == b'tag': raw[-1] ^= 1
                else: raw = raw[:12]
                corrupt, output = directory / 'corrupt', directory / f'rejected-{index}'
                corrupt.write_bytes(raw)
                result = example('examples/crypto/encrypt-file.ss', '--mode=decrypt',
                                 f'--input={corrupt}', f'--output={output}', env=bad_env, ok=False)
                assert result.returncode and not output.exists()
        methods = ['sha256', 'md5']
        features = Path('chezpp/c/build-config.h').read_text()
        if '#define CHEZPP_WITH_XXHASH 1' in features:
            methods += ['xxhash32', 'xxhash64']
        if '#define CHEZPP_WITH_BLAKE3 1' in features:
            methods += ['blake3']
        for method in methods:
            output = example('examples/crypto/hash-file.ss', f'--file={source}', f'--method={method}').stdout
            assert output
            if method in ['sha256', 'md5']:
                assert hashlib.new(method, source.read_bytes()).hexdigest().encode() in output
    summary('Crypto examples: round-trips, hashes, and authentication rejection cases passed')


def benchmarks():
    operations = {'treeset': ['populate', 'lookup', 'traverse', 'update', 'delete'],
                  'treemap': ['populate', 'lookup', 'traverse', 'update', 'delete'],
                  'vectors': ['fill', 'read', 'map', 'fold'],
                  'arrays': ['append', 'index', 'map', 'fold']}
    with tempfile.TemporaryDirectory(prefix='chezpp-example-benchmarks-') as name:
        for script, expected in operations.items():
            for fmt in ['text', 'csv', 'json', 'datum']:
                output = Path(name) / (script + '.' + fmt)
                result = example(f'benchmarks/{script}.ss', '--size=8', '--samples=2',
                                 '--warmup=0', '--min-time=0.0', '--max-iterations=1',
                                 f'--format={fmt}', f'--output={output}')
                assert not result.stdout, result.stdout
                content = output.read_text()
                assert all(op in content for op in expected), (script, fmt, content)
                if fmt == 'json':
                    records = json.loads(content)['results']
                    assert all(not record['error'] for record in records), records
                    assert 'VARIANT=' in json.loads(content)['metadata']['build']
                    for record in records:
                        assert len(record['samples']) == 2, record
                        assert all(key in record['summary']['cpu-ns'] for key in
                                   ['mean', 'median', 'min', 'max', 'stddev', 'confidence-interval'])
                        assert all(key in record['samples'][0] for key in
                                   ['gc-count', 'gc-bytes', 'bytes', 'cpu-ns', 'real-ns',
                                    'cost-center-allocation', 'cost-center-instructions'])
                if fmt == 'csv':
                    records = list(csv.DictReader(io.StringIO(content)))
                    assert all(record['error'] == '#f' for record in records), records
        # Error cases: invalid sizes/configurations and an unmatched filter must fail.
        for args in [('--samples=0',), ('--max-iterations=0',), ('--filter=missing',)]:
            assert example('benchmarks/treeset.ss', *args, ok=False).returncode != 0
        selected = []
        for _ in range(2):
            result = example('benchmarks/treemap.ss', '--size=0', '--samples=1',
                             '--min-time=0.0', '--max-iterations=1', '--format=json',
                             '--filter=fxtreemap-lookup')
            selected.append(json.loads(result.stdout)['results'][0]['name'])
        assert selected == ['fxtreemap-lookup'] * 2
    summary('Benchmarks: all operations and four report formats passed')


if __name__ == '__main__':
    groups = {'help': help_commands, 'http': http, 'core': core, 'crypto': crypto, 'benchmarks': benchmarks}
    for group in sys.argv[1:] or groups:
        groups[group]()
