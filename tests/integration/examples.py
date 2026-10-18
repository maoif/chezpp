#!/usr/bin/env python3
"""Run actual ChezPP examples against temporary files and loopback listeners.

Run from the repository root: python3 tests/integration/examples.py [http|core|crypto|benchmarks]
Requires curl with HTTP/2 and openssl for the HTTP group. No public hosts are contacted.
"""
import concurrent.futures
import contextlib
import csv
import hashlib
import http.server as http_server
import io
import json
import os
import re
import select
from pathlib import Path
import socket
import signal
import subprocess
import sys
import tempfile
import threading
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
def server(script, directory, *args, with_process=False):
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
        yield (port, process) if with_process else port
    finally:
        if process.poll() is None:
            process.send_signal(signal.SIGINT)
        try:
            _, log = process.communicate(timeout=5)
            assert b'file server closed' in log, log
        except subprocess.TimeoutExpired:
            process.kill()
            process.communicate()
            raise AssertionError('file server ignored shutdown signal')


def http_interrupt():
    with tempfile.TemporaryDirectory(prefix='chezpp-example-interrupt-') as name:
        directory = Path(name)
        with (directory / 'large').open('wb') as output:
            output.truncate(32 * 1024 * 1024)
        with server('examples/net/http-file-server.ss', directory,
                    with_process=True) as (port, process):
            # Error case: Ctrl-C during a response to a stalled reader must stop the server.
            with socket.socket() as connection:
                connection.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 4096)
                connection.settimeout(5)
                connection.connect(('127.0.0.1', port))
                connection.sendall(b'GET /large HTTP/1.1\r\nHost: localhost\r\n\r\n')
                assert connection.recv(1), 'response never started'
                process.send_signal(signal.SIGINT)
                process.wait(timeout=3)


def wget_errors():
    with tempfile.TemporaryDirectory(prefix='chezpp-example-wget-') as name:
        directory = Path(name)
        with server('examples/net/http-file-server.ss', directory) as port:
            for exists in (False, True):
                output = directory / 'output'
                if exists:
                    output.write_bytes(b'keep original')
                # Error case: HTTP errors must fail without creating/replacing the destination.
                result = example('examples/net/wget.ss',
                                 f'--uri=http://127.0.0.1:{port}/missing',
                                 f'--output={output}', ok=False)
                assert result.returncode != 0, result
                assert b'downloaded ' not in result.stderr, result.stderr
                assert b'100%' not in result.stderr, result.stderr
                if exists:
                    assert output.read_bytes() == b'keep original'
                else:
                    assert not output.exists()
            # A successful retry replaces the existing destination without leaking staging files.
            (directory / 'payload').write_bytes(b'downloaded content')
            example('examples/net/wget.ss', f'--uri=http://127.0.0.1:{port}/payload',
                    f'--output={output}')
            assert output.read_bytes() == b'downloaded content'
            assert sorted(p.name for p in directory.iterdir()) == ['output', 'payload']
            (directory / 'empty').touch()
            result = example('examples/net/wget.ss', f'--uri=http://127.0.0.1:{port}/empty',
                             f'--output={output}')
            assert output.read_bytes() == b'' and b'100% 0/0 bytes' in result.stderr, result
            assert not result.stdout, result.stdout


def wget_options():
    received = []

    class Handler(http_server.BaseHTTPRequestHandler):
        def do_GET(self):
            agent = self.headers.get('User-Agent')
            received.append(agent)
            # Error case: mirrors can reject downloaders that omit their User-Agent.
            if not agent:
                self.send_error(403)
                return
            if self.path == '/slow':
                time.sleep(2)
            self.send_response(200)
            self.send_header('Content-Length', '7')
            self.end_headers()
            try:
                self.wfile.write(b'payload')
            except (BrokenPipeError, ConnectionResetError):
                pass

        def log_message(self, *args):
            pass

    with tempfile.TemporaryDirectory(prefix='chezpp-wget-options-') as name:
        output = Path(name) / 'output'
        with http_server.ThreadingHTTPServer(('127.0.0.1', 0), Handler) as listener:
            thread = threading.Thread(target=listener.serve_forever, daemon=True)
            thread.start()
            base = f'http://127.0.0.1:{listener.server_port}'
            try:
                result = example('examples/net/wget.ss', f'--uri={base}/', f'--output={output}')
                assert b'100%' in result.stderr and b'7/7 bytes' in result.stderr, result.stderr
                assert not result.stdout, result.stdout
                assert received[-1].startswith('chezpp-wget'), received
                assert output.read_bytes() == b'payload'
                example('examples/net/wget.ss', f'--uri={base}/', f'--output={output}',
                        '--user-agent=Chezpp-test/1.0')
                assert received[-1] == 'Chezpp-test/1.0', received
                result = example('examples/net/wget.ss', f'--uri={base}/', f'--output={output}',
                                 '--progress=no')
                assert b'bytes' not in result.stderr and b'100%' not in result.stderr, result.stderr
                # Error case: invalid progress values must fail before sending a request.
                count = len(received)
                result = example('examples/net/wget.ss', f'--uri={base}/', f'--output={output}',
                                 '--progress=maybe', ok=False)
                assert result.returncode != 0 and len(received) == count, result
                # Error case: a short total timeout must fail and preserve the output.
                output.write_bytes(b'original')
                result = example('examples/net/wget.ss', f'--uri={base}/slow',
                                 f'--output={output}', '--timeout=1', ok=False)
                assert result.returncode != 0 and output.read_bytes() == b'original', result
                example('examples/net/wget.ss', f'--uri={base}/slow', f'--output={output}',
                        '--timeout=5')
                assert output.read_bytes() == b'payload'
                # Error case: zero timeout must fail before starting a request.
                count = len(received)
                result = example('examples/net/wget.ss', f'--uri={base}/',
                                 f'--output={output}', '--timeout=0', ok=False)
                assert result.returncode != 0 and len(received) == count, result
                assert list(Path(name).iterdir()) == [output]
            finally:
                listener.shutdown()
                thread.join(5)


def wget_progress():
    # A response stays open until the test observes live byte progress.
    for mode in ('known', 'unknown', 'redirect'):
        release = threading.Event()
        payload = b'x' * 65536

        class Handler(http_server.BaseHTTPRequestHandler):
            def do_GET(self):
                if self.path == '/redirect':
                    self.send_response(302)
                    self.send_header('Location', '/payload')
                    self.send_header('Content-Length', '4')
                    self.end_headers()
                    self.wfile.write(b'skip')
                    return
                self.send_response(200)
                if mode != 'unknown':
                    self.send_header('Content-Length', str(len(payload) * 2))
                self.end_headers()
                self.wfile.write(payload)
                self.wfile.flush()
                assert release.wait(10), 'test never released the streaming response'
                self.wfile.write(payload)

            def log_message(self, *args):
                pass

        with tempfile.TemporaryDirectory(prefix='chezpp-wget-progress-') as name:
            output = Path(name) / 'output'
            with http_server.ThreadingHTTPServer(('127.0.0.1', 0), Handler) as listener:
                thread = threading.Thread(target=listener.serve_forever, daemon=True)
                thread.start()
                path = 'redirect' if mode == 'redirect' else 'payload'
                process = subprocess.Popen(
                    ['./chez++', '--script', 'examples/net/wget.ss',
                     f'--uri=http://127.0.0.1:{listener.server_port}/{path}', f'--output={output}'],
                    stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                captured = bytearray()
                pattern = ((rb' [1-9][0-9]* bytes' if mode == 'unknown'
                            else rb' [0-9]{1,2}% [1-9][0-9]*/131072 bytes')
                           + rb' +[0-9]*[1-9][0-9]*(?:\.[0-9]+)? (?:B|KiB|MiB|GiB)/s')
                try:
                    deadline = time.monotonic() + 8
                    while not re.search(pattern, captured):
                        remaining = deadline - time.monotonic()
                        assert remaining > 0, (mode, bytes(captured))
                        ready, _, _ = select.select([process.stderr], [], [], remaining)
                        assert ready, (mode, bytes(captured))
                        data = os.read(process.stderr.fileno(), 4096)
                        assert data, (mode, bytes(captured))
                        captured.extend(data)
                    # Progress must arrive before the response finishes or the file is published.
                    assert process.poll() is None and not output.exists(), bytes(captured)
                    assert b'/4 bytes' not in captured, bytes(captured)
                    if mode == 'unknown':
                        assert b'(total unknown)' in captured, bytes(captured)
                    release.set()
                    stdout, stderr = process.communicate(timeout=10)
                    captured.extend(stderr)
                    assert process.returncode == 0 and not stdout, bytes(captured)
                    assert b'100% 131072/131072 bytes' in captured, bytes(captured)
                    assert b'########################' in captured, bytes(captured)
                    assert output.read_bytes() == payload * 2
                finally:
                    release.set()
                    if process.poll() is None:
                        process.kill()
                        process.communicate()
                    listener.shutdown()
                    thread.join(5)


def wget_speed():
    release = threading.Event()
    started = threading.Event()
    payload = b'x' * 2048
    tail = b'y' * 128

    class Handler(http_server.BaseHTTPRequestHandler):
        def do_GET(self):
            self.send_response(200)
            self.send_header('Content-Length', str(len(payload) + len(tail)))
            self.end_headers()
            self.wfile.write(payload)
            self.wfile.flush()
            started.set()
            assert release.wait(10), 'test never released the slow tail'
            self.wfile.write(tail)

        def log_message(self, *args):
            pass

    with tempfile.TemporaryDirectory(prefix='chezpp-wget-speed-') as name:
        output = Path(name) / 'output'
        with http_server.ThreadingHTTPServer(('127.0.0.1', 0), Handler) as listener:
            thread = threading.Thread(target=listener.serve_forever, daemon=True)
            thread.start()
            process = subprocess.Popen(
                ['./chez++', '--script', 'examples/net/wget.ss',
                 f'--uri=http://127.0.0.1:{listener.server_port}/', f'--output={output}'],
                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            captured = bytearray()
            try:
                assert started.wait(5)
                deadline = time.monotonic() + 5
                while b'2048/2176 bytes' not in captured:
                    remaining = deadline - time.monotonic()
                    assert remaining > 0, bytes(captured)
                    ready, _, _ = select.select([process.stderr], [], [], remaining)
                    assert ready, bytes(captured)
                    chunk = os.read(process.stderr.fileno(), 4096)
                    assert chunk, bytes(captured)
                    captured.extend(chunk)
                time.sleep(.3)
                release.set()
                stdout, stderr = process.communicate(timeout=10)
                captured.extend(stderr)
                assert process.returncode == 0 and not stdout, bytes(captured)
                speed = re.search(rb'100% 2176/2176 bytes +([0-9.]+) B/s', captured)
                assert speed and 0 < float(speed[1]) < 1024, bytes(captured)
                # The recent interval contains only the slow tail, not the initial burst.
                assert output.read_bytes() == payload + tail
            finally:
                release.set()
                if process.poll() is None:
                    process.kill()
                    process.communicate()
                listener.shutdown()
                thread.join(5)


def h2_window():
    failures = []
    release = threading.Event()

    def frame(kind, flags, stream, payload):
        return len(payload).to_bytes(3, 'big') + bytes([kind, flags]) + stream.to_bytes(4, 'big') + payload

    def serve(listener):
        try:
            connection, _ = listener.accept()
            with connection:
                connection.settimeout(5)
                data = b''
                while len(data) < 24:
                    chunk = connection.recv(24 - len(data))
                    assert chunk, 'client closed during HTTP/2 preface'
                    data += chunk
                assert data == b'PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n', data
                # Valid nginx-style peer settings: stream window 65536; connection window 2^31-1.
                connection.sendall(frame(4, 0, 0, b'\x00\x04' + (65536).to_bytes(4, 'big')) +
                                   frame(8, 0, 0, (2147483647 - 65535).to_bytes(4, 'big')))
                data = b''
                while True:
                    while len(data) < 9:
                        chunk = connection.recv(4096)
                        assert chunk, 'client closed before sending request headers'
                        data += chunk
                    length = int.from_bytes(data[:3], 'big')
                    while len(data) < 9 + length:
                        chunk = connection.recv(4096)
                        assert chunk, 'client closed during frame'
                        data += chunk
                    kind, stream = data[3], int.from_bytes(data[5:9], 'big')
                    data = data[9 + length:]
                    assert kind != 7, 'client incorrectly rejected the window update with GOAWAY'
                    if kind == 1:
                        # HPACK index 8 is :status 200; literal indexed name 28 is content-length.
                        # Use a nonempty, length-delimited body to exercise normal read completion.
                        connection.sendall(frame(4, 1, 0, b'') +
                                           frame(1, 4, stream, b'\x88\x0f\x0d\x017') +
                                           frame(0, 0, stream, b'pay'))
                        assert release.wait(10), 'HTTP/2 progress was not observed'
                        connection.sendall(frame(0, 1, stream, b'load'))
                        # Keep HTTP/2 alive until the client closes, instead of racing response delivery.
                        try:
                            while connection.recv(4096):
                                pass
                        except ConnectionResetError:
                            pass
                        return
        except Exception as error:
            failures.append(error)

    with socket.socket() as listener, tempfile.TemporaryDirectory(prefix='chezpp-h2-window-') as name:
        listener.bind(('127.0.0.1', 0))
        listener.listen()
        listener.settimeout(5)
        thread = threading.Thread(target=serve, args=(listener,), daemon=True)
        thread.start()
        output = Path(name) / 'output'
        process = subprocess.Popen(
            ['./chez++', '--script', 'examples/net/wget.ss',
             f'--uri=http://127.0.0.1:{listener.getsockname()[1]}/',
             f'--output={output}', '--version=h2'],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        captured = bytearray()
        try:
            deadline = time.monotonic() + 8
            while b'43% 3/7 bytes' not in captured:
                remaining = deadline - time.monotonic()
                assert remaining > 0, bytes(captured)
                ready, _, _ = select.select([process.stderr], [], [], remaining)
                assert ready, bytes(captured)
                chunk = os.read(process.stderr.fileno(), 4096)
                assert chunk, bytes(captured)
                captured.extend(chunk)
            assert process.poll() is None and not output.exists()
            release.set()
            stdout, stderr = process.communicate(timeout=10)
            captured.extend(stderr)
            assert process.returncode == 0 and not stdout, bytes(captured)
            assert output.read_bytes() == b'payload'
            assert b'100% 7/7 bytes' in captured, bytes(captured)
        finally:
            release.set()
            if process.poll() is None:
                process.kill()
                process.communicate()
            thread.join(6)
        assert not thread.is_alive() and not failures, failures



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
        wget_errors()
        wget_options()
        wget_progress()
        wget_speed()
        h2_window()
        http_interrupt()
        summary('HTTP examples: HTTP/1, cleartext H2, HTTPS/H2, trust, paths, concurrent requests, wget errors, shutdown passed')


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
    groups = {'help': help_commands, 'http': http, 'core': core, 'crypto': crypto,
              'benchmarks': benchmarks, 'wget-errors': wget_errors, 'http-interrupt': http_interrupt,
              'wget-options': wget_options, 'wget-progress': wget_progress, 'wget-speed': wget_speed, 'h2-window': h2_window}
    for group in sys.argv[1:] or ['help', 'http', 'core', 'crypto', 'benchmarks']:
        groups[group]()
