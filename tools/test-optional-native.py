#!/usr/bin/env python3
"""Audit optional exports and exercise every disabled native fallback signature."""
import pathlib
import re
import subprocess
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
FILES = ['chezpp/net/ffi.ss', 'chezpp/net/lws/ffi.ss', 'chezpp/crypto/ffi.ss',
         'chezpp/digest.ss', 'chezpp/hash.ss', 'chezpp/uuid.ss']
PATTERN = re.compile(r'\(foreign-procedure\s+"([^"]+)"\s+\(([^()]*)\)\s+([^()\s]+)\)')


def declarations():
    result = {}
    for name in FILES:
        source = (ROOT / name).read_text()
        source = re.sub(r'#\|.*?\|#', '', source, flags=re.S)
        source = re.sub(r';[^\n]*', '', source)
        for symbol, arguments, returns in PATTERN.findall(source):
            result[symbol] = (arguments.split(), returns)
    return result


def library(symbol):
    if symbol.startswith(('crypto_', 'chezpp_net_tls', 'digester_', 'digest_')):
        return 'blake3' if 'blake3' in symbol else 'openssl'
    if symbol.startswith(('hash_', 'hasher_', 'chezpp_xxhash')):
        return 'xxhash'
    if symbol.startswith('chezpp_blake3'):
        return 'blake3'
    if symbol.startswith(('chezpp_generate_uuid', 'chezpp_uuid', 'chezpp_string_to_uuid')):
        return 'uuid'
    for prefix, dependency in [('chezpp_zlib', 'zlib'), ('chezpp_net_dns', 'cares'),
                               ('chezpp_net_idna', 'idn2'), ('chezpp_net_ftp', 'curl'),
                               ('chezpp_net_ssh', 'ssh'), ('chezpp_net_sftp', 'ssh'),
                               ('chezpp_net_scp', 'ssh'), ('chezpp_net_ws', 'websockets'), ('chezpp_net_websocket', 'websockets'),
                               ('chezpp_lws', 'websockets'), ('chezpp_net_grpc', 'grpc')]:
        if symbol.startswith(prefix):
            return dependency
    return None


def main():
    sources = list((ROOT / 'chezpp/c').rglob('*.c')) + list((ROOT / 'chezpp/c').rglob('*.h'))
    for source in sources:
        if source.name == 'lws_http2_fixture.c':
            continue
        assert not re.search(r'\b(dl(?:open|close)|dlsym)\s*\(', source.read_text()), source
    symbols = declarations()
    exports = subprocess.check_output(['nm', '-D', '--defined-only', str(ROOT / 'libchezpp.so')], text=True)
    exported = {line.split()[-1] for line in exports.splitlines()}
    missing = sorted(set(symbols) - exported)
    assert not missing, f'Missing FFI exports: {missing}'
    if '--sentinels' not in sys.argv:
        return
    config = (ROOT / 'chezpp/c/build-config.h').read_text()
    mapping = {'ssh': 'LIBSSH', 'websockets': 'WEBSOCKETS'}
    tests = ['(import (chezscheme))', '(load-shared-object "./libchezpp.so")']
    for symbol, (arguments, returns) in symbols.items():
        dependency = library(symbol)
        if not dependency or 'load_error' in symbol or symbol == 'chezpp_lws_status':
            continue
        macro = mapping.get(dependency, dependency.upper())
        if not re.search(rf'#define CHEZPP_WITH_{macro}\s+0\b', config):
            continue
        values = ['"disabled.example"' if argument == 'string' else
                  '#vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)' if argument in ('ptr', 'scheme-object') else
                  '0.0' if argument == 'double' else '1' for argument in arguments]
        invocation = f'((foreign-procedure "{symbol}" ({" ".join(arguments)}) {returns}) {" ".join(values)})'
        if returns == 'void':
            tests += [f';; Disabled cleanup/update must tolerate an invalid native handle: {symbol}.', invocation, '']
        else:
            expected = '(and (vector? result) (= (vector-length result) 2) (eq? (vector-ref result 0) \'error))' if returns in ('ptr', 'scheme-object') else f'(= result {"-1" if returns == "int" and "flag" not in symbol else "0"})'
            tests += [f';; Disabled operation returns its ABI sentinel without dereferencing handles: {symbol}.',
                      f'(let ([result {invocation}]) (unless {expected} (error \'{symbol} "invalid fallback result" result)))', '']
    completed = subprocess.run(['scheme', '--script', '/dev/stdin'], input='\n'.join(tests), text=True, cwd=ROOT, capture_output=True)
    assert completed.returncode == 0 and not completed.stdout and not completed.stderr, completed.stdout + completed.stderr


if __name__ == '__main__':
    main()
