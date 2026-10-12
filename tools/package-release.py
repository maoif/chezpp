#!/usr/bin/env python3
"""Package a native Linux make-install prefix and its recursive ELF dependencies."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import shutil
import stat
import subprocess
import tarfile

# glibc and the architecture's ELF interpreter must match the receiving host.
HOST_LIBRARIES = re.compile(
    r'^(?:lib(?:c|m|dl|pthread|rt|resolv|util|anl|BrokenLocale)\.so\.[0-9]+|'
    r'libnss_[^.]+\.so\.2|ld-linux[^/]*\.so\.[0-9]+|ld64\.so\.[0-9]+)$')


def command(*arguments, env=None):
    result = subprocess.run(arguments, text=True, capture_output=True,
                            env=env or dict(os.environ, LC_ALL='C'))
    if result.returncode:
        raise RuntimeError(f'{arguments[0]} failed for {arguments[-1]}: {result.stderr.strip()}')
    return result.stdout


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def elf_files(prefix):
    files = set()
    for path in prefix.rglob('*'):
        if path.is_symlink() and not path.resolve().is_relative_to(prefix.resolve()):
            raise RuntimeError(f'installed symlink points outside package: {path}')
        if path.is_file():
            with path.open('rb') as stream:
                if stream.read(4) == b'\x7fELF':
                    files.add(path.resolve())
    return sorted(files)


def needed(path):
    return re.findall(r'\(NEEDED\).*\[([^]]+)\]', command('readelf', '-d', str(path)))


def collect_dependencies(prefix, extra_libraries=()):
    """Copy each resolved non-glibc dependency by SONAME; reject missing/conflicting libraries."""
    destination = prefix / 'lib/deps'
    destination.mkdir(parents=True, exist_ok=True)
    queue = elf_files(prefix)
    scanned = set()
    records = {}
    # Explicit roots cover libraries opened with dlopen, which have no DT_NEEDED edge.
    for source in extra_libraries:
        name = source.name
        records[name] = {'name': name, 'source': str(source.resolve()),
                         'sha256': digest(source), 'bundled': True}
        shutil.copy2(source, destination / name)
        queue.append(source.resolve())
    while queue:
        path = queue.pop()
        if path in scanned:
            continue
        scanned.add(path)
        requirements = needed(path)
        if not requirements:
            continue
        # Resolve against the original object's RPATH and system search paths, then
        # inspect every dependency itself. Do not flatten away SONAME conflicts.
        output = command('ldd', str(path))
        missing = re.findall(r'^\s*(\S+)\s+=>\s+not found', output, re.MULTILINE)
        if missing:
            raise RuntimeError(f'unresolved dependencies for {path}: {", ".join(missing)}')
        resolved = dict(re.findall(r'^\s*(\S+)\s+=>\s+(/.*?)\s+\(0x[0-9a-f]+\)',
                                   output, re.MULTILINE))
        for interpreter in re.findall(r'^\s*(/\S+)\s+\(0x[0-9a-f]+\)',
                                       output, re.MULTILINE):
            resolved[Path(interpreter).name] = interpreter
        for name in requirements:
            if '/' in name:
                raise RuntimeError(f'non-relocatable dependency name: {name}')
            if name not in resolved:
                raise RuntimeError(f'unresolved dependency {name} for {path}')
            source = Path(resolved[name]).resolve()
            checksum = digest(source)
            if name in records and records[name]['sha256'] != checksum:
                raise RuntimeError(f'conflicting dependency SONAME: {name}')
            bundled = not bool(HOST_LIBRARIES.fullmatch(name))
            records[name] = {'name': name, 'source': str(source), 'sha256': checksum,
                             'bundled': bundled}
            if bundled:
                target = destination / name
                if target.exists() and digest(target) != checksum:
                    raise RuntimeError(f'conflicting packaged dependency: {name}')
                shutil.copy2(source, target)
                queue.append(source)
    return sorted(records.values(), key=lambda item: item['name'])


def patch_runpaths(prefix):
    """Give every dynamic ELF object a relative path to the packaged dependency directory."""
    for path in elf_files(prefix):
        if needed(path):
            relative = os.path.relpath(prefix / 'lib/deps', path.parent)
            runpath = '$ORIGIN' if relative == '.' else '$ORIGIN/' + relative
            mode = stat.S_IMODE(path.stat().st_mode)
            try:
                path.chmod(mode | stat.S_IWUSR)
                command('patchelf', '--set-rpath', runpath, str(path))
            finally:
                path.chmod(mode)


def relocate_launcher(prefix):
    launcher = prefix / 'bin/chez++'
    lines = launcher.read_text().splitlines(keepends=True)
    if (len(lines) < 3 or not lines[0].startswith('#!') or
            not lines[1].startswith('(putenv "LIBCHEZPP" ') or
            not lines[2].startswith('(load ')):
        raise RuntimeError('unrecognized installed chez++ launcher')
    body = ('(putenv "LIBCHEZPP" (getenv "CHEZPP_PACKAGE_NATIVE"))\n'
            '(load (getenv "CHEZPP_PACKAGE_LIBRARY"))\n' + ''.join(lines[3:]))
    (prefix / 'lib/chez++.ss').write_text(body)
    boot_files = list((prefix / 'lib').glob('csv*/*/scheme.boot'))
    if len(boot_files) != 1:
        raise RuntimeError('expected exactly one installed ChezScheme boot directory')
    boot_directory = boot_files[0].parent.relative_to(prefix)
    launcher.write_text('''#!/bin/sh
set -eu
package_root=$(dirname "$(dirname "$(readlink -f -- "$0")")")
export CHEZPP_PACKAGE_NATIVE="$package_root/lib/libchezpp.so"
export CHEZPP_PACKAGE_LIBRARY="$package_root/lib/chezpp.lib"
export LD_LIBRARY_PATH="$package_root/lib/deps${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
''' + f'export SCHEMEHEAPDIR="$package_root/{boot_directory}"\n' + '''exec "$package_root/bin/scheme" --script "$package_root/lib/chez++.ss" "$@"
''')
    launcher.chmod(0o755)


def copy_licenses(prefix, repository, dependencies):
    destination = prefix / 'share/licenses'
    destination.mkdir(parents=True)
    for label, source in [('chezpp', repository / 'LICENSE'),
                          ('ChezScheme', repository / 'vendor/ChezScheme/NOTICE'),
                          ('ChezScheme-license', repository / 'vendor/ChezScheme/LICENSE')]:
        if source.is_file():
            shutil.copy2(source, destination / label)
    for item in dependencies if shutil.which('dpkg-query') else ():
        if not item['bundled']:
            continue
        # Ubuntu's merged /usr can give either spelling in dpkg's ownership database.
        source = item['source']
        candidates = [source]
        if source.startswith('/usr/lib/'):
            candidates.append(source[4:])
        for candidate in candidates:
            result = subprocess.run(['dpkg-query', '-S', candidate],
                                    text=True, capture_output=True)
            if result.returncode:
                continue
            package = result.stdout.split(': ', 1)[0].split(', ')[0]
            copyright_file = Path('/usr/share/doc') / package.split(':')[0] / 'copyright'
            if copyright_file.is_file():
                shutil.copy2(copyright_file, destination / (package + '-copyright'))
            break
    extra = Path('/usr/local/share/doc/blake3')
    if extra.is_dir():
        shutil.copytree(extra, destination / 'blake3')


def build_host_info():
    """Describe the actual build system and conservatively require its glibc version."""
    glibc = command('getconf', 'GNU_LIBC_VERSION').strip()
    if not re.fullmatch(r'glibc \d+\.\d+', glibc):
        raise RuntimeError(f'packaging requires glibc: {glibc}')
    return {'baseline': platform.freedesktop_os_release()['PRETTY_NAME'],
            'host_requirements': ['Linux', glibc.replace('glibc ', 'glibc >= '),
                                  'architecture-matching ELF loader']}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('prefix', type=Path, help='private, staged make-install prefix to modify')
    parser.add_argument('--output', required=True, type=Path)
    parser.add_argument('--arch', required=True, choices=['x64', 'arm64', 'riscv64'])
    parser.add_argument('--version', required=True)
    parser.add_argument('--commit', required=True)
    arguments = parser.parse_args()
    if not re.fullmatch(r'\d+\.\d+\.\d+@\d+-\d+-\d+', arguments.version):
        parser.error('version must have the form major.minor.patch@chezMajor-chezMinor-chezPatch')
    if not re.fullmatch(r'[0-9a-f]{40}', arguments.commit):
        parser.error('commit must be a full Git SHA')
    prefix = arguments.prefix.resolve()
    repository = Path(__file__).resolve().parents[1]
    x11 = Path(command('cc', '-print-file-name=libX11.so').strip())
    if not x11.is_absolute() or not x11.is_file():
        raise RuntimeError('cannot locate ChezScheme X11 runtime library')
    dependencies = collect_dependencies(prefix, [x11])
    patch_runpaths(prefix)
    relocate_launcher(prefix)
    copy_licenses(prefix, repository, dependencies)
    metadata = {'version': arguments.version, 'commit': arguments.commit,
                'arch': arguments.arch, **build_host_info(),
                'dependencies': dependencies,
                'build_options': (repository / '.chezpp-build-options').read_text().strip()}
    (prefix / 'BUILD-INFO.json').write_text(json.dumps(metadata, indent=2) + '\n')
    (prefix / 'README.txt').write_text(
        'Extract this directory anywhere and run bin/chez++.\n'
        f'Requires {", ".join(metadata["host_requirements"])}.\n'
        'Non-glibc ELF dependencies are bundled recursively in lib/deps.\n'
        'Build provenance and enabled features are in BUILD-INFO.json.\n')
    name = f'chezpp-{arguments.version}-linux-{arguments.arch}'
    arguments.output.mkdir(parents=True, exist_ok=True)
    archive = arguments.output / (name + '.tar.gz')
    with tarfile.open(archive, 'w:gz') as stream:
        stream.add(prefix, arcname=name)
    archive.with_name(archive.name + '.sha256').write_text(
        f'{digest(archive)}  {archive.name}\n')
    print(archive)


if __name__ == '__main__':
    main()
