#!/usr/bin/env python3
"""Exercise packaging with a real, transitive ELF dependency graph."""
import importlib.util
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

MODULE = Path(__file__).resolve().parents[1] / 'tools/package-release.py'


class ReleasePackageTests(unittest.TestCase):
    def setUp(self):
        self.assertTrue(MODULE.exists(), 'release packaging tooling is missing')
        spec = importlib.util.spec_from_file_location('release_package', MODULE)
        self.package = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.package)
        self.temporary = tempfile.TemporaryDirectory(prefix='chezpp-package-test-')
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.prefix = self.root / 'installed'
        (self.prefix / 'bin').mkdir(parents=True)
        (self.prefix / 'lib').mkdir()
        self.external = self.root / 'external'
        self.external.mkdir()
        self.compile('leaf', 'int leaf(void) { return 42; }')
        self.compile('middle', 'extern int leaf(void); int middle(void) { return leaf(); }',
                     ['-lleaf'])
        source = self.root / 'main.c'
        source.write_text('extern int middle(void); int main(void) { return middle() != 42; }')
        subprocess.run(['cc', str(source), '-L' + str(self.external), '-lmiddle',
                        '-Wl,-rpath,' + str(self.external),
                        '-o', str(self.prefix / 'bin/probe')], check=True)

    def compile(self, name, source, libraries=()):
        path = self.root / (name + '.c')
        path.write_text(source)
        subprocess.run(['cc', '-shared', '-fPIC', str(path),
                        '-Wl,-soname,lib' + name + '.so', '-L' + str(self.external),
                        '-Wl,-rpath,' + str(self.external), *libraries,
                        '-o', str(self.external / ('lib' + name + '.so'))], check=True)

    def test_transitive_dependencies_run_after_original_libraries_are_removed(self):
        dependencies = self.package.collect_dependencies(self.prefix)
        self.assertTrue((self.prefix / 'lib/deps/libleaf.so').is_file())
        self.assertTrue((self.prefix / 'lib/deps/libmiddle.so').is_file())
        self.assertFalse((self.prefix / 'lib/deps/libc.so.6').exists())
        self.assertEqual({item['name'] for item in dependencies if item['bundled']},
                         {'libleaf.so', 'libmiddle.so'})
        self.package.patch_runpaths(self.prefix)
        shutil.rmtree(self.external)
        relocated = self.root / 'moved package'
        self.prefix.rename(relocated)
        env = dict(os.environ)
        env.pop('LD_LIBRARY_PATH', None)
        result = subprocess.run([str(relocated / 'bin/probe')], env=env,
                                capture_output=True)
        self.assertEqual(result.returncode, 0, result.stderr.decode())

    def test_unresolved_transitive_dependency_rejects_package(self):
        # Error case: an indirect runtime library is missing despite successful linking.
        (self.external / 'libleaf.so').unlink()
        with self.assertRaisesRegex(RuntimeError, 'libleaf.so'):
            self.package.collect_dependencies(self.prefix)

    def test_dlopen_seed_includes_its_recursive_dependencies(self):
        # ChezScheme's X11 clipboard library is opened at runtime, outside DT_NEEDED.
        dependencies = self.package.collect_dependencies(
            self.prefix, [self.external / 'libmiddle.so'])
        self.assertTrue((self.prefix / 'lib/deps/libmiddle.so').is_file())
        self.assertTrue((self.prefix / 'lib/deps/libleaf.so').is_file())
        self.assertEqual({item['name'] for item in dependencies if item['bundled']},
                         {'libleaf.so', 'libmiddle.so'})

    def test_conflicting_sonames_reject_package(self):
        # Error case: two installed objects resolve the same SONAME to different libraries.
        alternative = self.root / 'alternative'
        shutil.copytree(self.external, alternative)
        original = self.external
        self.external = alternative
        self.compile('leaf', 'int leaf(void) { return 7; }')
        source = self.root / 'other.c'
        source.write_text('extern int leaf(void); int other(void) { return leaf(); }')
        subprocess.run(['cc', '-shared', '-fPIC', str(source),
                        '-L' + str(alternative), '-lleaf',
                        '-Wl,-rpath,' + str(alternative),
                        '-o', str(self.prefix / 'lib/other.so')], check=True)
        self.external = original
        with self.assertRaisesRegex(RuntimeError, 'conflict'):
            self.package.collect_dependencies(self.prefix)

    def test_loader_needed_by_shared_library_remains_host_dependency(self):
        # glibc libraries can list the interpreter in DT_NEEDED; ldd prints it bare.
        source = self.root / 'cpp.cpp'
        source.write_text('#include <iostream>\nint main() { std::cout << "ok"; }')
        subprocess.run(['c++', str(source), '-o', str(self.prefix / 'bin/cpp')], check=True)
        dependencies = self.package.collect_dependencies(self.prefix)
        loaders = [item for item in dependencies if item['name'].startswith('ld-linux')]
        self.assertTrue(loaders)
        self.assertTrue(all(not item['bundled'] for item in loaders))

    def test_symlink_outside_install_prefix_rejects_package(self):
        # Error case: an install symlink would refer outside the released archive.
        (self.prefix / 'bin/escape').symlink_to(self.external / 'libleaf.so')
        with self.assertRaisesRegex(RuntimeError, 'outside'):
            self.package.collect_dependencies(self.prefix)


if __name__ == '__main__':
    unittest.main()
