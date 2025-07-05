# chezpp: ChezScheme Enhancements (Work-in-Progress)

Functionalities:

- adt: algebraic datatype, plus simpler record syntax
- comprehension: powerful comprehensions and reductions for list, vector, hashtable, etc., in for-loop style
- control: various control features
- file: rich file and IO utilites, filesystem watcher
- irregex: regex support
- iter: iterator support (eliminating intermediate data structures when processing data through a series of procedures)
- list: more list operations
- match: powerful pattern matching
- os: OS utilities
- string: more string operations
- utils: miscellaneous useful procedures like type checking and random data generator
- vector: more vector/fxvector/flvector operations
- data structures:
  - array: dynamic vector
  - dlist: doubly-linked list
  - hashset: unordered set using hash function
  - treemap: ordered map based on reb-black tree
  - treeset: ordered set based on reb-black tree
  - stack: Last-In-First-Out container
  - queue: First-In-First-Out container
  - heap: binary heap/priority queue
- parser combinators:
  - support both textual and binary parsers
  - context-sensitive parsing using monadic `<bind>`
  - simple csv, json5, xml parsers

# Installation and Usage

Build requirements:

- gcc/clang
- make
- ChezScheme

Build:

```
git clone --depth=1 https://github.com/maoif/chezpp.git
cd chezpp
make
```

`gcc` is used by default. To use clang, set the `CC` variable:

```
make CC=clang
```

Run:

```
make run
```

Or launch `chez++` directly:

```
./chez++
```


## Customize `scheme` executable path

By default, the build system assumes ChezScheme is installed and the `scheme` command is available.
If it is not the case, you have to provide the path to the `scheme` command when building chezpp:

```
make SCHEME=path/to/scheme/executable
```


## Install

Other than running the `chez++` command directly after `make`, you can install chezpp to a given location
by setting the `PREFIX` variable:

```
make install PREFIX=$PWD/install
```

The command above installs chezpp files under `install/`.
You can run `chez++` by invoking `$PWD/install/bin/chez++`.
The path given *must* be absolute.



# Test

To test chezpp, make sure the library is already built, then `cd tests`.

Run test of all libraries:

```
make test-all
```

Run test of some libraries by specifying the file name under `tests/`:

```
make test-some TEST=treemap

# alternatively, you can specify more libraries
make test-some TEST='treemap treeset'
```
