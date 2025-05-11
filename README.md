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

- gcc
- make
- ChezScheme

Build:

```
git clone --depth=1 https://github.com/maoif/chezpp.git
cd chezpp
make
```

Run:

```
make run
```

Or launch `chez++` directly:

```
./chez++
```


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
