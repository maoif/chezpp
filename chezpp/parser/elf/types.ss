(library (chezpp parser elf types)
  (export make-elf-file elf-file? elf-file-identification elf-file-header
          elf-file-program-headers elf-file-sections
          make-elf-identification elf-identification? elf-identification-class
          elf-identification-endianness elf-identification-version elf-identification-osabi
          elf-identification-abi-version
          make-elf-header elf-header? elf-header-type elf-header-machine elf-header-version
          elf-header-entry elf-header-program-header-offset elf-header-section-header-offset
          elf-header-flags elf-header-header-size elf-header-program-header-entry-size
          elf-header-program-header-count elf-header-section-header-entry-size
          elf-header-section-header-count elf-header-section-name-index
          make-elf-program-header elf-program-header? elf-program-header-type
          elf-program-header-flags elf-program-header-offset elf-program-header-virtual-address
          elf-program-header-physical-address elf-program-header-file-size
          elf-program-header-memory-size elf-program-header-alignment elf-program-header-data
          make-elf-section elf-section? elf-section-header elf-section-content
          make-elf-section-header elf-section-header? elf-section-header-name-index
          elf-section-header-name elf-section-header-type elf-section-header-flags
          elf-section-header-address elf-section-header-offset elf-section-header-size
          elf-section-header-link elf-section-header-info elf-section-header-address-alignment
          elf-section-header-entry-size
          make-elf-raw-section elf-raw-section? elf-raw-section-bytes
          make-elf-string-table elf-string-table? elf-string-table-bytes
          make-elf-symbol-table elf-symbol-table? elf-symbol-table-symbols
          make-elf-symbol elf-symbol? elf-symbol-name-index elf-symbol-name elf-symbol-info
          elf-symbol-other elf-symbol-section-index elf-symbol-value elf-symbol-size
          make-elf-relocation-table elf-relocation-table? elf-relocation-table-relocations
          elf-relocation-table-with-addends?
          make-elf-relocation elf-relocation? elf-relocation-offset elf-relocation-info
          elf-relocation-symbol-index elf-relocation-type elf-relocation-addend
          make-elf-relr-table elf-relr-table? elf-relr-table-entries
          make-elf-hash-table elf-hash-table? elf-hash-table-buckets elf-hash-table-chains
          make-elf-group-section elf-group-section? elf-group-section-flags
          elf-group-section-members
          make-elf-word-table elf-word-table? elf-word-table-entries
          make-elf-dynamic-table elf-dynamic-table? elf-dynamic-table-entries
          make-elf-dynamic-entry elf-dynamic-entry? elf-dynamic-entry-tag elf-dynamic-entry-value
          make-elf-note-table elf-note-table? elf-note-table-notes
          make-elf-note elf-note? elf-note-name elf-note-type elf-note-descriptor)
  (import (chezpp chez))

  (define-record-type elf-file
    (fields (immutable identification) (immutable header)
            (immutable program-headers) (immutable sections)))
  (define-record-type elf-identification
    (fields (immutable class) (immutable endianness) (immutable version)
            (immutable osabi) (immutable abi-version)))
  (define-record-type elf-header
    (fields (immutable type) (immutable machine) (immutable version) (immutable entry)
            (immutable program-header-offset) (immutable section-header-offset)
            (immutable flags) (immutable header-size) (immutable program-header-entry-size)
            (immutable program-header-count) (immutable section-header-entry-size)
            (immutable section-header-count) (immutable section-name-index)))
  (define-record-type elf-program-header
    (fields (immutable type) (immutable flags) (immutable offset)
            (immutable virtual-address) (immutable physical-address)
            (immutable file-size) (immutable memory-size) (immutable alignment)
            (immutable data)))
  (define-record-type elf-section
    (fields (immutable header) (immutable content)))
  (define-record-type ($elf-section-header make-elf-section-header elf-section-header?)
    (fields (immutable name-index elf-section-header-name-index)
            (immutable name elf-section-header-name)
            (immutable type elf-section-header-type)
            (immutable flags elf-section-header-flags)
            (immutable address elf-section-header-address)
            (immutable offset elf-section-header-offset)
            (immutable size elf-section-header-size)
            (immutable link elf-section-header-link)
            (immutable info elf-section-header-info)
            (immutable address-alignment elf-section-header-address-alignment)
            (immutable entry-size elf-section-header-entry-size)))
  (define-record-type elf-raw-section (fields (immutable bytes)))
  (define-record-type elf-string-table (fields (immutable bytes)))
  (define-record-type elf-symbol-table (fields (immutable symbols)))
  (define-record-type elf-symbol
    (fields (immutable name-index) (immutable name) (immutable info) (immutable other)
            (immutable section-index) (immutable value) (immutable size)))
  (define-record-type elf-relocation-table
    (fields (immutable relocations) (immutable with-addends?)))
  (define-record-type elf-relocation
    (fields (immutable offset) (immutable info) (immutable symbol-index)
            (immutable type) (immutable addend)))
  (define-record-type elf-relr-table (fields (immutable entries)))
  (define-record-type elf-hash-table (fields (immutable buckets) (immutable chains)))
  (define-record-type elf-group-section (fields (immutable flags) (immutable members)))
  (define-record-type elf-word-table (fields (immutable entries)))
  (define-record-type elf-dynamic-table (fields (immutable entries)))
  (define-record-type elf-dynamic-entry (fields (immutable tag) (immutable value)))
  (define-record-type elf-note-table (fields (immutable notes)))
  (define-record-type elf-note
    (fields (immutable name) (immutable type) (immutable descriptor)))
  )
