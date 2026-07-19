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

  #|proc:make-elf-file
  The `make-elf-file` procedure creates an ELF file record.
  The `identification` parameter supplies the record's `identification` field.
  The `header` parameter supplies the record's `header` field.
  The `program-headers` parameter supplies the record's `program-headers` field.
  The `sections` parameter supplies the record's `sections` field.
  |#
  #|proc:elf-file?
  The `elf-file?` procedure returns whether `object` is an ELF file record.
  The `object` parameter is the object to test.
  |#
  #|proc:elf-file-identification
  The `elf-file-identification` procedure returns the `identification` field of `record`.
  The `record` parameter is an ELF file record.
  |#
  #|proc:elf-file-header
  The `elf-file-header` procedure returns the `header` field of `record`.
  The `record` parameter is an ELF file record.
  |#
  #|proc:elf-file-program-headers
  The `elf-file-program-headers` procedure returns the `program-headers` field of `record`.
  The `record` parameter is an ELF file record.
  |#
  #|proc:elf-file-sections
  The `elf-file-sections` procedure returns the `sections` field of `record`.
  The `record` parameter is an ELF file record.
  |#
  (define-record-type elf-file
    (fields (immutable identification) (immutable header)
            (immutable program-headers) (immutable sections)))
  #|proc:make-elf-identification
  The `make-elf-identification` procedure creates an ELF identification record.
  The `class` parameter supplies the record's `class` field.
  The `endianness` parameter supplies the record's `endianness` field.
  The `version` parameter supplies the record's `version` field.
  The `osabi` parameter supplies the record's `osabi` field.
  The `abi-version` parameter supplies the record's `abi-version` field.
  |#
  #|proc:elf-identification?
  The `elf-identification?` procedure returns whether `object` is an ELF identification record.
  The `object` parameter is the object to test.
  |#
  #|proc:elf-identification-class
  The `elf-identification-class` procedure returns the `class` field of `record`.
  The `record` parameter is an ELF identification record.
  |#
  #|proc:elf-identification-endianness
  The `elf-identification-endianness` procedure returns the `endianness` field of `record`.
  The `record` parameter is an ELF identification record.
  |#
  #|proc:elf-identification-version
  The `elf-identification-version` procedure returns the `version` field of `record`.
  The `record` parameter is an ELF identification record.
  |#
  #|proc:elf-identification-osabi
  The `elf-identification-osabi` procedure returns the `osabi` field of `record`.
  The `record` parameter is an ELF identification record.
  |#
  #|proc:elf-identification-abi-version
  The `elf-identification-abi-version` procedure returns the `abi-version` field of `record`.
  The `record` parameter is an ELF identification record.
  |#
  (define-record-type elf-identification
    (fields (immutable class) (immutable endianness) (immutable version)
            (immutable osabi) (immutable abi-version)))
  #|proc:make-elf-header
  The `make-elf-header` procedure creates an ELF header record.
  The `type` parameter supplies the record's `type` field.
  The `machine` parameter supplies the record's `machine` field.
  The `version` parameter supplies the record's `version` field.
  The `entry` parameter supplies the record's `entry` field.
  The `program-header-offset` parameter supplies the record's `program-header-offset` field.
  The `section-header-offset` parameter supplies the record's `section-header-offset` field.
  The `flags` parameter supplies the record's `flags` field.
  The `header-size` parameter supplies the record's `header-size` field.
  The `program-header-entry-size` parameter supplies the record's `program-header-entry-size` field.
  The `program-header-count` parameter supplies the record's `program-header-count` field.
  The `section-header-entry-size` parameter supplies the record's `section-header-entry-size` field.
  The `section-header-count` parameter supplies the record's `section-header-count` field.
  The `section-name-index` parameter supplies the record's `section-name-index` field.
  |#
  #|proc:elf-header?
  The `elf-header?` procedure returns whether `object` is an ELF header record.
  The `object` parameter is the object to test.
  |#
  #|proc:elf-header-type
  The `elf-header-type` procedure returns the `type` field of `record`.
  The `record` parameter is an ELF header record.
  |#
  #|proc:elf-header-machine
  The `elf-header-machine` procedure returns the `machine` field of `record`.
  The `record` parameter is an ELF header record.
  |#
  #|proc:elf-header-version
  The `elf-header-version` procedure returns the `version` field of `record`.
  The `record` parameter is an ELF header record.
  |#
  #|proc:elf-header-entry
  The `elf-header-entry` procedure returns the `entry` field of `record`.
  The `record` parameter is an ELF header record.
  |#
  #|proc:elf-header-program-header-offset
  The `elf-header-program-header-offset` procedure returns the `program-header-offset` field of `record`.
  The `record` parameter is an ELF header record.
  |#
  #|proc:elf-header-section-header-offset
  The `elf-header-section-header-offset` procedure returns the `section-header-offset` field of `record`.
  The `record` parameter is an ELF header record.
  |#
  #|proc:elf-header-flags
  The `elf-header-flags` procedure returns the `flags` field of `record`.
  The `record` parameter is an ELF header record.
  |#
  #|proc:elf-header-header-size
  The `elf-header-header-size` procedure returns the `header-size` field of `record`.
  The `record` parameter is an ELF header record.
  |#
  #|proc:elf-header-program-header-entry-size
  The `elf-header-program-header-entry-size` procedure returns the `program-header-entry-size` field of `record`.
  The `record` parameter is an ELF header record.
  |#
  #|proc:elf-header-program-header-count
  The `elf-header-program-header-count` procedure returns the `program-header-count` field of `record`.
  The `record` parameter is an ELF header record.
  |#
  #|proc:elf-header-section-header-entry-size
  The `elf-header-section-header-entry-size` procedure returns the `section-header-entry-size` field of `record`.
  The `record` parameter is an ELF header record.
  |#
  #|proc:elf-header-section-header-count
  The `elf-header-section-header-count` procedure returns the `section-header-count` field of `record`.
  The `record` parameter is an ELF header record.
  |#
  #|proc:elf-header-section-name-index
  The `elf-header-section-name-index` procedure returns the `section-name-index` field of `record`.
  The `record` parameter is an ELF header record.
  |#
  (define-record-type elf-header
    (fields (immutable type) (immutable machine) (immutable version) (immutable entry)
            (immutable program-header-offset) (immutable section-header-offset)
            (immutable flags) (immutable header-size) (immutable program-header-entry-size)
            (immutable program-header-count) (immutable section-header-entry-size)
            (immutable section-header-count) (immutable section-name-index)))
  #|proc:make-elf-program-header
  The `make-elf-program-header` procedure creates an ELF program header record.
  The `type` parameter supplies the record's `type` field.
  The `flags` parameter supplies the record's `flags` field.
  The `offset` parameter supplies the record's `offset` field.
  The `virtual-address` parameter supplies the record's `virtual-address` field.
  The `physical-address` parameter supplies the record's `physical-address` field.
  The `file-size` parameter supplies the record's `file-size` field.
  The `memory-size` parameter supplies the record's `memory-size` field.
  The `alignment` parameter supplies the record's `alignment` field.
  The `data` parameter supplies the record's `data` field.
  |#
  #|proc:elf-program-header?
  The `elf-program-header?` procedure returns whether `object` is an ELF program header record.
  The `object` parameter is the object to test.
  |#
  #|proc:elf-program-header-type
  The `elf-program-header-type` procedure returns the `type` field of `record`.
  The `record` parameter is an ELF program header record.
  |#
  #|proc:elf-program-header-flags
  The `elf-program-header-flags` procedure returns the `flags` field of `record`.
  The `record` parameter is an ELF program header record.
  |#
  #|proc:elf-program-header-offset
  The `elf-program-header-offset` procedure returns the `offset` field of `record`.
  The `record` parameter is an ELF program header record.
  |#
  #|proc:elf-program-header-virtual-address
  The `elf-program-header-virtual-address` procedure returns the `virtual-address` field of `record`.
  The `record` parameter is an ELF program header record.
  |#
  #|proc:elf-program-header-physical-address
  The `elf-program-header-physical-address` procedure returns the `physical-address` field of `record`.
  The `record` parameter is an ELF program header record.
  |#
  #|proc:elf-program-header-file-size
  The `elf-program-header-file-size` procedure returns the `file-size` field of `record`.
  The `record` parameter is an ELF program header record.
  |#
  #|proc:elf-program-header-memory-size
  The `elf-program-header-memory-size` procedure returns the `memory-size` field of `record`.
  The `record` parameter is an ELF program header record.
  |#
  #|proc:elf-program-header-alignment
  The `elf-program-header-alignment` procedure returns the `alignment` field of `record`.
  The `record` parameter is an ELF program header record.
  |#
  #|proc:elf-program-header-data
  The `elf-program-header-data` procedure returns the `data` field of `record`.
  The `record` parameter is an ELF program header record.
  |#
  (define-record-type elf-program-header
    (fields (immutable type) (immutable flags) (immutable offset)
            (immutable virtual-address) (immutable physical-address)
            (immutable file-size) (immutable memory-size) (immutable alignment)
            (immutable data)))
  #|proc:make-elf-section
  The `make-elf-section` procedure creates an ELF section record.
  The `header` parameter supplies the record's `header` field.
  The `content` parameter supplies the record's `content` field.
  |#
  #|proc:elf-section?
  The `elf-section?` procedure returns whether `object` is an ELF section record.
  The `object` parameter is the object to test.
  |#
  #|proc:elf-section-header
  The `elf-section-header` procedure returns the `header` field of `record`.
  The `record` parameter is an ELF section record.
  |#
  #|proc:elf-section-content
  The `elf-section-content` procedure returns the `content` field of `record`.
  The `record` parameter is an ELF section record.
  |#
  (define-record-type elf-section
    (fields (immutable header) (immutable content)))
  #|proc:make-elf-section-header
  The `make-elf-section-header` procedure creates an ELF section header record.
  The `name-index` parameter supplies the record's `name-index` field.
  The `name` parameter supplies the record's `name` field.
  The `type` parameter supplies the record's `type` field.
  The `flags` parameter supplies the record's `flags` field.
  The `address` parameter supplies the record's `address` field.
  The `offset` parameter supplies the record's `offset` field.
  The `size` parameter supplies the record's `size` field.
  The `link` parameter supplies the record's `link` field.
  The `info` parameter supplies the record's `info` field.
  The `address-alignment` parameter supplies the record's `address-alignment` field.
  The `entry-size` parameter supplies the record's `entry-size` field.
  |#
  #|proc:elf-section-header?
  The `elf-section-header?` procedure returns whether `object` is an ELF section header record.
  The `object` parameter is the object to test.
  |#
  #|proc:elf-section-header-name-index
  The `elf-section-header-name-index` procedure returns the `name-index` field of `record`.
  The `record` parameter is an ELF section header record.
  |#
  #|proc:elf-section-header-name
  The `elf-section-header-name` procedure returns the `name` field of `record`.
  The `record` parameter is an ELF section header record.
  |#
  #|proc:elf-section-header-type
  The `elf-section-header-type` procedure returns the `type` field of `record`.
  The `record` parameter is an ELF section header record.
  |#
  #|proc:elf-section-header-flags
  The `elf-section-header-flags` procedure returns the `flags` field of `record`.
  The `record` parameter is an ELF section header record.
  |#
  #|proc:elf-section-header-address
  The `elf-section-header-address` procedure returns the `address` field of `record`.
  The `record` parameter is an ELF section header record.
  |#
  #|proc:elf-section-header-offset
  The `elf-section-header-offset` procedure returns the `offset` field of `record`.
  The `record` parameter is an ELF section header record.
  |#
  #|proc:elf-section-header-size
  The `elf-section-header-size` procedure returns the `size` field of `record`.
  The `record` parameter is an ELF section header record.
  |#
  #|proc:elf-section-header-link
  The `elf-section-header-link` procedure returns the `link` field of `record`.
  The `record` parameter is an ELF section header record.
  |#
  #|proc:elf-section-header-info
  The `elf-section-header-info` procedure returns the `info` field of `record`.
  The `record` parameter is an ELF section header record.
  |#
  #|proc:elf-section-header-address-alignment
  The `elf-section-header-address-alignment` procedure returns the `address-alignment` field of `record`.
  The `record` parameter is an ELF section header record.
  |#
  #|proc:elf-section-header-entry-size
  The `elf-section-header-entry-size` procedure returns the `entry-size` field of `record`.
  The `record` parameter is an ELF section header record.
  |#
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
  #|proc:make-elf-symbol
  The `make-elf-symbol` procedure creates an ELF symbol record.
  The `name-index` parameter supplies the record's `name-index` field.
  The `name` parameter supplies the record's `name` field.
  The `info` parameter supplies the record's `info` field.
  The `other` parameter supplies the record's `other` field.
  The `section-index` parameter supplies the record's `section-index` field.
  The `value` parameter supplies the record's `value` field.
  The `size` parameter supplies the record's `size` field.
  |#
  #|proc:elf-symbol?
  The `elf-symbol?` procedure returns whether `object` is an ELF symbol record.
  The `object` parameter is the object to test.
  |#
  #|proc:elf-symbol-name-index
  The `elf-symbol-name-index` procedure returns the `name-index` field of `record`.
  The `record` parameter is an ELF symbol record.
  |#
  #|proc:elf-symbol-name
  The `elf-symbol-name` procedure returns the `name` field of `record`.
  The `record` parameter is an ELF symbol record.
  |#
  #|proc:elf-symbol-info
  The `elf-symbol-info` procedure returns the `info` field of `record`.
  The `record` parameter is an ELF symbol record.
  |#
  #|proc:elf-symbol-other
  The `elf-symbol-other` procedure returns the `other` field of `record`.
  The `record` parameter is an ELF symbol record.
  |#
  #|proc:elf-symbol-section-index
  The `elf-symbol-section-index` procedure returns the `section-index` field of `record`.
  The `record` parameter is an ELF symbol record.
  |#
  #|proc:elf-symbol-value
  The `elf-symbol-value` procedure returns the `value` field of `record`.
  The `record` parameter is an ELF symbol record.
  |#
  #|proc:elf-symbol-size
  The `elf-symbol-size` procedure returns the `size` field of `record`.
  The `record` parameter is an ELF symbol record.
  |#
  (define-record-type elf-symbol
    (fields (immutable name-index) (immutable name) (immutable info) (immutable other)
            (immutable section-index) (immutable value) (immutable size)))
  #|proc:make-elf-relocation-table
  The `make-elf-relocation-table` procedure creates an ELF relocation table record.
  The `relocations` parameter supplies the record's `relocations` field.
  The `with-addends?` parameter supplies the record's `with-addends?` field.
  |#
  #|proc:elf-relocation-table?
  The `elf-relocation-table?` procedure returns whether `object` is an ELF relocation table record.
  The `object` parameter is the object to test.
  |#
  #|proc:elf-relocation-table-relocations
  The `elf-relocation-table-relocations` procedure returns the `relocations` field of `record`.
  The `record` parameter is an ELF relocation table record.
  |#
  #|proc:elf-relocation-table-with-addends?
  The `elf-relocation-table-with-addends?` procedure returns the `with-addends?` field of `record`.
  The `record` parameter is an ELF relocation table record.
  |#
  (define-record-type elf-relocation-table
    (fields (immutable relocations) (immutable with-addends?)))
  #|proc:make-elf-relocation
  The `make-elf-relocation` procedure creates an ELF relocation record.
  The `offset` parameter supplies the record's `offset` field.
  The `info` parameter supplies the record's `info` field.
  The `symbol-index` parameter supplies the record's `symbol-index` field.
  The `type` parameter supplies the record's `type` field.
  The `addend` parameter supplies the record's `addend` field.
  |#
  #|proc:elf-relocation?
  The `elf-relocation?` procedure returns whether `object` is an ELF relocation record.
  The `object` parameter is the object to test.
  |#
  #|proc:elf-relocation-offset
  The `elf-relocation-offset` procedure returns the `offset` field of `record`.
  The `record` parameter is an ELF relocation record.
  |#
  #|proc:elf-relocation-info
  The `elf-relocation-info` procedure returns the `info` field of `record`.
  The `record` parameter is an ELF relocation record.
  |#
  #|proc:elf-relocation-symbol-index
  The `elf-relocation-symbol-index` procedure returns the `symbol-index` field of `record`.
  The `record` parameter is an ELF relocation record.
  |#
  #|proc:elf-relocation-type
  The `elf-relocation-type` procedure returns the `type` field of `record`.
  The `record` parameter is an ELF relocation record.
  |#
  #|proc:elf-relocation-addend
  The `elf-relocation-addend` procedure returns the `addend` field of `record`.
  The `record` parameter is an ELF relocation record.
  |#
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
  #|proc:make-elf-note
  The `make-elf-note` procedure creates an ELF note record.
  The `name` parameter supplies the record's `name` field.
  The `type` parameter supplies the record's `type` field.
  The `descriptor` parameter supplies the record's `descriptor` field.
  |#
  #|proc:elf-note?
  The `elf-note?` procedure returns whether `object` is an ELF note record.
  The `object` parameter is the object to test.
  |#
  #|proc:elf-note-name
  The `elf-note-name` procedure returns the `name` field of `record`.
  The `record` parameter is an ELF note record.
  |#
  #|proc:elf-note-type
  The `elf-note-type` procedure returns the `type` field of `record`.
  The `record` parameter is an ELF note record.
  |#
  #|proc:elf-note-descriptor
  The `elf-note-descriptor` procedure returns the `descriptor` field of `record`.
  The `record` parameter is an ELF note record.
  |#
  (define-record-type elf-note
    (fields (immutable name) (immutable type) (immutable descriptor)))
  )
