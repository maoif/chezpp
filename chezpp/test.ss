#!chezscheme
(library (chezpp test)
  (export)
  (import (chezpp chez)
          (chezpp test private common)
          (chezpp test descriptor))

  (export (import (chezpp test private common)
                  (chezpp test descriptor))))
