(library (chezpp system info)
  (export
          ;; time
          sleep
          milisleep
          nanosleep
          sleep-seconds
          sleep-milliseconds
          sleep-nanoseconds

          ;; platform predicates
          unix?
          windows?
          darwin?
          linux?
          system-platform

          ;; host information
          hostname
          system-hostname
          cpu-arch
          cpu-count
          system-machine)
  (import (chezpp system time) (chezpp system platform)))
