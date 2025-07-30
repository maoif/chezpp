(library (chezpp io)
  (export println displayln)
  (import (chezpp chez)
          (chezpp internal))



  #|doc
  Display a string either to the current output port
  or a given textual output port, with a newline.
  |#
  (define displayln
    (case-lambda
      [(obj) (display obj) (display "\n")]
      [(obj port) (display obj port) (display "\n" port)]))


  #|doc
  The same as `displayln`.
  |#
  (define println displayln)

  #|doc
  Print a formated string either to the current output port
  or a given textual output port, with a newline.
  |#
  (define printfln
    (case-lambda
      [(fmt . objs) (todo)]
      [(fmt . objs+port) (todo)]))

  #|doc
  Write a formated string either to the current output port
  or a given textual output port, with a newline.
  |#
  (define writefln
    (case-lambda
      [(fmt . objs) (todo)]
      [(fmt . objs+port) (todo)]
      ))

  #|doc
  Display a formated string either to the current output port
  or a given textual output port, with a newline.
  |#
  (define displayfln
    (case-lambda
      [(fmt . objs) (todo)]
      [(fmt . objs+port)
       (todo)]
      ))


  )
