(library (testing private colours)
  (export set-colour-green set-colour-red set-colour-blue set-colour-dark-red
          reset-colour)
  (import (rnrs (6))
          (format))

  (define (set-colour-rgb r g b)
    (format "\x1B[38;2;{:};{:};{:}m" r g b))

  (define (set-colour-green)
    (set-colour-rgb 200 255 40))

  (define (set-colour-red)
    (set-colour-rgb 255 60 100))

  (define (set-colour-dark-red)
    (set-colour-rgb 150 40 60))

  (define (set-colour-blue)
    (set-colour-rgb 100 100 200))

  (define (reset-colour)
    "\x1B[m"))
