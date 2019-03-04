(library (monads support)
  (export seq-map)
  (import (rnrs (6))
          (monads monads))

  (define (seq-map M f . args)
    (if (null? (car args))
      ((monad-return M) '())
      (seq M
        (first <- (apply f (map car args)))
        (rest  <- (apply seq-map M f (map cdr args)))
        ((monad-return M) (cons first rest)))))
)
