(library (monads state)
  (export state-bind state-return <state>
          get-state set-state update-state)

  (import (rnrs (6))
          (receive)
          (monads monads))

  (define (state-bind m f)
    (lambda (state)
      (receive (value new-state) (m state)
        ((f value) new-state))))

  (define (state-return value)
    (lambda (state)
      (values value state)))

  (define <state> (make-monad state-bind state-return))

  (define get-state
    (case-lambda
      (()  (lambda (state)
             (values state state)))
      ((f) (lambda (state)
             (values (f state) state)))))

  (define (set-state value)
    (lambda (state)
      (values value value)))

  (define (update-state f)
    (lambda (state)
      (values '() (f state))))
)
