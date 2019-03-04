(library (monads monads)
  (export seq make-monad monad? monad-return monad-bind <- ::)

  (import (rnrs (6))
          (utility receive)
          (monads aux-keyword))

  (define-auxiliary-keywords <- ::)

  (define-record-type monad
    (fields bind return))

  (define-syntax seq
    (syntax-rules (<- ::)
      ;; the last expression in a sequence remains as is.
      ((_ <M> <f>)
       <f>)

      ;; (seq M (a <- expression) ...) expands to a nested
      ;; binding to a function that contains the rest of the
      ;; sequence
      ((_ <M>
          (<formals> ... <- <f>)
          <rest> ...)

       ((monad-bind <M>)
        <f>
        (lambda (<formals> ...)
          (seq <M> <rest> ...))))

      ;; (seq M (a :: expression) ...) expands to a nested
      ;; let binding
      ((_ <M>
          (<formals> ... :: <f>)
          <rest> ...)
       
       (call-with-values
         (lambda () <f>)
         (lambda (<formals> ...)
           (seq <M> <rest> ...))))

      ;; If the pattern doesn't match the (a <- expr) pattern,
      ;; the outcome of <f> is thrown away, but we still need
      ;; a lambda for bind to work on.
      ((_ <M>
          <f>
          <rest> ...)

       ((monad-bind <M>)
        <f>
        (lambda _
          (seq <M> <rest> ...))))))
)
