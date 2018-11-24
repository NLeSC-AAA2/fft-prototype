(library (parsing latex)
  (export latex-expression latex-identifyer latex-comment
          latex-escaped-char latex-text latex-inline-equation
          latex-command enclosed)

  (import (rnrs (6))
          (ice-9 format)
          (parsing parsing))

  (define latex-identifyer
    (seq (chars <- (some (sattisfies (lambda (c)
                     (or (char-alphabetic? c)
                         (memq c '(#\* #\' #\" #\^ #\`)))))))
         (return (list->string chars))))

  (define (enclosed open-char parser close-char)
    (seq (is-char open-char)
         (v <- parser)
         (is-char close-char)
         (return v)))

  (define (math-env? s)
    (or (string=? s "equation")
        (string=? s "align")
        (string=? s "eqnarray")))

  (define latex-environment
    (seq (is-string "\\begin{")
         (v <- latex-identifyer)
         (is-string "}")
         (many (enclosed #\[ (many latex-expression) #\]))
         (w <- (if (math-env? v)
                 (seq
                   (x <- (ends-with (format #f "\\end{~a}" v) item))
                   (return (list (list->string x))))
                 (ends-with (format #f "\\end{~a}" v)
                            latex-expression)))
         (return (cons* 'environment v w))))

  (define latex-comment
    (seq (is-char #\%)
         (chars <- (many (char-not-in #\newline)))
         (return (list 'comment (list->string chars)))))

  (define latex-escaped-char
    (seq (is-char #\\)
         (c <- (char-in #\{ #\} #\\ #\$ #\% #\space))
         (return (list 'special-char c))))

  (define latex-text
    (seq (chars <- (some (char-not-in #\{ #\} #\\ #\$ #\% #\] #\[ #\newline)))
         (return (list->string chars))))

  (define latex-newline
    (seq (is-char #\newline)
         (return 'newline)))

  (define latex-paragraph
    (seq (is-char #\newline)
         (some (is-char #\newline))
         (return 'paragraph)))

  (define latex-block
    (seq (is-char #\{)
         (v <- (many latex-expression))
         (is-char #\})
         (return v)))

  (define latex-bracketed
    (seq (is-char #\[)
         (v <- (many latex-expression))
         (is-char #\])
         (return v)))

  (define inline-math-expression
    (seq (first <- (many (char-not-in #\$)))
         (rest  <- (many (seq (a <- (is-string "\\$"))
                              (b <- (many (char-not-in #\$)))
                              (return (string-append a (list->string b))))))
         (return (apply string-append (list->string first) rest))))

  (define math-block-expression
    (seq (first <- (many (char-not-in #\\)))
         (rest  <- (many (seq (a <- (is-char #\\))
                              (b <- (char-not-in #\]))
                              (c <- (many (char-not-in #\\)))
                              (return (list->string (cons a (cons b c)))))))
         (return (apply string-append (list->string first) rest))))

  (define latex-math-block
    (seq (is-string "\\[")
         (equation <- math-block-expression)
         (is-string "\\]")
         (return (list 'equation equation))))

  (define latex-inline-math
    (seq (is-char #\$)
         (equation <- inline-math-expression)
         (is-char #\$)
         (return (list 'inline-math equation))))

  (define latex-command
    (seq (is-char #\\)
         (name   <- latex-identifyer)
         (args   <- (many
                      (choice (seq (x <- (enclosed #\[ (many latex-expression) #\]))
                                   (return (cons 'option x)))
                              (seq (x <- (enclosed #\{ (many latex-expression) #\}))
                                   (return (cons 'argument x))))))
         (return (cons* 'command name args))))

  (define latex-expression
    (choice (span latex-paragraph) latex-newline
            latex-comment latex-text latex-block latex-bracketed
            latex-escaped-char latex-inline-math latex-math-block
            (span latex-environment) (span latex-command)))
)

