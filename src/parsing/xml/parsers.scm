(library (parsing xml parsers)
  (export xml:document xml:root xml:content xml:text xml:comment
          xml:element xml:entity xml:prolog xml:tag)

  (import (rnrs (6))
          (monads)
          (parsing full)
          (parsing predicates)
          (parsing xml data))

  #| XML parsers =========================================================== |#
  (define (xml:document)
    (seq-parsing
      (pro <- (optional (xml:prolog)))
      (root <- (xml:root))
      (return (make-document pro root))))

  (define (xml:root)
    (tokenize (xml:element)))

  (define (xml:content)
    (many (choice (ignore (tokenize (xml:comment)))
                  (tokenize (xml:element))
                  (xml:text (char!= #\< #\> #\&))
                  (xml:entity))))

  (define xml:text
    (case-lambda
      (()  (xml:text (char!= #\< #\> #\&)))
      ((p) (some
             (choice (some-char p)
                     (xml:entity))
             (lambda (lst)
               (apply string-append (reverse lst)))))))

  (define (xml:comment)
    (seq-parsing
      (flush)
      (literal "<!--")
      (many-end-with* item "-->")
      (x <- (flush))
      (return `(comment ,x))))

  (define (xml:element)
    (seq-parsing
      (tag <- (xml:tag))
      (if (tag-self-closing? tag)
        (return tag)
        (seq-parsing
          (content <- (xml:content))
          (xml:closing-tag tag)
          (return (tag-update-content tag content))))))

  (define (xml:tag)
    (seq-parsing
      (literal "<")
      (tag-name <- word)
      space?
      (attributes <- (sep-by (xml:attribute) space))
      space?
      (content <- (optional (seq-parsing
                              (literal "/")
                              (return *self-closing*))))
      (literal ">")
      (return (make-tag (string->symbol tag-name) attributes content))))

  (define (xml:closing-tag tag)
    (seq-parsing
      (literal "</")
      (literal (symbol->string (tag-name tag)))
      (literal ">")))

  (define (xml:attribute)
    (seq-parsing
      (attribute <- word)
      (literal "=")
      (value <- (xml:string))
      (return (cons (string->symbol attribute) value))))

  (define (xml:string)
    (choice (simple-string #\')
            (simple-string #\")))

  (define (simple-string quote-char)
    (seq-parsing
      (char= quote-char)
      (text <- (xml:text (char!= #\& quote-char)))
      (char= quote-char)
      (return text)))

  (define (xml:prolog)
    (seq-parsing
      (literal "<?")
      (tag-name <- word)
      space
      (attributes <- (sep-by (xml:attribute) space))
      space?
      (literal "?>")
      (return (make-tag (string->symbol tag-name) attributes *prolog*))))

  (define (xml:entity)
    (define xml-entity-alist
      '(("lt"   . "<")
        ("gt"   . ">")
        ("amp"  . "&")
        ("quot" . "\"")
        ("apos" . "'")))

    (seq-parsing
      (char= #\&)
      (x <- word)
      (char= #\;)
      (cond
        ((assoc x xml-entity-alist)
         => (lambda (y) (return (cdr y))))
        (else (fail)))))
)
