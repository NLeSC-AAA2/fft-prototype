(library (parsing xml)
  (export parse-xml xml:get-path xml:get-all-path
          xml:get-attr xml->sexpr xml:read
          xml:get xml:get-all xml:get-content

          document? document-prolog document-root
          tag? tag-name tag-attributes tag-content
          tag-prolog? tag-self-closing? tag-has-content?)

  (import (rnrs (6))
          (parsing private utility)
          (parsing private cut)
          (parsing xml data)
          (parsing xml parsers)
          (parsing full))

  #| Main parsers ========================================================== |#
  (define (parse-xml str)
    (parse-string (xml:document) str))

  (define (xml:read filename)
    (let* ((f    (open-input-file filename))
           (text (get-string-all f)))
      (close-port f)
      (parse-string (xml:document) text)))

  (define (xml:get-attr tag name)
    (cond
      ((assq name (tag-attributes tag)) => cdr)
      (else #f)))

  (define (xml:query-predicate query-name query-attrs)
    (lambda (x)
      (and (tag? x)
           (eq? (tag-name x) query-name)
           (all? (lambda (a)
                   (let ((value (xml:get-attr x (car a)))
                         (pred  (cdr a)))
                     (if (procedure? pred)
                       (pred value)
                       (equal? value pred))))
                 query-attrs))))

  (define (xml:get data query-name . query-attr)
    (find (xml:query-predicate query-name query-attr)
          (xml:get-content data)))

  (define (xml:get-content tag)
    (if (tag-has-content? tag)
      (tag-content tag)
      '()))

  ;(define xml:get-content
  ;  (compose append <<= (cut map tag-content <>) xml:get))

  (define (xml:get-all data query-name . query-attr)
    (filter (xml:query-predicate query-name query-attr)
            (xml:get-content data)))

  (define (xml:get-path data . path)
    (fold-left (lambda (data query)
                 (apply xml:get data query))
               data path))

  (define (xml:get-all-path tag . path)
    (fold-left (lambda (data query)
                 (apply append (map (cut apply xml:get-all <> query) data)))
               (list tag)
               path))

  (define (xml->sexpr x)
    (if (tag? x)
      `(,(tag-name x) ,(tag-attributes x)
        ,@(map xml->sexpr (xml:get-content x)))
      x))
)
