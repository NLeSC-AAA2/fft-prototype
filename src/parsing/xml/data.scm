(library (parsing xml data)
  (export make-document document? document-prolog document-root
          make-tag tag? tag-name tag-attributes tag-content
          tag-prolog? tag-self-closing? tag-update-content tag-has-content?
          *self-closing* *prolog*)

  (import (rnrs (6)))

  #| Data structures ======================================================= |#
  (define-record-type document
    (fields prolog root))

  (define-record-type self-closing)
  (define *self-closing* (make-self-closing))

  (define-record-type prolog)
  (define *prolog* (make-prolog))

  (define-record-type tag
    (fields name attributes content))

  (define (tag-update-content tag content)
    (let ((name (tag-name tag))
          (attributes (tag-attributes tag)))
      (make-tag name attributes content)))

  (define (tag-self-closing? t)
    (self-closing? (tag-content t)))

  (define (tag-prolog? t)
    (prolog? (tag-content t)))

  (define (tag-has-content? t)
    (list? (tag-content t)))
)
