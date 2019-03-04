(library (text-cursors)
  (export text-cursor?

	  ;; accessors
	  text-cursor-string text-cursor-start text-cursor-end

	  ;; creation
	  make-text-cursor string->text-cursor

	  ;; querying
	  text-cursor-ref text-cursor-select text-cursor-peek
	  text-cursor-null?

	  ;; manipulation
	  text-cursor-next text-cursor-flush text-cursor-forward)

  (import (rnrs (6)))

  (define-record-type text-cursor
    (fields string start end))

  (define (text-cursor-null? tc)
    (>= (text-cursor-end tc)
	(string-length (text-cursor-string tc))))

  (define (string->text-cursor text)
    (make-text-cursor text 0 0))

  (define (text-cursor-ref tc)
    (string-ref (text-cursor-string tc)
		(text-cursor-end tc)))

  (define (text-cursor-select tc)
    (substring (text-cursor-string tc)
	       (text-cursor-start tc)
	       (text-cursor-end tc)))

  (define (text-cursor-peek tc n)
    (text-cursor-select (text-cursor-forward tc n)))

  (define (text-cursor-next tc)
    (make-text-cursor
     (text-cursor-string tc)
     (text-cursor-start tc)
     (+ 1 (text-cursor-end tc))))

  (define (text-cursor-forward tc n)
    (make-text-cursor
     (text-cursor-string tc)
     (text-cursor-start tc)
     (min (string-length (text-cursor-string tc))
	  (+ n (text-cursor-end tc)))))

  (define (text-cursor-flush tc)
    (make-text-cursor
     (text-cursor-string tc)
     (text-cursor-end tc)
     (text-cursor-end tc)))
)
