(import (rnrs (6))
        (testing assertions)
        (pmatch))

(define (test-pmatch)
  (assert-equal
   (pmatch '(1 2 3)
     ((,a . ,b) b))
   '(2 3)))
