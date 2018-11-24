(library (testing reports)
  (export report? make-report report-n-successes report-n-failures report-failures
          report-add-result print-report)
  (import (rnrs (6))
          (format)
          (testing private contexts)
          (testing tests))

  #| Reporting ============================================================= |#
  (define-context report
    (fields n-successes n-failures failures))

  (define (report-add-result report result)
    (with-report report
      (if (failed? result)
        (make-report
          n-successes (+ 1 n-failures) (cons result failures))
        (make-report
          (+ 1 n-successes) n-failures failures))))

  (define (print-report report)
    (with-report report
      (println "{:} successes, {:} failures" n-successes n-failures)))
)
