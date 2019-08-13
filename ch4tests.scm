;; Load ch4-mceval.scm before running tests

;;EXERCISE 4.1
(define (run-test)
  (eval '(begin (define log '())
                (define (f x) (set! log (cons x log)) x)
                (define (g a b) 'ok)
                (g (f 5) (f 6))
                log)
        the-global-environment))

;; left-to-right version of list-of-values
(run-test)
;Value: (6 5)

;; right-to-left version of list-of-values
(run-test)
;Value: (5 6)


;;EXERCISE 4.2
(eval '(begin (define (f) 5)
              (f))
      the-global-environment)
;Error: Unknown expression type

(eval '(call f) the-global-environment)
;Value: 5
