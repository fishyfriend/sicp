;;;SECTION 4.1.1-4.1.2
;; Before each exercise, restart scheme and load ch4-mceval.scm including the
;; commented-out initialization of the-global-environment.


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


;;EXERCISE 4.3
(eval '(begin (define msg 'hey)
              (define fun
                (lambda (arg)
                  (cond (arg (set! msg false) 'sup)
                        (else (if msg msg 'eyo))))))
      the-global-environment)

(eval '(fun false) the-global-environment)
;Value: hey

(eval '(fun true) the-global-environment)
;Value: sup

(eval '(fun false) the-global-environment)
;Value: eyo


;;EXERCISE 4.4
(eval '(and true false 5) the-global-environment)
;Value: #f

(eval '(and true 5) the-global-environment)
;Value: 5

(eval '(or false 4 5) the-global-environment)
;Value: 4

(eval '(or false false) the-global-environment)
;Value: #f


;;EXERCISE 4.5
(eval '(define (f x)
         (cond ((and x (cdr x)) => car)
               (x true)
               (else x)))
      the-global-environment)

(eval '(f '(1 2 3)) the-global-environment)
;Value: 2

(eval '(f (cons 4 false)) the-global-environment)
;Value: #t

(eval '(f false) the-global-environment)
;Value: #f


;;EXERCISE 4.6
(eval '(let ((x 5) (y 6)) (cons x y)) the-global-environment)
;Value: (5 . 6)

(eval '(let () 5) the-global-environment)
;Value: 5


;;EXERCISE 4.7
(eval '(let* ((x 5) (y 6) (xy (cons x y))) (car xy)) the-global-environment)
;Value: 5


;;EXERCISE 4.8, EXERCISE 4.10
;; first eval the fib definition from exercise prompt
(eval '(fib 6) the-global-environment)
;Value: 8


;;;SECTION 4.1.3


;;EXERCISE 4.11
(define my-frame (make-frame '((b 2) (c 3))))
(equal? my-frame (make-frame (frame-bindings my-frame)))
;Value: #t

(add-binding-to-frame! 'a 1 my-frame)
(equal? my-frame (make-frame '((a 1) (b 2) (c 3))))
;Value: #t
