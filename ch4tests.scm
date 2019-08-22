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


;;EXERCISE 4.11
(define my-frame (make-frame '((b 2) (c 3))))
(equal? my-frame (make-frame (frame-bindings my-frame)))
;Value: #t

(add-binding-to-frame! 'a 1 my-frame)
(equal? my-frame (make-frame '((a 1) (b 2) (c 3))))
;Value: #t


;;EXERCISE 4.12
(define outer-env (extend-environment '(x y) '(6 7) the-global-environment))
(define inner-env (extend-environment '(y z) '(8 9) outer-env))

(lookup-variable-value 'x inner-env)
;Value: 6

(lookup-variable-value 'y inner-env)
;Value: 8

(define-variable! 'w 5 inner-env)
(lookup-variable-value 'w inner-env)
;Value: 5

(lookup-variable-value 'w outer-env)
;Error: Unbound variable w

(define-variable! 'w 55 inner-env)
(lookup-variable-value 'w inner-env)
;Value: 55

(set-variable-value! 'x 66 outer-env)
(lookup-variable-value 'x inner-env)
;Value: 66

(set-variable-value! 'y false inner-env)
(lookup-variable-value 'y inner-env)
;Value: #f

(lookup-variable-value 'y outer-env)
;Value: 7

(set-variable-value! 'v 4 inner-env)
;Error: Unbound variable v


;;EXERCISE 4.13
(define outer-env (extend-environment '(x y) '(6 7) the-global-environment))
(define inner-env (extend-environment '(y z) '(8 9) outer-env))

(make-unbound! 'x inner-env)
(lookup-variable-value 'x inner-env)
;Value: 6

(make-unbound! 'x outer-env)
(lookup-variable-value 'x inner-env)
;Error: Unbound variable x


;;EXERCISE 4.16
;; a. first repeat tests from exercise 12
(define-variable! 'u '*unassigned* the-global-environment)
(lookup-variable-value 'u the-global-environment)
;Error: Attempt to use uninitialized variable u

;; b. example from the text, section 4.1.6
(define before
  '((define u e1)
    (define v e2)
    e3))

(define after
  '((let ((u '*unassigned*)
          (v '*unassigned*))
     (set! u e1)
     (set! v e2)
     e3)))

(equal? (scan-out-defines before) after)
;Value: #t

;; c.
(eval '((lambda ()
          (define a b)
          (define b 5)
          (+ a b)))
      the-global-environment)
;Error: Attempt to use uninitialized variable b

(eval '(begin
         (define (f x)
           (define (g) y)
           (define y x)
           (g))
         (f 5))
      the-global-environment)
;Value: 5


;; EXERCISE 4.17
(define before
  '((define u e1)
    (define v e2)
    e3))

(define after
  '((define u '*unassigned*)
    (define v '*unassigned*)
    (set! u e1)
    (set! v e2)
    e3))

(equal? (scan-out-defines before) after)
;Value: #t

;; Repeat test case for exercise 16 part c


;; EXERCISE 4.19
(define before
  '((define u e1)
    (define v e2)
    e3))

(define after
  '((define u e1)
    (define v e2)
    u
    v
    e3))

(equal? (scan-out-defines before) after)
;Value: #t

(eval
  '(let ((a 1))
    (define (f x)
      (define b (+ a x))
      (define a 5)
      (+ a b))
    (f 10))
  the-global-environment)
;Value: 20

;; test to ensure that value expressions are evaluated in the correct environment
(eval
  '((lambda ()
      (define a
        ((lambda ()
           (define b 10)
           (+ c 2))))
      (define b 6)
      (define c (+ b 1))
      (+ a b)))
  the-global-environment)
;Value: 15

;; test to ensure infinite recursion is avoided
(eval
  '((lambda ()
      (define a b)
      (define b a)))
  the-global-environment)
;Error: Invalid recursive reference a


;; EXERCISE 4.20
;; eval the fact example from the exercise prompt
;Value: 3628800

(eval
  '((lambda ()
      (define (f x)
        (letrec ((even?
                  (lambda (n)
                    (if (= n 0)
                        true
                        (odd? (- n 1)))))
                 (odd?
                  (lambda (n)
                    (if (= n 0)
                        false
                        (even? (- n 1))))))
          (even? x)))
      (f 5)))
  the-global-environment)
;Value: #f
