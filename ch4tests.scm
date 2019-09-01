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


;; EXERCISE 4.22
(eval '(let ((x 5)) (+ x 1)) the-global-environment)
;Value: 6


;; EXERCISE 4.29
(define count 0)
(define actual-value-0 actual-value)
(define (actual-value exp env)
  (set! count (+ count 1))
  (actual-value-0 exp env))

(define program (quote ((lambda ()
  ;; paste in the program from the answer
))))

;; Memoizing evaluator
;; Use "memoizing version of force-it" from ch4-leval.scm
(actual-value program the-global-environment)
count
; Value: 1311

;; Non-memoizing evaluator
;; Use "non-memoizing version of force-it" from ch4-leval.scm
(set! count 0)
(actual-value program the-global-environment)
count
; Value: 41311


;; EXERCISE 4.31
;; Evaluate in lazy evaluator
(define (f a (b lazy) (c lazy-memo))
  (+ a b b c c))

(define a-count 0)
(define b-count 0)
(define c-count 0)

(f (begin (set! a-count (+ a-count 1))
          (+ 1 2))
   (begin (set! b-count (+ b-count 1))
          (+ 3 4))
   (begin (set! c-count (+ c-count 1))
          (+ 5 6)))
;Value: 39

a-count
;Value: 1

b-count
;Value: 2

c-count
;Value: 1


;; EXERCISE 4.32
;; Examples 1-2 -- Stream version -- evaluate in fresh Scheme session
;; Requires first-n-of-series from ch3support.scm
(define count 0)

(define (test-stream n)
  (if (= n 0)
      the-empty-stream
      (cons-stream (begin (set! count (+ count 1)) n)
                   (test-stream (- n 1)))))

(last-item (test-stream 10))
;Value: 1

count
;Value: 10

(set! count 0)

(define s (reverse (test-stream 10)))

count
;Value: 10

(first-n-of-series s 3)
;Value: '(1 2 3)

;; Examples 1-2 -- "Lazier" list version -- evaluate in lazy evaluator
(define count 0)

(define (test-list n)
  (if (= n 0)
      '()
      (cons (begin (set! count (+ count 1)) n)
            (test-list (- n 1)))))

(last-item (test-list 10))
;Value: 1

count
;Value: 1

(set! count 0)

(define s (reverse (test-list 10)))

count
;Value: 0

(list-ref s 0)
;Value: 0

(list-ref s 1)
;Value: 1

(list-ref s 2)
;Value: 2

;; Example 3 -- Stream version
(define count 0)

(first-n-of-series (balance-with-interest 9500.) 3)
;Value: (9500. 19133. 38763.458)

;; Example 3 -- "Lazier" list version
(define b (balance-with-interest 9500.))

(list-ref b 0)
;Value: 9500.

(list-ref b 1)
;Value: 19133.

(list-ref b 2)
;Value: 38763.458


;; EXERCISE 4.34
;; Evaluate in lazy evaluator

(cons 5 '())
;Value: (lazy 5)

(cons 5 6)
;Value: (lazy 5 . 6)

(cons 5 (cons 6 7))
;Value: (lazy 5 6 . 7)

(cons 5 (cons 6 (cons 7 '())))
;Value: (lazy 5 6 7)

(cons 5 (cons 6 (cons 7 8)))
;Value: (lazy 5 6 7 ...)

(cons 5 (cons 6 (cons 7 (cons 8 9))))
;Value: (lazy 5 6 7 ...)

(cons 5 (cons 6 (cons 7 (cons 8 '()))))
;Value: (lazy 5 6 7 ...)

(cons (cons 5 (cons 6 '())) (cons (cons 7 (cons 8 '())) (cons 9 '())))
;Value: (lazy (lazy 5 6) (lazy 7 8) 9)


;; EXERCISE 4.35
;; Evaluate in amb evaluator

(an-integer-between 6 4)
;Value: 4

try-again
;Value: 5

try-again
;Value: 6

try-again
;No more values

(a-pythagorean-triple-between 10 30)
;Value: (10 24 26)

try-again
;Value: (12 16 20)


;; EXERCISE 4.36
;; Evaluate in amb evaluator

(a-pythagorean-triple-starting-from 1)
;Value: (3 4 5)

try-again
;Value: (6 8 10)

try-again
;Value: (5 12 13)


;; EXERCISE 4.39
(define (time exp reps)
  (let iter ((start (runtime))
             (rep 0))
    (if (= rep reps)
        (/ (- (runtime) start)
           reps)
        (ambeval exp
                 the-global-environment
                 (lambda (val next) (iter start (+ rep 1)))
                 (lambda () (error "unexpected failure"))))))

(time '(multiple-dwelling) 100)
;Value: .6576

(time '(multiple-dwelling-reordered) 100)
;Value: .5737


;; EXERCISE 4.40
;; Uses time routine from exercise 39
(time '(multiple-dwelling-fast) 100)
;Value: .105


;; EXERCISE 4.48
;; Evaluate in amb evaluator

(parse '(the always best student with the cat studies with the professor now))
;(sentence
; (simple-noun-phrase
;  (article the)
;  (noun-with-adjective-prefix
;   (adjective-phrase-with-adverbial-prefix (adverb always)
;                                           (adjective best))
;   (noun student)))
; (verb-phrase-with-adverbial-suffix
;  (verb-phrase-with-adverbial-suffix
;   (verb-phrase-with-adverbial-prefix
;    (prep-phrase (prep with)
;                 (simple-noun-phrase (article the) (noun cat)))
;    (verb studies))
;   (prep-phrase
;    (prep with)
;    (simple-noun-phrase (article the) (noun professor))))
;  (adverb now)))

try-again
;(sentence
; (simple-noun-phrase
;  (article the)
;  (noun-with-adjective-prefix
;   (adjective-phrase-with-adverbial-prefix (adverb always)
;                                           (adjective best))
;   (noun student)))
; (verb-phrase-with-adverbial-suffix
;  (verb-phrase-with-adverbial-prefix
;   (prep-phrase (prep with)
;                (simple-noun-phrase (article the) (noun cat)))
;   (verb-phrase-with-adverbial-suffix
;    (verb studies)
;    (prep-phrase
;     (prep with)
;     (simple-noun-phrase (article the) (noun professor)))))
;  (adverb now)))

try-again
;(sentence
; (simple-noun-phrase
;  (article the)
;  (noun-with-adjective-prefix
;   (adjective-phrase-with-adverbial-prefix (adverb always)
;                                           (adjective best))
;   (noun student)))
; (verb-phrase-with-adverbial-prefix
;  (prep-phrase (prep with)
;               (simple-noun-phrase (article the) (noun cat)))
;  (verb-phrase-with-adverbial-suffix
;   (verb-phrase-with-adverbial-suffix
;    (verb studies)
;    (prep-phrase
;     (prep with)
;     (simple-noun-phrase (article the) (noun professor))))
;   (adverb now))))

try-again
;(sentence
; (noun-phrase
;  (simple-noun-phrase
;   (article the)
;   (noun-with-adjective-prefix
;    (adjective-phrase-with-adverbial-prefix (adverb always)
;                                            (adjective best))
;    (noun student)))
;  (prep-phrase (prep with)
;               (simple-noun-phrase (article the) (noun cat))))
; (verb-phrase-with-adverbial-suffix
;  (verb-phrase-with-adverbial-suffix
;   (verb studies)
;   (prep-phrase
;    (prep with)
;    (simple-noun-phrase (article the) (noun professor))))
;  (adverb now)))

try-again
;No more values

(parse '(the cat eats and the professor sleeps but the student studies))
;(compound-sentence
; (compound-sentence
;  (simple-sentence
;   (simple-noun-phrase (article the) (noun cat))
;   (verb eats))
;  (conjunction and)
;  (simple-sentence
;   (simple-noun-phrase (article the) (noun professor))
;   (verb sleeps)))
; (conjunction but)
; (simple-sentence
;  (simple-noun-phrase (article the) (noun student))
;  (verb studies)))

try-again
;(compound-sentence
; (simple-sentence (simple-noun-phrase (article the) (noun cat))
;                  (verb eats))
; (conjunction and)
; (compound-sentence
;  (simple-sentence
;   (simple-noun-phrase (article the) (noun professor))
;   (verb sleeps))
;  (conjunction but)
;  (simple-sentence
;   (simple-noun-phrase (article the) (noun student))
;   (verb studies))))


;; EXERCISE 4.55
;; a.
;;; Query results:
(supervisor (tweakit lem e) (bitdiddle ben))
(supervisor (fect cy d) (bitdiddle ben))
(supervisor (hacker alyssa p) (bitdiddle ben))

;; b.
;;; Query results:
(job (cratchet robert) (accounting scrivener))
(job (scrooge eben) (accounting chief accountant))

;; c.
;;; Query results:
(address (aull dewitt) (slumerville (onion square) 5))
(address (reasoner louis) (slumerville (pine tree road) 80))
(address (bitdiddle ben) (slumerville (ridge road) 10))


;; EXERCISE 4.56
;; a.
;;; Query results:
(and (supervisor (tweakit lem e) (bitdiddle ben))
     (address (tweakit lem e) (boston (bay state road) 22)))
(and (supervisor (fect cy d) (bitdiddle ben))
     (address (fect cy d) (cambridge (ames street) 3)))
(and (supervisor (hacker alyssa p) (bitdiddle ben))
     (address (hacker alyssa p) (cambridge (mass ave) 78)))

;; b.
;;; Query results:
(and (salary (aull dewitt) 25000)
     (salary (bitdiddle ben) 60000)
     (lisp-value < 25000 60000))
(and (salary (cratchet robert) 18000)
     (salary (bitdiddle ben) 60000)
     (lisp-value < 18000 60000))
(and (salary (reasoner louis) 30000)
     (salary (bitdiddle ben) 60000)
     (lisp-value < 30000 60000))
(and (salary (tweakit lem e) 25000)
     (salary (bitdiddle ben) 60000)
     (lisp-value < 25000 60000))
(and (salary (fect cy d) 35000)
     (salary (bitdiddle ben) 60000)
     (lisp-value < 35000 60000))
(and (salary (hacker alyssa p) 40000)
     (salary (bitdiddle ben) 60000)
     (lisp-value < 40000 60000))

;; c.
;;; Query results:
(and (supervisor (aull dewitt) (warbucks oliver))
     (not (job (warbucks oliver) (computer . ?type)))
     (job (warbucks oliver) (administration big wheel)))
(and (supervisor (cratchet robert) (scrooge eben))
     (not (job (scrooge eben) (computer . ?type)))
     (job (scrooge eben) (accounting chief accountant)))
(and (supervisor (scrooge eben) (warbucks oliver))
    (not (job (warbucks oliver) (computer . ?type)))
    (job (warbucks oliver) (administration big wheel)))
(and (supervisor (bitdiddle ben) (warbucks oliver))
     (not (job (warbucks oliver) (computer . ?type)))
     (job (warbucks oliver) (administration big wheel)))


;; EXERCISE 4.57
;; a.
;;; Query results:
(can-replace (bitdiddle ben) (fect cy d))
(can-replace (hacker alyssa p) (fect cy d))

;; b.
;;; Query results:
(and (can-replace (aull dewitt) (warbucks oliver))
     (salary (aull dewitt) 25000)
     (salary (warbucks oliver) 150000)
     (lisp-value < 25000 150000))
(and (can-replace (fect cy d) (hacker alyssa p))
     (salary (fect cy d) 35000)
     (salary (hacker alyssa p) 40000)
     (lisp-value < 35000 40000))


;; EXERCISE 4.58
;;; Query input:
(big-shot ?person ?division)
;;; Query results:
(big-shot (scrooge eben) accounting)
(big-shot (warbucks oliver) administration)
(big-shot (bitdiddle ben) computer)


;; EXERCISE 4.59
;; a.
;;; Query input:
(meeting ?dept (Friday ?time))
;;; Query results:
(meeting administration (friday 1pm))

;; c.
;;; Query results:
(meeting-time (hacker alyssa p) (wednesday 4pm))
(meeting-time (hacker alyssa p) (wednesday 3pm))


;; EXERCISE 4.60
;;; Query results:
(and (lives-near (aull dewitt) (reasoner louis))
     (lisp-value in-order? (aull dewitt) (reasoner louis)))
(and (lives-near (aull dewitt) (bitdiddle ben))
     (lisp-value in-order? (aull dewitt) (bitdiddle ben)))
(and (lives-near (fect cy d) (hacker alyssa p))
     (lisp-value in-order? (fect cy d) (hacker alyssa p)))
(and (lives-near (bitdiddle ben) (reasoner louis))
     (lisp-value in-order? (bitdiddle ben) (reasoner louis)))


;; EXERCISE 4.62
;;; Query input:
(last-pair (3) ?x)
;;; Query results:
(last-pair (3) 3)

;;; Query input:
(last-pair (1 2 3) ?x)
;;; Query results:
(last-pair (1 2 3) 3)

;;; Query input:
(last-pair (2 ?x) (3))
;;; Query results:
(last-pair (2 (3)) (3))


;; EXERCISE 4.63
;;; Query input:
(grandson Cain ?x)
;;; Query response:
(grandson cain irad)

;;; Query input:
(son Lamech ?x)
;;; Query response:
(son lamech jubal)
(son lamech jabal)

;;; Query input:
(grandson Methushael ?x)
;;; Query response:
(grandson methushael jubal)
(grandson methushael jabal)
