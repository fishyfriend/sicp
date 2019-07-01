(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;; Exercise 3.1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-accumulator init)
  (let ((value init))
    (lambda (n)
      (set! value (+ value n))
      value)))

;; Exercise 3.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-monitored f)
  (let ((calls 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) calls)
            ((eq? arg 'reset-count) (set! calls 0))
            (else (set! calls (+ calls 1))
                  (f arg))))))

;; Exercise 3.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  (lambda (attempt m)
    (if (eq? attempt password)
        (dispatch m)
        (lambda (m) "Incorrect password"))))

;; Exercise 3.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (call-the-cops)
  (error "The police have been contacted"))

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  (let ((attempts 0))
    (lambda (attempt m)
      (lambda (arg)
        (if (eq? attempt password)
            (begin (set! attempts 0)
                   ((dispatch m) arg))
            (begin (set! attempts (+ attempts 1))
                   (if (> attempts 7)
                       (call-the-cops)
                       "Incorrect password")))))))

;; Exercise 3.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (monte-carlo trials experiment)
     (abs (- x1 x2))
     (abs (- y1 y2))))

(define (in-unit-circle? x y)
  (<= (+ (square x) (square y)) 1.))

(define (estimate-pi)
  (estimate-integral in-unit-circle? -1. 1. -1. 1. 1000000.))

(estimate-pi) ; 3.141824

;; Exercise 3.6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rand-update x) (+ 1 x))
(define (random-init) (random 1000))

(define rand
  (let ((x (random-init)))
    (lambda (m)
      (cond ((eq? m 'generate) (set! x (rand-update x))
                               x)
            ((eq? m 'reset) (lambda (n) (set! x n)))))))

;; Exercise 3.7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (password-protect dispatch password)
  (lambda (attempt m)
    (if (eq? attempt password)
        (dispatch m)
        (lambda (m) "Incorrect password"))))

(define (make-joint account password new-password)
  (password-protect (lambda (m) (account password m))
                    new-password))

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  (password-protect dispatch password))

;; Exercise 3.8 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define f
  (let ((y 0))
    (lambda (x)
      (set! y (if (= y 0) 1 0))
      (if (= x y) (/ 1 2) 0))))

(+ (f 0) (f 1)) ; 1
(+ (f 1) (f 0)) ; 0

;; Exercise 3.9 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; recursive version

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

#|   global env
         ↓
[...................]
  ↑  ↑  ↑  ↑  ↑  ↑
  │  │  │  │  │  └[ n: 6 ] ← E1
  │  │  │  │  └[ n: 5 ] ← E2
  │  │  │  └[ n: 4 ] ← E3
  │  │  └[ n: 3 ] ← E4
  │  └[ n: 2 ] ← E5
  └[ n: 1 ] ← E6 |#

; iterative version

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

#|    global env
          ↓
[.....................]
  ↑  ↑  ↑  ↑  ↑  ↑  ↑
  │  │  │  │  │  │  └[ product: 1   counter: 1   max-count: 6 ] ← E1
  │  │  │  │  │  └[ product: 1   counter: 2   max-count: 6 ] ← E2
  │  │  │  │  └[ product: 2   counter: 3   max-count: 6 ] ← E3
  │  │  │  └[ product: 6   counter: 4   max-count: 6 ] ← E4
  │  │  └[ product: 24   counter: 5   max-count: 6 ] ← E5
  │  └[ product: 120   counter: 6   max-count: 6 ] ← E6
  └[ product: 720   counter: 7   max-count: 6 ] ← E7 |#
  
