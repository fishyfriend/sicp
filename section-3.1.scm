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
