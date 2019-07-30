;;;;CODE FROM CHAPTER 3 OF STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;; Examples from the book are commented out with ;: so that they
;;;  are easy to find and so that they will be omitted if you evaluate a
;;;  chunk of the file (programs with intervening examples) in Scheme.

;;; BEWARE: Although the whole file can be loaded into Scheme,
;;;  you won't want to do so.  For example, you generally do
;;;  not want to use the procedural representation of pairs
;;;  (cons, car, cdr as defined in section 3.3.1) instead of
;;;  Scheme's primitive pairs.

;;; Some things require code that is not in the book -- see ch3support.scm


;;;;SECTION 3.1

;;;SECTION 3.1.1

;: (withdraw 25)
;: (withdraw 25)
;: (withdraw 60)
;: (withdraw 15)

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))


(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))


(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))


;: (define W1 (make-withdraw 100))
;: (define W2 (make-withdraw 100))
;: (W1 50)
;: (W2 70)
;: (W2 40)
;: (W1 40)


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

;: (define acc (make-account 100))

;: ((acc 'withdraw) 50)
;: ((acc 'withdraw) 60)
;: ((acc 'deposit) 40)
;: ((acc 'withdraw) 60)

;: (define acc2 (make-account 100))


;; EXERCISE 3.1
(define (make-accumulator init)
  (let ((value init))
    (lambda (n)
      (set! value (+ value n))
      value)))

;: (define A (make-accumulator 5))
;: (A 10) ; 15
;: (A 10) ; 25


;; EXERCISE 3.2
(define (make-monitored f)
  (let ((calls 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) calls)
            ((eq? arg 'reset-count) (set! calls 0))
            (else (set! calls (+ calls 1))
                  (f arg))))))

;: (define s (make-monitored sqrt))
;: (s 100) ; 10
;: (s 'how-many-calls?) ; 1


;; EXERCISE 3.3
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
          (else (error "unknown request -- make-account"
                       m))))
  (lambda (attempt m)
    (if (eq? attempt password)
        (dispatch m)
        (lambda (m) "incorrect password"))))

;: (define acc (make-account 100 'secret-password))
;: ((acc 'secret-password 'withdraw) 40) ; 60
;: ((acc 'some-other-password 'deposit) 50) ; "Incorrect password"


;;EXERCISE 3.4
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

(define (call-the-cops)
  (error "The police have been contacted"))


;;;SECTION 3.1.2

;; *following uses rand-update -- see ch3support.scm
;; *also must set random-init to some value
(define random-init 7)			;**not in book**
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))


(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;; second version (no assignment)
(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))


;; EXERCISE 3.5
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

;(estimate-pi) ; 3.141824


;;EXERCISE 3.6
(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate) (set! x (rand-update x))
                               x)
            ((eq? m 'reset) (lambda (n) (set! x n)))))))


;;;SECTION 3.1.3

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))


;: (define W (make-simplified-withdraw 25))
;: (W 20)
;: (W 10)


(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

;: (define D (make-decrementer 25))
;: (D 20)
;: (D 10)

;: ((make-decrementer 25) 20)
;: ((lambda (amount) (- 25 amount)) 20)
;: (- 25 20)

;: ((make-simplified-withdraw 25) 20)

;: ((lambda (amount) (set! balance (- 25 amount)) 25) 20)
;: (set! balance (- 25 20)) 25

;;;Sameness and change

;: (define D1 (make-decrementer 25))
;: (define D2 (make-decrementer 25))
;:
;: (define W1 (make-simplified-withdraw 25))
;: (define W2 (make-simplified-withdraw 25))
;:
;: (W1 20)
;: (W1 20)
;: (W2 20)

;: (define peter-acc (make-account 100))
;: (define paul-acc (make-account 100))
;:
;: (define peter-acc (make-account 100))
;: (define paul-acc peter-acc)

;;;Pitfalls of imperative programming

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))


;; EXERCISE 3.7
(define (make-joint account password new-password)
  (password-protect (lambda (m) (account password m))
                    new-password))

(define (password-protect dispatch password)
  (lambda (attempt m)
    (if (eq? attempt password)
        (dispatch m)
        (lambda (m) "Incorrect password"))))

;: (define peter-acc (make-account 100 'open-sesame))
;: (define paul-acc
;:   (make-joint peter-acc 'open-sesame 'rosebud))
;: ((paul-acc 'rosebud 'withdraw) 10) ; 90
;: ((paul-acc 'open-sesame 'withdraw) 10) ; "Incorrect password"

;;EXERCISE 3.8
(define f
  (let ((y 0))
    (lambda (x)
      (set! y (if (= y 0) 1 0))
      (if (= x y) (/ 1 2) 0))))

;: (+ (f 0) (f 1)) ; 1
;: (+ (f 1) (f 0)) ; 0


;;;;SECTION 3.2

;;;SECTION 3.2.1

(define (square x)
  (* x x))

(define square
  (lambda (x) (* x x)))


;;;SECTION 3.2.2

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;: (sum-of-squares (+ a 1) (* a 2))


;; EXERCISE 3.9
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;      global env
;          ↓
; [...................]
;   ↑  ↑  ↑  ↑  ↑  ↑
;   │  │  │  │  │  └[ n: 6 ] ← E1
;   │  │  │  │  └[ n: 5 ] ← E2
;   │  │  │  └[ n: 4 ] ← E3
;   │  │  └[ n: 3 ] ← E4
;   │  └[ n: 2 ] ← E5
;   └[ n: 1 ] ← E6

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;       global env
;           ↓
; [.....................]
;   ↑  ↑  ↑  ↑  ↑  ↑  ↑
;   │  │  │  │  │  │  └[ product: 1   counter: 1   max-count: 6 ] ← E1
;   │  │  │  │  │  └[ product: 1   counter: 2   max-count: 6 ] ← E2
;   │  │  │  │  └[ product: 2   counter: 3   max-count: 6 ] ← E3
;   │  │  │  └[ product: 6   counter: 4   max-count: 6 ] ← E4
;   │  │  └[ product: 24   counter: 5   max-count: 6 ] ← E5
;   │  └[ product: 120   counter: 6   max-count: 6 ] ← E6
;   └[ product: 720   counter: 7   max-count: 6 ] ← E7


;;;SECTION 3.2.3

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

;: (define W1 (make-withdraw 100))
;: (W1 50)

;: (define W2 (make-withdraw 100))


;; EXERCISE 3.10

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))


;: (define W1 (make-withdraw 100))
;;
;;         ┌────────────────────────────────────────────────┐
;; global  │ make-withdraw: ──────────────────────────┐     │
;; env ───▶│ W1: ─┐                                   │     │
;;         └───── │ ───────────────────────────────── │ ────┘
;;                │                ▲                  │   ▲
;;                │                │                  │   │
;;                │        ┌─────────────────────┐    ▼   │
;;                │  E1 ──▶│ initial-amount: 100 │  (•I•)─┘
;;                │        └─────────────────────┘   │
;;                │             ▲             ▲      │
;;                │             │             │      ▼
;;                │        ┌──────────────┐   │    params: initial-amount
;;                │  E2 ──▶│ balance: 100 │   │    body: (let ((balance ...
;;                │        └──────────────┘   │
;;                ▼            ▲            (•I•) ◀── lambda from let-binding
;;              (•I•)──────────┘             │
;;               │                           │
;;               ▼                           ▼
;;             params: amount              params: balance
;;             body: (if (>= ...           body: (lambda (amount) ...
;;
;: (W1 50)
;;
;;         ┌────────────────────────────┐
;; global  │ make-withdraw: ...         │
;; env ───▶│ W1: ─┐                     │
;;         └───── │ ────────────────────┘
;;                │                ▲
;;                │                │
;;                │        ┌─────────────────────┐
;;                │  E1 ──▶│ initial-amount: 100 │
;;                │        └─────────────────────┘
;;                │             ▲
;;                │             │
;;                │        ┌──────────────┐  Here is the balance
;;                │  E2 ──▶│ balance: 100 │  that will be changed
;;                │        └──────────────┘  by the set!.
;;                ▼            ▲      ▲
;;              (•I•)──────────┘      │      ┌──────────────┐
;;               │                    └──────│ amount: 50   │
;;               ▼                           └──────────────┘
;;             params: amount
;;             body: (if (>= ...
;;
;: (define W2 (make-withdraw 100))
;;
;;         ┌─────────────────────────────────────────────────┐
;; global  │ make-withdraw: ...                              │
;; env ───▶│ W2: ────────────────────────────────┐           │
;;         │ W1: ─┐                              │           │
;;         └───── │ ──────────────────────────── │ ──────────┘
;;                │         ▲                    │         ▲
;;                │         │                    │         │
;;                │      ┌─────────────────────┐ │      ┌─────────────────────┐
;;                │ E1 ─▶│ initial-amount: 100 │ │ E3 ─▶│ initial-amount: 100 │
;;                │      └─────────────────────┘ │      └─────────────────────┘
;;                │           ▲            ▲     │           ▲            ▲
;;                │           │            │     │           │            │
;;                │      ┌──────────────┐  │     │      ┌──────────────┐  │
;;                │ E2 ─▶│ balance: 100 │  │     │ E4 ─▶│ balance: 100 │  │
;;                │      └──────────────┘  │     │      └──────────────┘  │
;;                ▼          ▲         * (•I•)   ▼          ▲         * (•I•)
;;              (•I•)────────┘            │    (•I•)────────┘            │
;;               │                        │     │                        │
;;               ▼                        │     ▼                        │
;;             params: amount             │   params: amount             │
;;             body: (if (>= ...          │   body: (if (>= ...          │
;;                                        ▼                              ▼
;;                              params: balance             params: balance
;; * = lambdas from             body:                       body:
;;     let-bindings               (lambda (amount)            (lambda (amount)
;;                                 ...                         ...
;;
;; Both versions of make-withdraw return lambdas that can be called to withdraw
;; from a private balance. Different lambdas created by make-withdraw do not
;; have access to other lambdas' balances; each individual lambda's balance is
;; hidden inside a frame that only that lambda can access.
;;
;; Whereas the first version of make-withdraw creates a single additional frame
;; for each lambda it returns, the second version creates two frames. This is
;; arguably less efficient since the outer frame is simply used to set up the
;; lambda and is no longer needed after the initial call to make-withdraw.
;; The inner frame is equivalent to the single frame used in the first version;
;; it simply stores the balance.

;;;SECTION 3.2.4

;; same as in section 1.1.8
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


;; EXERCISE 3.11

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

;: (define acc (make-account 50))
;: ((acc 'deposit) 40)
;: ((acc 'withdraw) 60)
;;
;;          ┌───────────────────────────────┐
;;  global  │ make-account: ─┐              │
;;  env ───▶│ acc: ─┐        │              │
;;          └────── │ ────── │ ─────────────┘        E1
;;                  │        ▼   ▲      ▲            │
;;      ┌──────────────────(•I•)─┘      │            ▼
;;      ▼           │          ┌─────────────────────────────────────────┐
;;  params:         │ ┌───────▶│ balance: 50                             │
;;   balance        │ │ ┌─────▶│ withdraw: ───────────────────────┐      │
;;  body:           │ │ │ ┌───▶│ deposit: ─────────────┐          │      │
;;   (define        │ │ │ │ ┌─▶│ dispatch: ─┐          │          │      │
;;     (withdraw …  │ │ │ │ │  └─────────── │ ──────── │ ──────── │ ─────┘
;;                  └─|─|─|─|───────┐       │   ▲      │   ▲      │   ▲
;;   ┌──────────────┐ │ │ │ │       │       ▼   │      ▼   │      ▼   │
;;   │ m: 'withdraw │─┘ │ │ │       └────▶(•I•)─┘    (•I•)─┘    (•I•)─┘
;;   └──────────────┘   │ │ │                │          │          │
;;   ┌──────────────┐   │ │ │                ▼          ▼          ▼
;;   │ amount: 60   │───┘ │ │              params:    params:    params:
;;   └──────────────┘     │ │               m:         amount     amount
;;   ┌──────────────┐     │ │              body:      body:      body:
;;   │ m: 'deposit  │─────┘ │               (cond …    (set! …    (if (>= …
;;   └──────────────┘       │
;;   ┌──────────────┐       │
;;   │ amount: 40   │───────┘
;;   └──────────────┘
;;
;; The local state for acc is kept in frame E1.
;:
;: (define acc2 (make-account 100))
;;
;; Only the global environment is shared between acc and acc2. The local state
;; of acc2 will be stored in a separate frame identical to, but entirely
;; separate from, E1.


;;;;SECTION 3.3

;;;SECTION 3.3.1

(define (cons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))


;; EXERCISE 3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;: (define x (list 'a 'b))
;: (define y (list 'c 'd))
;: (define z (append x y))
;: z
;: (cdr x)
;: ;Value: (b)
;;
;;        ┌───┬───┐        y ──▶┌───┬───┐
;;   x ──▶│ ● │ ● │         ┌──▶│ ● │ ● │
;;        └─┼─┴─┼─┘         │   └─┼─┴─┼─┘
;;          ▼   ▼           │     ▼   ▼
;;       ┌───┐ ┌───┬───┐    │  ┌───┐ ┌───┬───┐
;;       │ a │ │ ● │ / │    │  | c │ │ ● │ / │
;;       └───┘ └─┼─┴───┘    │  └───┘ └─┼─┴───┘
;;         ▲     ▼          │          ▼
;;         │   ┌───┐        │        ┌───┐
;;         │   │ b │◀───┐   │        | d │
;;         │   └───┘    │   │        └───┘
;;       ┌─┼─┬───┐    ┌─┼─┬─┼─┐
;;  z ──▶│ ● │ ●─┼───▶│ ● │ ● │
;;       └───┴───┘    └───┴───┘
;;
;: (define w (append! x y))
;: w
;: (cdr x)
;; ;Value: (b c d)
;;
;;   x ──▶┌───┬───┐        y ──▶┌───┬───┐
;;   w ──▶│ ● │ ● │  ┌─────────▶│ ● │ ● │
;;        └─┼─┴─┼─┘  │          └─┼─┴─┼─┘
;;          ▼   ▼    │            ▼   ▼
;;       ┌───┐ ┌───┬─┼─┐       ┌───┐ ┌───┬───┐
;;       │ a │ │ ● │ ● │       | c │ │ ● │ / │
;;       └───┘ └─┼─┴───┘       └───┘ └─┼─┴───┘
;;               ▼                     ▼
;;             ┌───┐                 ┌───┐
;;             │ b │                 | d │
;;             └───┘                 └───┘

;; EXERCISE 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;: (define z (make-cycle (list 'a 'b 'c)))
;;
;;          ┌─────────────────────────┐
;;          ▼                         │
;;      ┌───┬───┐   ┌───┬───┐   ┌───┬─┼─┐
;; z ──▶│ ● │ ●─┼──▶│ ● │ ●─┼──▶│ ● │ ● │
;;      └─┼─┴───┘   └─┼─┴───┘   └─┼─┴───┘
;;        ▼           ▼           ▼
;;      ┌───┐       ┌───┐       ┌───┐
;;      │ a │       │ b │       | c │
;;      └───┘       └───┘       └───┘
;;
;; Attempting to compute (last-pair z) results in an infinite loop since the
;; stopping condition (null? (cdr x)) is never reached.

;; EXERCISE 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; (define v (list 'a 'b 'c 'd))
;;
;;      ┌───┬───┐   ┌───┬───┐   ┌───┬───┐   ┌───┬───┐
;; v ──▶│ ● │ ●─┼──▶│ ● │ ●─┼──▶│ ● │ ●─┼──▶│ ● │ / │
;;      └─┼─┴───┘   └─┼─┴───┘   └─┼─┴───┘   └─┼─┴───┘
;;        ▼           ▼           ▼           ▼
;;      ┌───┐       ┌───┐       ┌───┐       ┌───┐
;;      │ a │       │ b │       | c │       | d │
;;      └───┘       └───┘       └───┘       └───┘
;;
;; (define w (mystery v))
;;
;;                ┌───────┐   ┌───────┐   ┌───────┐
;;      ┌───┬───┐ │ ┌───┬─┼─┐ │ ┌───┬─┼─┐ │ ┌───┬─┼─┐
;; v ──▶│ ● │ / │◀┘ │ ● │ ● │◀┘ │ ● │ ● │◀┘ │ ● │ ● │◀── w
;;      └─┼─┴───┘   └─┼─┴───┘   └─┼─┴───┘   └─┼─┴───┘
;;        ▼           ▼           ▼           ▼
;;      ┌───┐       ┌───┐       ┌───┐       ┌───┐
;;      │ a │       │ b │       | c │       | d │
;;      └───┘       └───┘       └───┘       └───┘
;;
;; v
;; ;Value: (a)
;;
;; w
;; ;Value: (d c b a)


;;; Sharing and identity

;: (define x (list 'a 'b))
;: (define z1 (cons x x))
;: (define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

;: z1
;: (set-to-wow! z1)
;: z2
;: (set-to-wow! z2)


;; EXERCISE 3.15
;; z1 after (set-to-wow! z1)
;;
;;       ┌───┬───┐
;; z1 ──▶│ ● │ ● │
;;       └─┼─┴─┼─┘
;;         ▼   ▼
;;       ┌───┬───┐   ┌───┬───┐
;;       │ ● │ ●─┼──▶│ ● │ / │
;;       └─┼─┴───┘   └─┼─┴───┘
;;         ▼           ▼
;;       ┌───┐       ┌───┐
;;       │wow│       │ b │
;;       └───┘       └───┘
;;
;; z2 after (set-to-wow! z2)
;;
;;       ┌───┬───┐   ┌───┬───┐   ┌───┬───┐
;; z2 ──▶│ ● │ ●─┼──▶│ ● │ ●─┼──▶│ ● │ / │
;;       └─┼─┴───┘   └─┼─┴───┘   └─┼─┴───┘
;;         │           ▼           ▼
;;         │         ┌───┐       ┌───┐
;;         │         │ a │       │ b │
;;         │         └───┘       └───┘
;;         │                       ▲
;;         │         ┌───┬───┐   ┌─┼─┬───┐
;;         └────────▶│ ● │ ●─┼──▶│ ● │ / │
;;                   └─┼─┴───┘   └───┴───┘
;;                     ▼
;;                   ┌───┐
;;                   │wow│
;;                   └───┘


;; EXERCISE 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; (count-pairs '(1 2 3))
;; ;Value: 3
;;
;; ┌───┬───┐   ┌───┬───┐   ┌───┬───┐
;; │ ● │ ● │──▶│ ● │ ● │──▶│ ● │ / │
;; └─┼─┴───┘   └─┼─┴───┘   └─┼─┴───┘
;;   ▼           ▼           ▼
;; ┌───┐       ┌───┐       ┌───┐
;; │ 1 │       │ 2 │       │ 3 │
;; └───┘       └───┘       └───┘
;;
;; (define x (list 'a))
;; (define y (list x x))
;; (count-pairs y)
;; ;Value: 4
;;
;;      ┌───┬───┐   ┌───┬───┐
;; y ──▶│ ● │ ● │──▶│ ● │ / │
;;      └─┼─┴───┘   └─┼─┴───┘
;;        ▼           │
;;      ┌───┬───┐     │
;; x ──▶│ ● │ / │◀────┘
;;      └─┼─┴───┘
;;        ▼
;;      ┌───┐
;;      │ a │
;;      └───┘
;;
;; (define x (cons 'a 'a))
;; (define y (cons x x))
;; (define z (cons y y))
;; (count-pairs z)
;; ;Value: 7
;;
;;      ┌───┬───┐
;; z ──▶│ ● │ ● │
;;      └─┼─┴─┼─┘
;;        ▼   ▼
;;      ┌───┬───┐
;; y ──▶│ ● │ ● │
;;      └─┼─┴─┼─┘
;;        ▼   ▼
;;      ┌───┬───┐
;; x ──▶│ ● │ ● │
;;      └─┼─┴─┼─┘
;;        ▼   ▼
;;        ┌───┐
;;        │ a │
;;        └───┘
;;
;; (define x '(1 2 3))
;; (set-cdr! (cdr (cdr x)) x)
;; (count-pairs x)
;; ;Aborting!: maximum recursion depth exceeded
;;
;;          ┌─────────────────────────┐
;;          ▼                         │
;;      ┌───┬───┐   ┌───┬───┐   ┌───┬─┼─┐
;; x ──▶│ ● │ ●─┼──▶│ ● │ ●─┼──▶│ ● │ ● │
;;      └─┼─┴───┘   └─┼─┴───┘   └─┼─┴───┘
;;        ▼           ▼           ▼
;;      ┌───┐       ┌───┐       ┌───┐
;;      │ 1 │       │ 2 │       | 3 │
;;      └───┘       └───┘       └───┘

;; EXERCISE 3.17
(define (count-pairs x)
  (define pairs '())
  (define (iter x)
    (if (or (not (pair? x))
            (memq x pairs))
        0
        (begin (set! pairs (cons x pairs))
               (+ (iter (car x))
                  (iter (cdr x))
                  1))))
  (iter x))

;; EXERCISE 3.18
(define (cycle? x)
  (define pairs '())
  (define (iter x)
    (cond ((null? x) false)
          ((memq (cdr x) pairs) true)
          (else (set! pairs (cons (cdr x) pairs))
                (iter (cdr x)))))
  (iter x))

;; EXERCISE 3.19
(define (cycle? x)
  (define (iter ys)
    (cond ((null? ys) false)
          ((eq? (cdr ys) x) true)
          (else (iter (cdr ys)))))
  (iter x))


;;;Mutation as assignment

(define (cons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))


(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)


;; EXERCISE 3.20
;: (define x (cons 1 2))
;;
;;         ┌──────────────────────────────────────────────────────────────┐
;; global  │ cons: ────────────────────┐ set-car!: ───────────────┐       │
;; env ───▶│ x: ────────────┐          │ set-cdr!: ─┐             │       │
;;         │ car: ──────────┼─────┐    │ cdr: ─┐    │             │       │
;;         └────────────────┼─────┼────┼───────┼────┼─────────────┼───────┘
;;               ▲          │     │    │    ▲  │    │   ▲         │   ▲
;;       ┌───────┴───────┐  │     │    ▼    │  │    ▼   │         ▼   │
;;       │ x: 1          │  │     │  (•I•)──┘  │  (•I•)─┘       (•I•)─┘
;;       │ y: 2          │  │     │   │        │   │              │
;;       │ set-x!: ...   │◀─┼──┐  │   ▼        │   ▼              ▼
;;       │ set-y!: ...   │  │  │  │ params:    │ params:        params:
;;       │ dispatch: ─┐  │  │  │  │  x y       │  z new-value    z new-value
;;       └────────────┼──┘  │  │  │ body:      │ body:          body:
;;                    │ ┌───┘  │  │  (define   │  ((z 'set-cdr!   ((z 'set-car!
;;                    ▼ ▼      │  │    set-x!  │  ...             ...
;;                   (•I•)─────┘  │  ...       │
;;                    │           ▼            ▼
;;                    ▼         params:       params:
;;                  params: m    z             z
;;                  body:       body:         body:
;;                   (cond …     ((z 'car))     ((z 'cdr))
;;
;: (define z (cons x x))
;;
;;         ┌──────────────────────────────────────────────────────────────────┐
;; global  │ cons: ────────────────────────────────────────────┐  set-car!: … │
;; env ───▶│ x: ───────────────────────────────────────┐       │  set-cdr!: … │
;;         │ z: ────────┐ car: ...                     │       │  cdr: ...    │
;;         └────────────┼──────────────────────────────┼───────┼──────────────┘
;;           ▲          │                   ▲          │       │    ▲
;;   ┌───────┴───────┐  │           ┌───────┴───────┐  │       ▼    │
;;   │ x: ───────────┼──┼────────┐  │ x: 1          │  │     (•I•)──┘
;;   │ y: ───────────┼──┼─────┐  │  │ y: 2          │  │      │
;;   │ set-x!: ...   │◀─┼──┐  │  │  │ set-x!: ...   │◀─┼──┐   ▼
;;   │ set-y!: ...   │  │  │  │  │  │ set-y!: ...   │  │  │ params:
;;   │ dispatch: ─┐  │  │  │  │  │  │ dispatch: ─┐  │  │  │  x y
;;   └────────────┼──┘  │  │  │  │  └────────────┼──┘  │  │ body:
;;                │ ┌───┘  │  │  │               │ ┌───┘  │  (define set-x! …
;;                ▼ ▼      │  │  │               ▼ ▼      │
;;               (•I•)─────┘  └──╘═════════════▶(•I•)─────┘
;;                │                              │
;;                ▼                              ▼
;;              params: m                      params: m
;;              body: (cond …                  body: (cond …
;;
;: (set-car! (cdr z) 17)
;;
;;           ┌────────────────────────────────────────────────────────────────┐
;;   global  │ cons: ...                                    set-car!: ...     │
;;   env ───▶│ x: ───────────────────────────────────────┐  set-cdr!: ...     │
;; ┌────────▶│ z: ────────┐ car: ...                     │  cdr: ...          │
;; │         └────────────┼──────────────────────────────┼────────────────────┘
;; │           ▲          │                   ▲          │            ▲
;; │   ┌───────┴───────┐  │           ┌───────┴───────┐  │         ┌──┴──┐
;; │   │ x: ───────────┼──┼────────┐  │ x: 1 ─> 17    │  │         │ z: ─┼────┐
;; │   │ y: ───────────┼──┼─────┐  │  │ y: 2          │  │         └─────┘    │
;; │ ┌▶│ set-x!: ...   │◀─┼──┐  │  │  │ set-x!: ...   │◀─┼──┐   ┌───────────┐ |
;; │ │ │ set-y!: ...   │  │  │  │  │  │ set-y!: ...   │◀─┼──┼───┤m:'set-car!│ │
;; │ │ │ dispatch: ─┐  │  │  │  │  │  │ dispatch: ─┐  │◀─┼──┼──┐└───────────┘ │
;; │ │ └────────────┼──┘  │  │  │  │  └────────────┼──┘  │  │  │  ┌───────┐   │
;; │ │  ┌─────┐     │ ┌───┘  │  │  │               │ ┌───┘  │  └──┤ v: 17 │   │
;; └─┼──┤ z: ─┼─┐   ▼ ▼      │  │  │               ▼ ▼      │     └───────┘   │
;;   │  └─────┘ └─▶(•I•)─────┘  └──╘═════════════▶(•I•)─────┘                 │
;;   │┌─────────┐   │                              │ ▲                        │
;;   └┤ m: 'cdr │   ▼                              ▼ └────────────────────────┘
;;    └─────────┘ params: m                   params: m
;;                body: (cond …               body: (cond …
;;
;: (car x)
;; ;Value: 17


;;;SECTION 3.3.2

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


;; EXERCISE 3.21
;: (define q1 (make-queue))
;;
;: (insert-queue! q1 'a)
;; ;Value: ((a) a)
;;
;: (insert-queue! q1 'b)
;; ;Value: ((a b) b)
;;
;: (delete-queue! q1)
;: ;Value: ((b) b)
;;
;: (delete-queue! q1)
;; ;Value: (() b)

;; Even after deleting the last item in the queue, the rear pointer remains
;; pointed at that item because there is no need to update it. The rear pointer
;; is meaningless in an empty queue per the implementation. Since queues are
;; represented as list structures, the REPL prints them as such, and so the
;; contents of the rear pointer remain "visible" even though they are (from the
;; perspective of queue logic) purely vestigial.

(define (print-queue q)
  (display (front-ptr q)))

;; EXERCISE 3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?) (null? front-ptr))
    (define (front)
      (if (empty?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))
    (define (insert! x)
      (if (empty?)
          (begin (set! front-ptr (list x))
                 (set! rear-ptr front-ptr))
          (begin (set-cdr! rear-ptr (list x))
                 (set! rear-ptr (cdr rear-ptr))))
      dispatch)
    (define (delete!)
      (if (empty?)
          (error "DELETE! called with an empty queue")
          (let ((x (front)))
            (set! front-ptr (cdr front-ptr))
            x)))
    (define (dispatch m)
      (cond ((eq? m 'empty?) empty?)
            ((eq? m 'front) front)
            ((eq? m 'insert!) insert!)
            ((eq? m 'delete!) delete!)
            (else (error "Undefined operation -- MAKE-QUEUE" m))))
    dispatch))

(define (empty-queue? q) ((q 'empty?)))
(define (front-queue q) ((q 'front)))
(define (insert-queue! q x) ((q 'insert!) x))
(define (delete-queue! q) ((q 'delete!)))

;; EXERCISE 2.23
;; We can reuse front-ptr, rear-ptr, set-front-ptr!, and set-rear-ptr!
(define (make-deque) (cons '() '()))
(define (empty-deque? d) (null? (front-ptr d)))
(define (front-deque d) (car (front-ptr d)))
(define (rear-deque d) (car (rear-ptr d)))

(define (front-insert-deque! d x)
  (let ((ptr (cons x (front-ptr d))))
    (if (empty-deque? d)
        (set-rear-ptr! d ptr))
    (set-front-ptr! d ptr)
    d))

(define (rear-insert-deque! d x)
  (let ((ptr (cons x (rear-ptr d))))
    (if (empty-deque? d)
        (set-front-ptr! d ptr))
    (set-rear-ptr! d ptr)
    d))

(define (front-delete-deque! d)
  (if (empty-deque? d)
      (error "DELETE! called with an empty deque" d)
      (let ((value (front-deque d))
            (ptr (front-ptr d)))
        (set-front-ptr! d (cdr ptr))
        (if (eq? ptr (rear-ptr d))
            (set-rear-ptr! d (cdr ptr)))
        value)))

(define (rear-delete-deque! d)
  (if (empty-deque? d)
      (error "DELETE! called with an empty deque" d)
      (let ((value (rear-deque d))
            (ptr (rear-ptr d)))
        (set-rear-ptr! d (cdr ptr))
        (if (eq? ptr (front-ptr d))
            (set-front-ptr! d (cdr ptr)))
        value)))


;;;SECTION 3.3.3

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

;; two-dimensional
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

;; local tables
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


;; EXERCISE 3.24
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (display (list 'lookup key)) (newline)
      (let ((record (lookup-full key)))
        (if record (cdr record) false)))
    (define (lookup-full key)
      (display (list 'lookup-full key)) (newline)
      (define (iter entries)
        (cond ((null? entries) false)
              ((same-key? key (caar entries)) (car entries))
              (else (iter (cdr entries)))))
      (iter (cdr local-table)))
    (define (insert! key value)
      (display (list 'insert! key value)) (newline)
      (let ((record (lookup-full key)))
        ;; only replace record when key is an exact match
        (if (and record (equal? (car record) key))
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; EXERCISE 3.25
;; I provide two implementations of multidimensional tables. The first is based
;; on the text's implementation of two-dimensional tables, using nested
;; subtables. The second is simply a one-dimensional table that forces the use
;; of lists as keys -- i.e., compound keys.
;;
;; The first version is more memory-efficient as it only stores each key once.
;; It has the property that subtables are treated as values: for example, if I
;; have a subtable under the compound key '(a b), then:
;;
;;   - (get '(a b)) returns the list of all key-value pairs in the subtable.
;;   - (put '(a b) 4) makes (get '(a b)) return 4; the prior contents of the
;;     subtable under '(a b) are lost.
;;   - (put '(a b) (list (cons 'c 5))) makes (get '(a b c)) return 5. The
;;     inserted key-value pair is treated as a subtable.
;;
;; The second version does not have this property, and permits storing values
;; under keys that could be prefixes of other keys. For example, after doing
;;
;;   (put '(a b) 4)
;;   (put '(a b c) 5)
;;
;; I can look up either of these key-value pairs in the table.
;;
;; These implementations address different interpretations of the exercise
;; prompt. It could be that a given application wishes to preserve the nested
;; tables approach for memory efficiency, while using the behavioral semantics
;; of the second version. To achieve this we could add some additional fields to
;; the data structures to differentiate between subtables (which, in this
;; application, may not be returned as lookup results) and values (which may).
;; Whether the resulting implementation is actually more memory-efficient than
;; simply using the second version would depend on the size of typical keys and
;; the number of keys in a typical compound key.

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (iter keys table)
        (if (null? keys)
            (cdr table)
            (let ((subtable (assoc (car keys) (cdr table))))
              (if subtable
                (iter (cdr keys) subtable)
                false))))
      (iter keys local-table))
    (define (insert! keys value)
      (define (iter keys table)
        (if (null? keys)
            (set-cdr! table value)
            (let ((record (assoc (car keys) (cdr table))))
              (let ((subtable
                     (if record
                         record
                         (let ((new-subtable (cons (car keys) '())))
                           (set-cdr! table (cons new-subtable (cdr table)))
                           new-subtable))))
                    (iter (cdr keys) subtable)))))
      (iter keys local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys table)
      (if (list? keys)
          (let ((record (assoc keys (cdr table))))
            (if record
                (cdr record)
                false)))
          (error "List of keys is required -- LOOKUP"))
    (define (insert! keys value table)
      (if (list? keys)
          (let ((record (assoc keys (cdr table))))
            (if record
                (set-cdr! record value)
                (set-cdr! table
                          (cons (cons keys value) (cdr table)))))
          'ok)
          (error "List of keys is required -- INSERT!"))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; EXERCISE 3.26
;; To implement a multidimensional table using binary trees, we can use a
;; one-dimensional key-value representation with lists as compound keys, as in
;; the second implementation from exercise 25. The user would specify a
;; comparator for individual keys (e.g. numeric or alphabetic comparison), and
;; we would use this to create a compound key comparator which first compares
;; the size of the compound keys (i.e. list length) and then the individual
;; subkeys in front-to-back order. This compound key comparator would be used to
;; implement the binary tree lookup and insertion operations. I implement such a
;; table below. (Keys must be equal?-comparable.)
(define (make-table key-less-than?)
  ;; tree representation
  (define (empty-tree) (cons '() '()))
  (define (empty-tree? tree) (null? (car tree)))
  (define (key tree) (caar tree))
  (define (value tree) (cdar tree))
  (define (left tree) (cadr tree))
  (define (right tree) (cddr tree))
  ;; tree operations
  (define (lookup k tree)
    (if (list? k)
        (if (empty-tree? tree)
            false
            (let ((comp (compare-compound-keys k (key tree))))
              (cond ((eq? comp '<) (lookup k (left tree)))
                    ((eq? comp '>) (lookup k (right tree)))
                    (else (value tree)))))
        (error "Key must be a list -- LOOKUP" k)))
  (define (insert! k v tree)
    (if (list? k)
        (if (empty-tree? tree)
            (begin (set-car! tree (cons k v))
                   (set-cdr! tree (cons (empty-tree) (empty-tree))))
            (let ((comp (compare-compound-keys k (key tree))))
              (cond ((eq? comp '<) (insert! k v (left tree)))
                    ((eq? comp '>) (insert! k v (right tree)))
                    (else (set-cdr! (car tree) v)))))
        (error "Key must be a list -- INSERT!")))
  ;; key comparison
  (define (compare-keys k1 k2)
    (cond ((equal? k1 k2) '=)
          ((key-less-than? k1 k2) '<)
          (else '>)))
  (define (compare-compound-keys ck1 ck2)
    (let ((l1 (length ck1))
          (l2 (length ck2)))
    (cond ((< l1 l2) '<)
          ((> l1 l2 '>)
          ((null? ck1) '=)
          (else
            (let ((head-comp (compare-keys (car ck1) (car ck2))))
              (if (eq? head-comp '=)
                  (compare-compound-keys (cdr ck1) (cdr ck2))
                  head-comp)))))))
  (define local-table (empty-tree))
  (define (dispatch m)
    (cond ((eq? m 'lookup-proc) (lambda (k) (lookup k local-table)))
          ((eq? m 'insert-proc!) (lambda (k v) (insert! k v local-table)))
          (else (error "Unknown operation -- TABLE" m))))
  dispatch)

;; EXERCISE 3.27
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

;; note: p-c-r = previously-computed-result
;;
;;        ┌───────────────────────────────────────────────────────────┐
;; global │ memoize: ────────────────────────────────────┐            │
;; env ──▶│ memo-fib: ─────────────────────┐             │            │
;;        └────────────────────────────────┼─────────────┼────────────┘
;;           ▲         ▲         ▲   ▲     ▼   ▲   ▲     ▼   ▲   ▲
;;           │   ┌─────┼──▶(•I•)─┘   │   (•I•)─┘   │   (•I•)─┘   │
;;           │   │     │    │        │    │        │    │        │
;;           │   │     │    ▼        │    ▼        │    ▼        │
;;           │   │     │ params: n   │ params: x   │ params: f   │
;;           │   │     │ body:       │ body:       │ body:       │
;;        ┌──┴───┼──┐  │  (cond      │  (let       │  (let       │
;;        │ f: ──┘  │  │   ((= n 0)  │   ((p-c-r   │   ((table   │
;;        └─────────┘  │    …        │     …       │    …        │
;;             ▲       │             │             │             │
;;        ┌────┴────┐◀─┼─────────────┼─────────────┼─────────────┼──────┐
;;        │table: … │◀─┼─────────────┼─────────────┼──────┐      │      │
;;        │         │◀─┼─────────────┼──────┐      │      │      │      │
;;        └─────────┘◀─┼──────┐      │      │      │      │      │      │
;;             ▲       │      │      │      │      │      │      │      │
;;        ┌────┴────┐  │ ┌────┴────┐ │ ┌────┴────┐ │ ┌────┴────┐ │ ┌────┴────┐
;;        │ x: 3    │  │ │ x: 2    │ │ │ x: 1    │ │ │ x: 0    │ │ │ x: 1    │
;;        └─────────┘  │ └─────────┘ │ └─────────┘ │ └─────────┘ │ └─────────┘
;;             ▲       │      ▲      │      ▲      │      ▲      │      ▲
;;        ┌────┴────┐  │ ┌────┴────┐ │ ┌────┴────┐ │ ┌────┴────┐ │ ┌────┴────┐
;;        │p-c-r: #f│  │ │p-c-r: #f│ │ │p-c-r: #f│ │ │p-c-r: #f│ │ │p-c-r: 1 │
;;        └─────────┘  │ └─────────┘ │ └─────────┘ │ └─────────┘ │ └─────────┘
;;             ▲       │      ▲      │      ▲      │      ▲      │
;;        ┌────┴────┐  │ ┌────┴────┐ │ ┌────┴────┐ │ ┌────┴────┐ │
;;        │result: 2│  │ │result: 1│ │ │result: 1│ │ │result: 0│ │
;;        └─────────┘  │ └─────────┘ │ └─────────┘ │ └─────────┘ │
;;        ┌───────┐    │ ┌───────┐   │ ┌───────┐   │ ┌───────┐   │
;;        │ n: 3  │────┘ │ n: 2  │───┘ │ n: 1  │───┘ │ n: 0  │───┘
;;        └───────┘      └───────┘     └───────┘     └───────┘
;;
;; memo-fib computes the nth Fibonacci term in a number of steps proportional to
;; n because it executes the memoized procedure exactly n times and each one
;; adds the same amount of work:
;;
;;   - one execution of the memoized procedure's mathematical logic
;;   - one recursive call resulting in a lookup in the result table (which we
;;     assume is constant-time)
;;   - one recursive call to memo-fib which kicks off the next iteration (i.e.,
;;     for which a memoized result is not available).
;;
;; Defining memo-fib to be (memoize fib) would not accrue the same performance
;; benefit since fib is still calling its old, non-memoized self internally. We
;; would still get the benefit of caching if memo-fib were being called multiple
;; times over the course of a program, but the first call for any given n would
;; not benefit from memoization.


;;;SECTION 3.3.4

;: (define a (make-wire))
;: (define b (make-wire))
;: (define c (make-wire))
;: (define d (make-wire))
;: (define e (make-wire))
;: (define s (make-wire))
;:
;: (or-gate a b d)
;: (and-gate a b c)
;: (inverter c e)
;: (and-gate d e s)


;;NB. To use half-adder, need or-gate from exercise 3.28
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

;; *following uses logical-and -- see ch3support.scm

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)


;; EXERCISE 3.28
(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (let ((sigs (list s1 s2)))
    (cond ((equal? sigs '(0 0)) 0)
          ((or (equal? sigs '(0 1))
               (equal? sigs '(1 0))
               (equal? sigs '(1 1))) 1)
          (else (error "Invalid signal(s)" s1 s2)))))

;; EXERCISE 3.29
;; The delay time of this or-gate is equal to two inverter delays plus one
;; and-gate delay.
(define (or-gate o1 o2 output)
  (let ((a (make-wire))
        (b (make-wire))
        (c (make-wire)))
    (inverter o1 a)
    (inverter o2 b)
    (and-gate a b c)
    (inverter c output)
    'ok))

;; EXERCISE 3.30
(define (ripple-carry-adder as bs ss c)
  (if (null? as)
      'ok
      (let ((c-in (make-wire)))
        (full-adder (car as) (car bs) c-in (car ss) c)
        (ripple-carry-adder (cdr as) (cdr bs) (cdr ss) c-in))))

;; Derivation of the delay for a ripple-carry adder of n bits:
;;
;;   D(x) := delay for output from function box x
;;   rₙ   := ripple-carry adder of n bits
;;   f    := full adder
;;   h    := half-adder
;;   a    := and-gate
;;   o    := or-gate
;;   i    := inverter
;;
;;   D(h) = D(a) + max(D(o), D(a) + D(i))
;;   D(f) = 2 * D(h) + D(o)
;;   D(rₙ) = n * D(f)
;;         = n * (2 * D(h)                            + D(o))
;;         = n * (2 * (D(a) + max(D(o), D(a) + D(i))) + D(o))
;;
;;         = / n * (2 * (D(a) + D(o))        + D(o)) if D(o) > D(a) + D(i)
;;           \ n * (2 * (D(a) + D(a) + D(i)) + D(o)) otherwise
;;
;;         = / n * (2 * D(a) + 3 * D(o))             if D(o) > D(a) + D(i)
;;           \ n * (4 * D(a) + 2 * D(i) + D(o))      otherwise


;;;SECTION 3.3.4 again
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

;;; Sample simulation

;: (define the-agenda (make-agenda))
;: (define inverter-delay 2)
;: (define and-gate-delay 3)
;: (define or-gate-delay 5)
;:
;: (define input-1 (make-wire))
;: (define input-2 (make-wire))
;: (define sum (make-wire))
;: (define carry (make-wire))
;:
;: (probe 'sum sum)
;: (probe 'carry carry)
;:
;: (half-adder input-1 input-2 sum carry)
;: (set-signal! input-1 1)
;: (propagate)
;:
;: (set-signal! input-2 1)
;: (propagate)


;; EXERCISE 3.31
;: (define (accept-action-procedure! proc)
;:   (set! action-procedures (cons proc action-procedures)))
;;
;; The initial trigger of each action procedure is necessary because some wires
;; should have an initial state of 1 rather than the default initial state 0.
;; For example, the output of an inverter whose input is 0 should initially be
;; 1. With accept-action-procedure! defined as above, those non-default initial
;; states won't be set, and so when the simulation runs, any actions that are
;; registered for those wires will fail to trigger as expected when some other
;; action sets the value of the wire to 0, or trigger inappropriately when some
;; other action sets the value to 1. In the half-adder example above, the
;; responses change as follows:
;;
;;   (probe 'sum sum)
;;   (probe 'carry carry)
;;   ;; No diagnostics output.
;;
;;   (half-adder input-1 input-2 sum carry)
;;   ;Value: ok
;;   ;; Note that wire E inside the half-adder (fig. 25) is now 0, should be 1.
;;
;;   (set-signal! input-1 1)
;;   ;Value: done
;;
;;   (propagate)
;;   ;Value: done
;;   ;; No diagnostics output as output values did not change.
;;   ;; Time is now 5 (should be 8) as the D-E-S and-gate failed to trigger.
;;   ;; sum is now 0 (should be 1).
;;
;;   (set-signal! input-2 1)
;;   (propagate)
;;   ;Output: carry 8  New-value = 1
;;   ;; carry is correct; wire E does not affect carry.
;;   ;; Wire E was set to 0 (correct but unchanged) & does not trigger actions.
;;   ;; No output for sum as it remains at 0 (correct).


;;;Implementing agenda

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))


;; EXERCISE 3.32
;; The prompt asks why actions that occur within a segment must be executed in
;; the order they were triggered and added to the agenda.
;;
;; Actions that are triggered during the course of a time segment may carry
;; state that depends on the ordering of state changes within that segment. If
;; two or more actions are scheduled with the same delay, then they must be
;; executed in the order triggered to ensure that the states that they each
;; "see" are consistent with respect to that ordering, and properly reflect the
;; effects of other actions triggered by prior state changes in the originating
;; time segment.
;;
;; To think about it another way: the ordering of state changes within a time
;; segment effectively divides that segment into smaller time "ticks" which
;; are infinitesimal but have an ordering with respect to one another. Consider
;; two consecutive ticks where the first one happens at time t0 and the second
;; at time t0 + dt. If each tick triggers an action with delay d, then we expect
;; the first-triggered action to execute at t0 + d and the second at
;; t0 + dt + d. This ordering is only possible if we execute triggered actions
;; in FIFO fashion.
;;
;; In the case of an and-gate whose inputs change from (0,1) to (1,0) within a
;; segment, there are two relevant state changes within that segment. Input a1
;; is updated from 0 to 1, and input a2 is updated from 1 to 0. Each of these
;; updates triggers an action (and-action-procedure) which calculates the new
;; output value and schedules a further action to update the output wire after
;; a delay equal to and-gate-delay.
;;
;; Now, if it so happens that the first input to be updated is a1, the new
;; output value computed by the first update procedure will be 1, since at the
;; time it is calculated, the values of a1 and a2 are momentarily both 1. Then
;; a2 is updated from 0 to 1, its own triggered action calculates the new value
;; as 0. If we execute the delayed update actions in FIFO order then all is
;; well; the output is momentarily changed to 1 by the first update, then
;; immediately back to 0 by the second. If we do it in LIFO order, as we might
;; do if we stored scheduled procedures in a list, then we end up with the
;; output set to 1, which is incorrect. This illustrates why FIFO order is
;; correct.


;;;SECTION 3.3.5

;: (define C (make-connector))
;: (define F (make-connector))
;: (celsius-fahrenheit-converter C F)

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

;: (probe "Celsius temp" C)
;: (probe "Fahrenheit temp" F)
;: (set-value! C 25 'user)
;: (set-value! F 212 'user)
;: (forget-value! C 'user)
;: (set-value! F 212 'user)


(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))


;; EXERCISE 3.33
(define (averager a b c)
  (let ((x (make-connector))
        (y (make-connector)))
    (adder a b x)
    (constant 2 y)
    (multiplier y c x)
    'ok))


;; EXERCISE 3.34
;; The problem is that multiplier requires two values in order to solve for the
;; third. If a is known the solution works fine, b is calculated as a * a. But
;; if only b is known then a can't be inferred since only one term is known.
(define (squarer a b)
  (multiplier a a b))


;; EXERCISE 3.35
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (square (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)


;; EXERCISE 3.36
;: (define a (make-connector))
;: (define b (make-connector))
;: (set-value! a 10 'user)

;;        ┌───────────────────────────────────────────────────────────────┐
;; global │ a: ──────────────────────────────────┐  inform-about-value: … │
;; env ──▶│ make-connector: ─────────┐           │  …                     │
;;   ┌───▶│ set-value!: ─┐           │           │  …                     │
;;   │    └──────────────┼───────────┼───────────┼────────────────────────┘
;;   │                   ▼   ▲       ▼   ▲       │           ▲
;;   │                 (•I•)─┘     (•I•)─┘       │  ┌────────┴─────────┐
;;   │                   │          │            │  │ value: 10        │
;;   │                   ▼          ▼            │  │ informant: 'user │
;;   │      params: connector   params: -        │  │ constraints: '() │
;;   │              new-value   body:            │  └──────────────────┘
;;   │              informant    (let            │           ▲
;;   │      body: ((connector     ((value        │  ┌────────┼──────────────┐
;;   │              'set-value)     …            │  │ forget-my-value: …    │
;;   │             …                             │  │ conncet: …            │
;;   │                                           │  │ set-my-value: ─┐      │
;;   │                    ┌──────────────────────┼─▶│ me: ─┐         │      │
;; ┌─┴─────────────────┐  │  ┌───────────────────┼─▶└──────┼─────────┼──────┘
;; │ connector: ───────┼──┼──┼───────────────────┼───────┐ │   ▲     │   ▲
;; │ new-value: 10     │  │  │                   │       ▼ ▼   │     ▼   │
;; | informant: 'user  │  │  │                   └──────▶(•I•)─┘   (•I•)─┘
;; └───────────────────┘  │  │                              │         │
;; ┌───────────────────┐  │  │                              ▼         ▼
;; │request:'set-value!├──┘  │                         params:    params:
;; └───────────────────┘     │                          request    newval
;; ┌───────────────────┐     │                         body:       setter
;; │ newval: 10        ├─────┘                          (cond     body:
;; │ setter: 'user     │                                  ((eq?    (cond ((not
;; └───────────────────┘                                    …              …


;; EXERCISE 3.37
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

;: (define C (make-connector))
;: (define F (celsius-fahrenheit-converter C))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv k)
  (let ((x (make-connector)))
    (constant k x)
    x))


;;;SECTION 3.4
;;;**Need parallel-execute, available for MIT Scheme

;;;SECTION 3.4.1

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))


;; EXERCISE 3.38
;: (set! balance (+ balance 10))
;: (set! balance (- balance 20))
;: (set! balance (- balance (/ balance 2)))
;;
;; a. Possible final values for balance: 35 40 45 50
;; b. Some possible final values if interleaving is allowed: 55 80 110
;;
;;  │   Bank            Peter           Paul            Mary
;;  │
;;  │   $100 ─────────────┬───────────────┬───────────────┐
;;  │                     ▼               ▼               ▼
;;  │                   Access          Access          Access
;;  │                   balance:        balance:        balance:
;;  │                   $100            $100            $100
;;  │                     │               │               │
;;  │                     ▼               ▼               ▼
;;  │                   new value:      new value:      new value:
;;  │                   100+10=110      100-20=80       100/2=50
;;  │                     │               │               │
;;  │                     ▼               │               │
;;  │                   set! balance      │               │
;;  │                   to $110           │               ▼
;;  │   $110 ◀────────────┘               │             set! balance
;;  │                                     │             to $50
;;  │    $50 ◀────────────────────────────┼───────────────┘
;;  │                                     ▼
;;  │                                   set! balance
;;  │                                   to $80
;;  V    $80 ◀────────────────────────────┘
;; time
;;
;;  │   Bank            Peter           Paul            Mary
;;  │
;;  │   $100 ─────────────┬───────────────┬───────────────┐
;;  │                     ▼               │               │
;;  │                   Access            │               │
;;  │                   balance:          │               │
;;  │                   $100              │               │
;;  │                     │               │               │
;;  │                     ▼               │               │
;;  │                   new value:        │               │
;;  │                   100+10=110        │               │
;;  │                     │               │               │
;;  │                     ▼               │               │
;;  │                   set! balance      │               │
;;  │                   to $110           │               │
;;  │   $110 ◀────────────┘             Access          Access
;;  │                                   balance:        balance:
;;  │                                   $110            $110
;;  │                                     │               │
;;  │                                     ▼               ▼
;;  │                                   new value:      new value:
;;  │                                   110-20=90       110/2=55
;;  │                                     │               │
;;  │                                     ▼               │
;;  │                                   set! balance      │
;;  │                                   to $90            ▼
;;  │    $90 ◀────────────────────────────┘             set! balance
;;  │                                                   to $55
;;  V    $55 ◀────────────────────────────────────────────┘
;; time
;;
;;  │   Bank            Peter           Paul            Mary
;;  │
;;  │   $100 ─────────────┬───────────────┬───────────────┐
;;  │                     ▼               ▼               ▼
;;  │                   Access          Access          Access
;;  │                   balance:        balance:        balance:
;;  │                   $100            $100            $100
;;  │                     │               │               │
;;  │                     ▼               ▼               ▼
;;  │                   new value:      new value:      new value:
;;  │                   100+10=110      100-20=80       100/2=50
;;  │                     │               │               │
;;  │                     │             set! balance      │
;;  │                     │             to $80            ▼
;;  │    $80 ◀────────────┼───────────────┘             set! balance
;;  │                     │                             to $50
;;  │    $50 ◀────────────┼───────────────────────────────┘
;;  │                     ▼
;;  │                   set! balance
;;  │                   to $110
;;  V   $110 ◀────────────┘
;; time


;;;SECTION 3.4.2

;: (define x 10)
;: (parallel-execute (lambda () (set! x (* x x)))
;:                   (lambda () (set! x (+ x 1))))

;: (define x 10)
;: (define s (make-serializer))
;: (parallel-execute (s (lambda () (set! x (* x x))))
;:                   (s (lambda () (set! x (+ x 1)))))


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))


;; EXERCISE 3.39
;: (define x 10)
;: (define s (make-serializer))
;: (parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
;:                   (s (lambda () (set! x (+ x 1)))))
;;
;; Four of the five original possible values of x are still possible: 11, 100,
;; 101, and 121.


;; EXERCISE 3.40
;: (define x 10)
;: (parallel-execute (lambda () (set! x (* x x)))
;:                   (lambda () (set! x (* x x x))))
;:
;; The possible values of x are 10², 10³, 10⁴, 10⁵, and 10⁶.
;:
;: (define x 10)
;: (define s (make-serializer))
;: (parallel-execute (s (lambda () (set! x (* x x))))
;:                   (s (lambda () (set! x (* x x x)))))
;;
;; The only possible value of x is 10⁶.


;; EXERCISE 3.41
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance)
             ((protected (lambda () balance))))
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

;; There is some merit to Ben's concerns, but the proposed solution is
;; misguided. The meritorious part is that external logic that relies on bank
;; balance information could potentially be tripped up by withdrawals or
;; deposits that occur during the execution of that logic. For example:
;;
;;   (define (withdraw-half account)
;;     (let ((current-balance (account 'balance)))
;;       ((account 'withdraw) (quotient current-balance 2))))
;;
;; Imagine that another withdrawal is in progress when we call withdraw-half,
;; but that it doesn't update the account balance until immediately after
;; current-balance is calculated. When our withdrawal executes, it will take
;; more than half the remaining balance since the balance is now smaller than
;; expected. If balance were serialized along with withdraw and deposit, this
;; problem would be avoided because the calculation of current-balance would be
;; forced to wait until the other withdrawal finished.
;;
;; However, other withdrawals and deposits can still interleave with any part of
;; our procedure that's not included in the serializer, so simply adding balance
;; to the serializer does not really protect us from this class of problem. For
;; example, if the external withdrawal took place after calculating
;; current-balance but before calculating our withdrawal amount (the quotient),
;; we'd again withdraw too much. Without adding additional concurrency control
;; to the system, there is no way around this issue.
;;
;; Considering also that the original purpose of the serializer in make-account
;; was to ensure that concurrent deposits and withdrawals cannot generate
;; inconsistent behavior (i.e., money being created or destroyed) -- a purpose
;; it succeeds at entirely -- there is no reason to modify it. Instead,
;; procedures that use accounts should be responsible for the implications of
;; their own concurrency, defining their own serializers or other mechanisms as
;; as necessary to ensure logical consistency in whatever sense that is
;; meaningful to them. In this case, for each account we wish to modify, we
;; could place withdraw-half in a lambda specialized for that account and ensure
;; that it and any other procedures that might access that account are
;; serialized together.
;;
;; (This answer assumes as an unalterable design choice that the responsibility
;; of managing concurrent balance accesses is kept internal to make-account. If
;; we were to reassign this responsibility to the users of the account object,
;; and accordingly expose the internal concurrency mechanism, we would have
;; some other options for how to solve issues like the example given. I have
;; avoided discussing this option because it is explored at length later, and
;; the exercise seems geared towards the present design.)


;; EXERCISE 3.42
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
	  (protected-deposit (protected deposit)))
      (define (dispatch m)
	(cond ((eq? m 'withdraw) protected-withdraw)
	      ((eq? m 'deposit) protected-deposit)
	      ((eq? m 'balance) balance)
	      (else (error "Unknown request -- MAKE-ACCOUNT"
			   m))))
      dispatch)))

;; The proposed change is safe, and does not change the behavior of the object
;; returned by make-account. In both versions, there is only one serializer,
;; protected. Any procedures that are added to it will be serialized with
;; respect to one other. Thus it does not matter whether dispatch returns a
;; fresh protected version of withdraw/deposit each time, or the same one. The
;; behavior of those methods has not changed and their concurrency restrictions
;; are the same.


;;;Multiple shared resources

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))


(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))


;; EXERCISE 3.43
;; The first part of the prompt can be generalized. We are to show that, given a
;; list of accounts, if we perform an arbitrary number of exchanges on those
;; accounts sequentially, the final balances will be equal to some ordering of
;; the starting balances. We know that if we only perform one exchange, this
;; condition holds true, since swapping any two balances is effectively just
;; changing the ordering. Thus, it follows inductively that the condition must
;; also hold for arbitrary numbers of exchanges, since it holds at each step.
;;
;; The stated condition does not hold if we revert to the original definition of
;; exchange. Below, we attempt to exchange the first and second accounts (proc.
;; 1) and the first and third accounts (proc. 2) concurrently. The final
;; balances ($40 $10 $10) are not a reordering of the starting balances ($10 $20
;; $30).
;;
;;   │   proc. 1         proc. 2         acc. a          acc. b          acc. c
;;   │
;;   │       ┌───────────────┬────────── $10             $20             $30
;;   │       ▼               ▼                            │               │
;;   │   Read a: 10      Read a: 10                       │               │
;;   │      │ ┌─────────────┼─────────────────────────────┘               │
;;   │      │ │             │ ┌───────────────────────────────────────────┘
;;   │      ▼ ▼             ▼ ▼
;;   │   Read b: 20      Read c: 30
;;   │       │               │
;;   │       ▼               ▼
;;   │   Calculate       Calculate
;;   │   diff.: -10      diff.: -20
;;   │       │               │
;;   │       ▼               │
;;   │   Withdraw ───────────┼─────────▶ $20
;;   │   from a: -10         ▼
;;   │       │           Withdraw ─────▶ $40
;;   │       │           from a: -20
;;   │       ▼               │
;;   │   Deposit ────────────┼─────────────────────────▶ $10
;;   │   into b: -10         ▼
;;   │                   Deposit ──────────────────────────────────────▶ $10
;;   V                   into c: -20
;; time
;;
;; The sum of the balances is still preserved in the above example because, even
;; though parts of the exchange operations are interleaved, the deposits and
;; withdrawals are still serialized. As money is conserved by deposits and
;; withdrawals, and all modification of balances takes place by means of
;; deposits and withdrawals, it follows that money is also conserved over any
;; other group of operations (such as exchanges) regardless of how those
;; operations interleave.
;;
;; If we remove the serialization of deposits and withdrawals, then interleaved
;; exchanges can result in interleaved deposits and withdrawals, leading to a
;; loss of conservation of money. Below, the total starting money
;; ($10 + $20 + $30 = $60) and final sum ($30 + $10 + $10 = $50) do not match.
;;
;;   │   proc. 1         proc. 2         acc. a          acc. b          acc. c
;;   │
;;   │       ┌───────────────┬────────── $10             $20             $30
;;   │       ▼               ▼                            │               │
;;   │   Read a: 10      Read a: 10                       │               │
;;   │      │ ┌─────────────┼─────────────────────────────┘               │
;;   │      │ │             │ ┌───────────────────────────────────────────┘
;;   │      ▼ ▼             ▼ ▼
;;   │   Read b: 20      Read c: 30
;;   │       │               │
;;   │       ▼               ▼
;;   │   Calculate       Calculate
;;   │   diff.: -10      diff.: -20
;;   │       │               │
;;   │       │               ▼
;;   │       │           Call
;;   │       │           (withdraw a -20)
;;   │       │  inside       │
;;   │       │  withdraw     │
;;   │       │     :  .....  ▼
;;   │       │     :  :  Read a: 10
;;   │       │     :  :      │
;;   │       │     :  :      ▼
;;   │       │     :..:  Calculate new
;;   │       │        :  balance: 30
;;   │       ▼        :      │
;;   │   Withdraw ────:──────┼─────────▶ $20
;;   │   from a: -10  :      ▼
;;   │       │        :  Set a: 30 ────▶ $30
;;   │       ▼        :....  │
;;   │   Deposit ────────────┼─────────────────────────▶ $10
;;   │   into b: -10         ▼
;;   │                   Deposit ──────────────────────────────────────▶ $10
;;   V                   into c: -20
;; time


;; EXERCISE 3.44
(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))


;; EXERCISE 3.45

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (deposit account amount)
 ((account 'deposit) amount))


;;;Implementing serializers

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

;;from footnote -- MIT Scheme
(define (test-and-set! cell)
  (without-interrupts
   (lambda ()
     (if (car cell)
         true
         (begin (set-car! cell true)
                false)))))

;;;SECTION 3.5

;;;SECTION 3.5.1

(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))


(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

;: (car (cdr (filter prime?
;:                   (enumerate-interval 10000 1000000))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))



;; stream-car and stream-cdr would normally be built into
;;  the stream implementation
;: (define (stream-car stream) (car stream))
;: (define (stream-cdr stream) (force (cdr stream)))

;: (stream-car
;:  (stream-cdr
;:   (stream-filter prime?
;:                  (stream-enumerate-interval 10000 1000000))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


;; force would normally be built into
;;  the stream implementation
;: (define (force delayed-object)
;:   (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))


;; EXERCISE 3.51

(define (show x)
  (display-line x)
  x)

;: (define x (stream-map show (stream-enumerate-interval 0 10)))
;: (stream-ref x 5)
;: (stream-ref x 7)


;; EXERCISE 3.52

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

;: (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;: (define y (stream-filter even? seq))
;: (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;:                          seq))

;: (stream-ref y 7)
;: (display-stream z)


;;;SECTION 3.5.2

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

;: (stream-ref no-sevens 100)

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

;: (stream-ref primes 50)


;;;Defining streams implicitly;;;Defining streams implicitly

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))


;; EXERCISE 3.53
;: (define s (cons-stream 1 (add-streams s s)))


;; EXERCISE 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))


;; EXERCISE 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))


;; EXERCISE 3.59
;: (define exp-series
;:   (cons-stream 1 (integrate-series exp-series)))


;;;SECTION 3.5.3

(define (sqrt-improve guess x)
  (average guess (/ x guess)))


(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

;: (display-stream (sqrt-stream 2))


(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

;: (define pi-stream
;:   (scale-stream (partial-sums (pi-summands 1)) 4))

;: (display-stream pi-stream)


(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;: (display-stream (euler-transform pi-stream))


(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

;: (display-stream (accelerated-sequence euler-transform
;:                                       pi-stream))


;; EXERCISE 3.63
(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))

;; EXERCISE 3.64
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))


;;; Infinite streams of pairs

;: (stream-filter (lambda (pair)
;:                  (prime? (+ (car pair) (cadr pair))))
;:                int-pairs)

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))


;: (pairs integers integers)


(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


;; EXERCISE 3.68

(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))


;;; Streams as signals

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)


;; EXERCISE 3.74

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

;: (define zero-crossings (make-zero-crossings sense-data 0))



;; EXERCISE 3.75

(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-value)
                 (make-zero-crossings (stream-cdr input-stream)
                                      avpt))))


;;;SECTION 3.5.4

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)


;: (stream-ref (solve (lambda (y) y) 1 0.001) 1000)


;; EXERCISE 3.77

(define (integral integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? integrand)
                   the-empty-stream
                   (integral (stream-cdr integrand)
                             (+ (* dt (stream-car integrand))
                                initial-value)
                             dt))))

;;;SECTION 3.5.5

;; same as in section 3.1.2
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))


(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))


;: (define cesaro-stream
;:   (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
;:                         random-numbers))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))


(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

;: (define pi
;:   (stream-map (lambda (p) (sqrt (/ 6 p)))
;:               (monte-carlo cesaro-stream 0 0)))


;; same as in section 3.1.3
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define (stream-withdraw balance amount-stream)
  (cons-stream
   balance
   (stream-withdraw (- balance (stream-car amount-stream))
                    (stream-cdr amount-stream))))
