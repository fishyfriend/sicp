;; Exercise 1.9 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The first procedure generates a recursive process.

(define (+ a b)
  (if (= 0 a)
      b
      (inc (+ (dec a) b))))

(+ 4 5)
(if (= 0 4) 5 (inc (+ (dec 4) 5)))
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc (if (= 0 3) 5 (inc (+ (dec 3) 5))))
(inc (inc (+ (dec 3) 5)))
(inc (inc (+ 2 5)))
(inc (inc (if (= 0 2) 5 (inc (+ (dec 2) 5)))))
(inc (inc (inc (+ (dec 2) 5))))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (if (= 0 1) 5 (inc (+ (dec 1) 5))))))
(inc (inc (inc (inc (+ (dec 1) 5)))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc (if (= 0 5) 5 (inc (+ (dec 0) 5)))))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

; The second procedure generates an iterative process.

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(+ 4 5)
(if (= 4 0) 5 (+ (dec 4) (inc 5)))
(+ (dec 4) (inc 5))
(+ 3 6)
(if (= 3 0) 6 (+ (dec 3) (inc 6)))
(+ (dec 3) (inc 6))
(+ 2 7)
(if (= 2 0) 7 (+ (dec 2) (inc 7)))
(+ (dec 2) (inc 7))
(+ 1 8)
(if (= 1 0) 8 (+ (dec 1) (inc 8)))
(+ (dec 1) (inc 8))
(+ 0 9)
(if (= 0 0) 9 (+ (dec 0) (inc 9)))
9

;; Exercise 1.10 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10) ;1024
(A 2 4) ;65536
(A 3 3) ;65536

(define (f n) (A 0 n)) ;(f n) computes 2*n.
(define (g n) (A 1 n)) ;(g n) computes 2^n.
(define (h n) (A 2 n)) ;(h n) computes 2↑↑n.

#| The last answer uses Knuth up-arrow notation. It is equivalent to saying that
(h n) computes P(n) where

          / 2          if n = 1
  P(n) = {
          \ 2^P(n-1)   otherwise. |#

;; Example: Counting change ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                      kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

#| Here is a more efficient version without using memoization. It was inspired
by the observation most kinds of coin are exact multiples of each other: one
half-dollar equals two quarters, one quarter equals five nickels, and one nickel
equals five pennies. One can use this information to build an efficient
recursive algorithm for calculating change using only this limited subset of
coins, then add in special handling for dimes. |#

(define (cc kind n-coins n-subcoins)
  (define n-unused-subcoins
    (- n-subcoins (* n-coins (if (= kind 5) 2 5))))
  (if (or (> kind 5) (< n-unused-subcoins 0))
      0
      (+ 1 (cc kind (+ n-coins 1) n-subcoins)
           (cc (+ kind (if (= kind 2) 2 1)) 1 n-coins)
           (cond ((= kind 2) (quotient n-coins 2))
                 ((and (= kind 4) (> n-coins 0))
                   (* (quotient n-unused-subcoins 2)
                      (+ 1 (quotient n-coins 2))))
                 (else 0)))))

(define (count-change amount)
  (if (< amount 0) 0 (+ 1 (cc 2 1 amount))))

;; Exercise 1.11 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (f-recursive n)
  (define (addend m)
    (* m (f-recursive (- n m))))
  (if (< n 3) n (+ (addend 1) (addend 2) (addend 3))))

(define (f-iterative n)
  (define (solve a b c)
    (+ (* 3 a) (* 2 b) c))
  (define (go a b c count)
    (if (= count 0) a (go b c (solve a b c) (- count 1))))
  (go 0 1 2 n))

;; Exercise 1.12 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pascal n)
  (define (f row col)
    (cond ((or (= col 0) (= col row)) 1)
          ((or (< col 0) (> col row)) 0)
          (else (+ (f (- row 1) (- col 1))
                   (f (- row 1) col)))))
  (define (g row col count)
    (cond ((> col row) (g (+ row 1) 0 count))
          ((= count 0) (f row col))
          (else (g row (+ col 1) (- count 1)))))
  (g 0 0 n))

;; Exercise 1.13 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| Let

  ϕ = (1 + √5)/2 and
  ψ = (1 - √5)/2.

Note that ϕ and ψ are positive and negative respectively, and that ψ² < 1 (the
proofs are omitted for brevity).

We will first prove that the following statement P(n) holds for all natural
numbers n.

  Fib(n) = (ϕⁿ - ψⁿ)/√5

It is easy to show that P(0) holds:

  Fib(0) = (((1 + √5)/2)⁰ - ((1 - √5)/2)⁰)/√5   by substitution
  Fib(0) = 0
  0      = 0                                    by the definition of Fib(n)

And similarly P(1):

  Fib(1) = (((1 + √5)/2)¹ - ((1 - √5)/2)¹)/√5   by substitution
  Fib(1) = 1
  1      = 1                                    by the definition of Fib(n)

Now we will show that for some i, if P(i) holds and P(i-1) holds, then P(i+1)
also holds.

  Fib(i+1)                        = (ϕⁱ⁺¹ - ψⁱ⁺¹)/√5   P(i+1)
  Fib(i)       + Fib(i-1)         = (ϕⁱ⁺¹ - ψⁱ⁺¹)/√5   by definition of Fib(n)
  (ϕⁱ - ψⁱ)/√5 + (ϕⁱ⁻¹ - ψⁱ⁻¹)/√5 = (ϕⁱ⁺¹ - ψⁱ⁺¹)/√5   because P(i) and P(i-1)
  ϕⁱ - ψⁱ + ϕⁱ⁻¹ - ψⁱ⁻¹           = ϕⁱ⁺¹ - ψⁱ⁺¹
  ϕⁱ⁻¹ + ϕⁱ - ϕⁱ⁺¹                = ψⁱ⁻¹ + ψⁱ - ψⁱ⁺¹
  ϕⁱ(ϕ⁻¹ + 1 - ϕ)                 = ψⁱ(ψ⁻¹ + 1 - ψ)    (reduction omitted for
  ϕⁱ(0)                           = ψⁱ(0)              brevity)
  0                               = 0

To prove that Fib(n) is the closest integer to ϕⁿ/√5, we will show that
|Fib(n) - ϕⁿ/√5| < 1/2 holds for all natural numbers n.

  |Fib(n)       - ϕⁿ/√5| < 1/2
  |(ϕⁿ - ψⁿ)/√5 - ϕⁿ/√5| < 1/2       because P(n)
  |-ψⁿ|                  < √5/2
  |-ψⁿ|²                 < (√5/2)²
  (ψ²)ⁿ                  < 5/4
  1ⁿ                     < 5/4      because ψ² < 1 and n ≥ 0
  1                      < 5/4      QED |#

;; Exercise 1.14 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (visualize-count-change amount)
  (define (go amount kinds-of-coins prefix tee?)
    (define child-prefix
      (string-append prefix (if tee? "  │ " "    ")))
    (define (draw-child-prefix tee? value)
      (display (string-append child-prefix (if tee? "  ├─" "  └─") value)))
    (display (list "cc" amount kinds-of-coins))
    (newline)
    (cond ((= amount 0)
           (begin (draw-child-prefix #f "1\n") 1))
          ((or (< amount 0) (= kinds-of-coins 0))
           (begin (draw-child-prefix #f "0\n") 1))
          (else (let* ((qty1 (begin (draw-child-prefix #t "")
                                    (go amount
                                        (- kinds-of-coins 1)
                                        child-prefix
                                        #t)))
                       (qty2 (begin (draw-child-prefix #f "")
                                    (go (- amount
                                           (first-denomination kinds-of-coins))
                                        kinds-of-coins
                                        child-prefix
                                        #f))))
                       (+ qty1 qty2 1))))
  (go amount 5 "" #f)))

#| The space used by the process grows as Θ(n) and the number of steps as Θ(2ⁿ).

(cc 11 5)
      ├─(cc 11 4)
      │   ├─(cc 11 3)
      │   │   ├─(cc 11 2)
      │   │   │   ├─(cc 11 1)
      │   │   │   │   ├─(cc 11 0)
      │   │   │   │   │   └─0
      │   │   │   │   └─(cc 10 1)
      │   │   │   │       ├─(cc 10 0)
      │   │   │   │       │   └─0
      │   │   │   │       └─(cc 9 1)
      │   │   │   │           ├─(cc 9 0)
      │   │   │   │           │   └─0
      │   │   │   │           └─(cc 8 1)
      │   │   │   │               ├─(cc 8 0)
      │   │   │   │               │   └─0
      │   │   │   │               └─(cc 7 1)
      │   │   │   │                   ├─(cc 7 0)
      │   │   │   │                   │   └─0
      │   │   │   │                   └─(cc 6 1)
      │   │   │   │                       ├─(cc 6 0)
      │   │   │   │                       │   └─0
      │   │   │   │                       └─(cc 5 1)
      │   │   │   │                           ├─(cc 5 0)
      │   │   │   │                           │   └─0
      │   │   │   │                           └─(cc 4 1)
      │   │   │   │                               ├─(cc 4 0)
      │   │   │   │                               │   └─0
      │   │   │   │                               └─(cc 3 1)
      │   │   │   │                                   ├─(cc 3 0)
      │   │   │   │                                   │   └─0
      │   │   │   │                                   └─(cc 2 1)
      │   │   │   │                                       ├─(cc 2 0)
      │   │   │   │                                       │   └─0
      │   │   │   │                                       └─(cc 1 1)
      │   │   │   │                                           ├─(cc 1 0)
      │   │   │   │                                           │   └─0
      │   │   │   │                                           └─(cc 0 1)
      │   │   │   │                                               └─1
      │   │   │   └─(cc 6 2)
      │   │   │       ├─(cc 6 1)
      │   │   │       │   ├─(cc 6 0)
      │   │   │       │   │   └─0
      │   │   │       │   └─(cc 5 1)
      │   │   │       │       ├─(cc 5 0)
      │   │   │       │       │   └─0
      │   │   │       │       └─(cc 4 1)
      │   │   │       │           ├─(cc 4 0)
      │   │   │       │           │   └─0
      │   │   │       │           └─(cc 3 1)
      │   │   │       │               ├─(cc 3 0)
      │   │   │       │               │   └─0
      │   │   │       │               └─(cc 2 1)
      │   │   │       │                   ├─(cc 2 0)
      │   │   │       │                   │   └─0
      │   │   │       │                   └─(cc 1 1)
      │   │   │       │                       ├─(cc 1 0)
      │   │   │       │                       │   └─0
      │   │   │       │                       └─(cc 0 1)
      │   │   │       │                           └─1
      │   │   │       └─(cc 1 2)
      │   │   │           ├─(cc 1 1)
      │   │   │           │   ├─(cc 1 0)
      │   │   │           │   │   └─0
      │   │   │           │   └─(cc 0 1)
      │   │   │           │       └─1
      │   │   │           └─(cc -4 2)
      │   │   │               └─0
      │   │   └─(cc 1 3)
      │   │       ├─(cc 1 2)
      │   │       │   ├─(cc 1 1)
      │   │       │   │   ├─(cc 1 0)
      │   │       │   │   │   └─0
      │   │       │   │   └─(cc 0 1)
      │   │       │   │       └─1
      │   │       │   └─(cc -4 2)
      │   │       │       └─0
      │   │       └─(cc -9 3)
      │   │           └─0
      │   └─(cc -14 4)
      │       └─0
      └─(cc -39 5)
          └─0 |#

;; Exercise 1.15 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cube x) (* x x x))

(define (p x) (begin (display ".") (- (* 3 x) (* 4 (cube x)))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

; a. When evaluating (sine 12.15), p is applied five times.
; b. For (sine a), the order of growth in both space and # of steps is Θ(log a).

;; Exercise 1.16 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (iterative-fast-expt b n)
  (define (go a b n)
    (cond ((= n 0) a)
          ((even? n) (go a (square b) (/ n 2)))
          (else (go (* a b) b (- n 1)))))
  (go 1 b n))

;; Exercise 1.17 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (*-recursive x y)
  (cond ((= y 0) 0)
        ((= y 1) x)
        ((even? y) (*-recursive (double x) (halve y)))
        (else (+ x (*-recursive x (- y 1))))))

;; Exercise 1.18 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (*-iterative x y)
  (define (go acc x y)
    (cond ((= y 0) acc)
          ((even? y) (go acc (double x) (halve y)))
          (else (go (+ acc x) x (- y 1)))))
  (go 0 x y))

;; Exercise 1.19 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| Given

  Tpq(a,b) = (bq + aq + ap, bp + aq)

we will show that Tpq(Tpq(a,b)) is equivalent to Tp'q'(a,b) where

  p' = p² + q² and
  q' = q² + 2pq.

Tpq(Tpq(a,b)) = Tpq(bq + aq + ap, bp + aq)
              = ((bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p,
                 (bp + aq)p + (bq + aq + ap)q)
              = (bpq + aq² + bq² + aq² + apq + bpq + apq + ap²,
                 bp² + apq + bq² + aq² + apq)
              = ((bq² + 2bpq) + (aq² + 2apq) + (ap² + aq²),
                 (bp² + bq²) + (aq² + 2apq))
              = (b(q² + 2pq) + a(q² + 2pq) + a(p² + q²),
                 b(p² + q²) + a(q² + 2pq))
              = (bq' + aq' + ap', bp' + aq')
              = Tp'q'(a,b) QED. |#

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;; Exercise 1.20 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

#| (visualize-normal-order expr) generates a visualization of the normal-order
evaluation of expr. The remainder procedure is abbreviated as r for brevity.
Each step during which a remainder operation was actually performed is prefixed
with an annotation, and the return value is the total number of remainder
operations performed. |#

(define (visualize-normal-order expr)
  ; (eval e) returns a pair (e2 . r) where e2 is e evaluated one additional step
  ; and r is true iff a remainder operation was performed.
  (define (eval expr)
    (cond ((not (equal? (subst expr) expr))    (cons (subst expr) #f))
          ((or (boolean? expr) (number? expr)) (cons expr #f))
          ((eq? (car expr) 'if)                (eval-if expr))
          (else                                (eval-binop expr))))
  (define (eval-if expr)
    (let* ((arg1 (cadr expr))
           (arg2 (caddr expr))
           (arg3 (cadddr expr))
           (pair1 (eval arg1)))
          (if (equal? (car pair1) arg1)
              (cons (if arg1 arg2 arg3) #f)
              (cons (list 'if (car pair1) arg2 arg3) (cdr pair1)))))
  (define (eval-binop expr)
    (let* ((op (car expr))
           (arg1 (cadr expr))
           (arg2 (caddr expr))
           (pair1 (eval arg1))
           (pair2 (eval arg2)))
          (cond ((not (equal? (car pair1) arg1))
                 (cons (list op (car pair1) arg2) (cdr pair1)))
                ((not (equal? (car pair2) arg2))
                 (cons (list op arg1 (car pair2)) (cdr pair2)))
                ((eq? op '=) (cons (= arg1 arg2) #f))
                ((eq? op 'r) (cons (remainder arg1 arg2)
                                   (list 'remainder arg1 arg2)))
                (else (error "unknown operand" op)))))
  (define (subst expr)
    (cond ((or (number? expr) (boolean? expr)) expr)
          ((eq? (car expr) 'gcd)
           (let ((a (cadr expr))
                 (b (caddr expr)))
                (list 'if (list '= b 0) a (list 'gcd b (list 'r a b)))))
          (else expr)))
  (define (show pair)
    (if (cdr pair)
        (begin (display "; operation performed: ")
               (display (cdr pair))
               (newline)))
    (pp (car pair) (current-output-port) #t)
    (newline))
  (define (go pair count)
    (show pair)
    (let* ((expr (car pair))
           (incr (if (cdr pair) 1 0))
           (pair2 (eval expr)))
          (if (not (equal? expr (car pair2)))
              (go (eval expr) (+ count incr))
              count)))
  (go (cons expr #f) 0))

#| (count-remainder-ops a b) calculates the number of remainder operations that
would be performed during applicative-order evaluation of (gcd a b). |#

(define (count-remainder-ops a b)
  (define (go a b count)
    (if (= b 0)
        count
        (go b (remainder a b) (+ count 1))))
  (go a b 0))

#| The remainder operation is performed 18 times in normal-order evaluation, and
only 4 times in applicative-order evaluation, as shown below. |#

(visualize-normal-order '(gcd 206 40))

    (gcd 206 40)

    (if (= 40 0)
        206
        (gcd 40 (r 206 40)))

    (if #f
        206
        (gcd 40 (r 206 40)))

    (gcd 40 (r 206 40))

    (if (= (r 206 40) 0)
        40
        (gcd (r 206 40) (r 40 (r 206 40))))

    ; operation performed: (remainder 206 40)
    (if (= 6 0)
        40
        (gcd (r 206 40) (r 40 (r 206 40))))

    (if #f
        40
        (gcd (r 206 40) (r 40 (r 206 40))))

    (gcd (r 206 40) (r 40 (r 206 40)))

    (if (= (r 40 (r 206 40)) 0)
        (r 206 40)
        (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))

    ; operation performed: (remainder 206 40)
    (if (= (r 40 6) 0)
        (r 206 40)
        (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))

    ; operation performed: (remainder 40 6)
    (if (= 4 0)
        (r 206 40)
        (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))

    (if #f
        (r 206 40)
        (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))

    (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))

    (if (= (r (r 206 40) (r 40 (r 206 40))) 0)
        (r 40 (r 206 40))
        (gcd (r (r 206 40) (r 40 (r 206 40)))
             (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))

    ; operation performed: (remainder 206 40)
    (if (= (r 6 (r 40 (r 206 40))) 0)
        (r 40 (r 206 40))
        (gcd (r (r 206 40) (r 40 (r 206 40)))
             (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))

    ; operation performed: (remainder 206 40)
    (if (= (r 6 (r 40 6)) 0)
        (r 40 (r 206 40))
        (gcd (r (r 206 40) (r 40 (r 206 40)))
             (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))

    ; operation performed: (remainder 40 6)
    (if (= (r 6 4) 0)
        (r 40 (r 206 40))
        (gcd (r (r 206 40) (r 40 (r 206 40)))
             (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))

    ; operation performed: (remainder 6 4)
    (if (= 2 0)
        (r 40 (r 206 40))
        (gcd (r (r 206 40) (r 40 (r 206 40)))
             (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))

    (if #f
        (r 40 (r 206 40))
        (gcd (r (r 206 40) (r 40 (r 206 40)))
             (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))

    (gcd (r (r 206 40) (r 40 (r 206 40)))
         (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))

    (if (= (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))) 0)
        (r (r 206 40) (r 40 (r 206 40)))
        (gcd
         (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
         (r (r (r 206 40) (r 40 (r 206 40)))
            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

    ; operation performed: (remainder 206 40)
    (if (= (r (r 40 6) (r (r 206 40) (r 40 (r 206 40)))) 0)
        (r (r 206 40) (r 40 (r 206 40)))
        (gcd
         (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
         (r (r (r 206 40) (r 40 (r 206 40)))
            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

    ; operation performed: (remainder 40 6)
    (if (= (r 4 (r (r 206 40) (r 40 (r 206 40)))) 0)
        (r (r 206 40) (r 40 (r 206 40)))
        (gcd
         (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
         (r (r (r 206 40) (r 40 (r 206 40)))
            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

    ; operation performed: (remainder 206 40)
    (if (= (r 4 (r 6 (r 40 (r 206 40)))) 0)
        (r (r 206 40) (r 40 (r 206 40)))
        (gcd
         (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
         (r (r (r 206 40) (r 40 (r 206 40)))
            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

    ; operation performed: (remainder 206 40)
    (if (= (r 4 (r 6 (r 40 6))) 0)
        (r (r 206 40) (r 40 (r 206 40)))
        (gcd
         (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
         (r (r (r 206 40) (r 40 (r 206 40)))
            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

    ; operation performed: (remainder 40 6)
    (if (= (r 4 (r 6 4)) 0)
        (r (r 206 40) (r 40 (r 206 40)))
        (gcd
         (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
         (r (r (r 206 40) (r 40 (r 206 40)))
            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

    ; operation performed: (remainder 6 4)
    (if (= (r 4 2) 0)
        (r (r 206 40) (r 40 (r 206 40)))
        (gcd
         (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
         (r (r (r 206 40) (r 40 (r 206 40)))
            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

    ; operation performed: (remainder 4 2)
    (if (= 0 0)
        (r (r 206 40) (r 40 (r 206 40)))
        (gcd
         (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
         (r (r (r 206 40) (r 40 (r 206 40)))
            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

    (if #t
        (r (r 206 40) (r 40 (r 206 40)))
        (gcd
         (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
         (r (r (r 206 40) (r 40 (r 206 40)))
            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

    (r (r 206 40) (r 40 (r 206 40)))

    ; operation performed: (remainder 206 40)
    (r 6 (r 40 (r 206 40)))

    ; operation performed: (remainder 206 40)
    (r 6 (r 40 6))

    ; operation performed: (remainder 40 6)
    (r 6 4)

    ; operation performed: (remainder 6 4)
    2

    ;Value: 17

(count-remainder-ops 206 40)

    ;Value: 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; Exercise 1.21 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smallest-divisor 199)   ; => 199
(smallest-divisor 1999)  ; => 1999
(smallest-divisor 19999) ; => 7

;; Exercise 1.22 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (begin (report-prime (- (runtime) start-time)) #t)
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; Set upper to -1 to remove the upper bound
(define (search-for-primes lower upper max-results)
  (define (go n bound results)
    (if (and (or (= bound -1)
                 (not (> n bound)))
             (< results max-results))
        (go (+ n 2)
            bound
            (+ results
               (if (timed-prime-test n)
                   1
                   0)))))
  (go (if (even? lower)
          (+ lower 1)
          lower)
      upper
      0))

(define (start-prime-test n start-time)
  (define (repeat times)
          (if (> times 0)
              (begin (prime? n)
                     (repeat (- times 1)))))
  (if (prime? n)
      (begin (repeat prime-test-n-reps)
             (report-prime (- (runtime) start-time))
             #t)
      #f))

(define prime-test-n-reps 9999)

#| The timings for primes up to 10^6 reflect an order of growth of Θ(√n). For
each tenfold increase in n, the runtime grows approximately √10 times. This
behavior is consistent with an order of growth Θ(√n).

This conclusion was reached after modifying the test code to account for the
high speed of this particular test system. More specifically, the procedure
start-prime-test has been replaced with a version that repeats its primality
test an additional 9999 times.

The modification was necessary because without it, the runtimes for n in this
range were too short to be useful for reasoning about the order of growth. The
runtime procedure reports in 0.01s increments, and the reported runtimes for
primes up to 10^6 were all essentially zero. Thus, even if our prime test were
exhibiting Θ(√n) growth, the growth in reported timings would be flat (implying,
incorrectly, an order of growth Θ(k) for some constant k). |#

(search-for-primes (expt 10 3) -1 3)
; primes: 1009, 1013, 1019; timings: 0.29s, 0.28s, 0.29s

(search-for-primes (expt 10 4) -1 3)
; primes: 10007, 10009, 100037; timings: 0.92s, 0.93s, 0.93s

(search-for-primes (expt 10 5) -1 3)
; primes: 100003, 100019, 100043; timings: 2.91s, 3.10s, 2.93s

(search-for-primes (expt 10 6) -1 3)
; primes: 1000003, 1000033, 1000037; timings: 9.26s, 9.26s, 9.19s

;; Exercise 1.23 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (next x)
  (if (= x 2) 3 (+ x 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(search-for-primes (expt 10 3) -1 3) ; 0.20s, 0.20s, 0.21s
(search-for-primes (expt 10 4) -1 3) ; 0.60s, 0.60s, 0.60s
(search-for-primes (expt 10 5) -1 3) ; 1.83s, 1.83s, 1.81s
(search-for-primes (expt 10 6) -1 3) ; 5.84s, 5.84s, 5.85s

#| The new algorithm runs takes a bit more than 60% as long as the old
algorithm, worse than the expected 50%. The discrepancy can be explained by the
observation that, while the new approach eliminates half of the test steps, it
also adds new computations within each step. Namely, it replaces a single
addition (+ test-divisor 1) with a procedure call (next test-divisor) whose
evaluation will include an addition, an equality check and conditional branch.
Roughly speaking, we've replaced n steps of size z with (n/2) steps of size
(6z/5):

  original runtime = nz
  revised runtime = (n/2)(6z/5) = 3nz/5 = (60/100)nz = 60% * original runtime |#

;; Exercise 1.24 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (prime? n)
  (fast-prime? n 100))

(search-for-primes (expt 10 3) -1 3) ; 1.79s, 1.87s, 1.93s
(search-for-primes (expt 10 4) -1 3) ; 2.29s, 2.24s, 2.31s
(search-for-primes (expt 10 5) -1 3) ; 2.59s, 2.67s, 2.70s
(search-for-primes (expt 10 6) -1 3) ; 3.03s, 3.01s, 3.18s

#| We would expect the Fermat test for primes near 1,000,000 to take double the
time needed for primes near 1000 since log 1000000 = log 1000² = 2 log 1000.
This expectation does not quite hold up empirically on my machine: the increase
in running time from n=1000 to n=1000000 is closer to 1.65x rather than the
expected 2x. To understand the discrepancy, note that the calculation includes
some constant-cost, non-logarithmic elements. We can assume that those elements
account for a non-trivial fraction of the processing time for n <= 1000000 as
there is no other good explanation. Now, since we don't know exactly what
portion of the reported timings is constant-cost and what portion is
logarithmic, it will be difficult to verify the order of growth empirically by
simply comparing two absolute runtimes. We must instead look at the runtime
deltas, which by virtue of being deltas will not be affected by constant costs.
Considering the reported runtimes for n=10^x, moving stepwise from x=3 to x=6,
we find that the increase in runtime for each step is fairly consistent.
Runtime increases about 0.4s for each tenfold increase in n. This observation
supports that the order of growth for the procedure is in fact logarithmic. |#

;; Exercise 1.25 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

#|
This procedure is not a viable substitute for expmod. The Fermat test algorithm
requires calculating a remainder from intermediate results at each recursive
step, whereas the proposed revision takes a remainder at only one level of
recursion.
|#

;; Exercise 1.26 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

#|
Observe that:

  (a) The recursion depth is logarithmic in n since the exp argument is divided
      by 2 on each evaluation of the (even? exp) branch. If the recursion depth
      is d then the number of evaluations of the (even? exp) branch will grow as
      log(n).
  (b) Since we now make two recursive calls in the (even? exp) branch, the
      running time of the outermost expmod will double for each evaluation of
      the branch.

Putting (a) and (b) together, we get an order of growth Θ(2^(log n)) which is
equivalent to Θ(2^(log2 n)), which reduces to Θ(n).
|#
