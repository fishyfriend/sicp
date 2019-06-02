;; Exercise 1.9 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (+ a b)
  (if (= 0 a)
      b
      (inc (+ (dec a) b))))

#|
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
|#

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

#|
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
|#

#|
The first procedure generates a recursive process; the second, an iterative
process.
|#

;; Exercise 1.10 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
;1024

(A 2 4)
;65536

(A 3 3)
;65536

(define (f n) (A 0 n))
;(f n) computes 2*n.

(define (g n) (A 1 n))
;(g n) computes 2^n.

(define (h n) (A 2 n))
#|
(h n) computes P(n) where P(n) = 2 if n = 1, and 2^P(n-1) otherwise.
More concise answer using Knuth up-arrow notation: (h n) computes 2↑↑n.
|#

;; Example: Counting change ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

#|
Here is a more efficient version without using memoization. It was inspired by
the observation most kinds of coin are exact multiples of each other: one
half-dollar equals two quarters, one quarter equals five nickels, and one nickel
equals five pennies. One can use this information to build an efficient
recursive algorithm for calculating change using only this limited subset of
coins, then add in special handling for dimes.
|#

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

#|

Let:

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

To prove the general statement, it will suffice to show that for some i,
if P(i) holds and P(i-1) holds, then P(i+1) also holds.

  Fib(i+1)                        = (ϕⁱ⁺¹ - ψⁱ⁺¹)/√5
  Fib(i)       + Fib(i-1)         = (ϕⁱ⁺¹ - ψⁱ⁺¹)/√5   by definition of Fib(n)
  (ϕⁱ - ψⁱ)/√5 + (ϕⁱ⁻¹ - ψⁱ⁻¹)/√5 = (ϕⁱ⁺¹ - ψⁱ⁺¹)/√5   because P(i) and P(i-1)
  ϕⁱ - ψⁱ + ϕⁱ⁻¹ - ψⁱ⁻¹           = ϕⁱ⁺¹ - ψⁱ⁺¹
  ϕⁱ⁻¹ + ϕⁱ - ϕⁱ⁺¹                = ψⁱ⁻¹ + ψⁱ - ψⁱ⁺¹
  ϕⁱ(ϕ⁻¹ + 1 - ϕ)                 = ψⁱ(ψ⁻¹ + 1 - ψ)
  ϕⁱ(0)                           = ψⁱ(0)              (reduction omitted)
  0                               = 0

Now to prove that Fib(n) is the closest integer to ϕⁿ/√5, it will suffice to
show that | Fib(n) - ϕⁿ/√5 | < 1/2 holds for all natural numbers n.

  | Fib(n)       - ϕⁿ/√5 | < 1/2
  | (ϕⁿ - ψⁿ)/√5 - ϕⁿ/√5 | < 1/2       because P above
  | -ψⁿ |                  < √5/2
  | -ψⁿ |²                 < (√5/2)²
  (ψ²)ⁿ                    < 5/4
  1ⁿ                       < 5/4       because ψ² < 1 and n ≥ 0
  1                        < 5/4       QED

|#

;; Exercise 1.14 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

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
          └─0

The space used by the process grows as Θ(n) and the number of steps as Θ(2ⁿ).

Below is an adaptation of the original change-counting procedure. Instead of
counting ways to make change, it calculates the number of recursive calls
required to generate the result, while printing out a tree diagram of the
computational process. This is used to generate the diagram above.

|#

(define (visualize-count-change amount)
  (vis-cc amount 5 "" #f))

(define (vis-cc amount kinds-of-coins prefix tee?)
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
                                  (vis-cc amount
                                          (- kinds-of-coins 1)
                                          child-prefix
                                          #t)))
                     (qty2 (begin (draw-child-prefix #f "")
                                  (vis-cc (- amount
                                             (first-denomination kinds-of-coins))
                                          kinds-of-coins
                                          child-prefix
                                          #f))))
                     (+ qty1 qty2 1)))))

;; Exercise 1.15 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cube x) (* x x x))

(define (p x) (begin (display ".") (- (* 3 x) (* 4 (cube x)))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

#|
a. When evaluating (sine 12.15), p is applied five times.
b. For (sine a), the order of growth in both space and # of steps is Θ(log a).
|#

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
