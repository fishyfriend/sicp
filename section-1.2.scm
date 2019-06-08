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

;; Exercise 1.19 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Fib(n) = Tⁿ
T = T₀₁
Fib(n) = T₀₁ⁿ

Tpq(a,b) = (bq + aq + ap, bp + aq)
         = ((a + b)q + ap, aq + bp)

Tpq(Tpq(a,b)) = Tpq((a + b)q + ap, aq + bp)
              = ((((a + b)q + ap) + (aq + bp))q + ((a + b)q + ap)p,
                 ((a + b)q + ap)q + (aq + bp)p)
              = (aq² + bq² + apq + aq² + bpq + apq + bpq + ap²,
                 aq² + bq² + apq + apq + bp²)
              = ((aq² + 2apq) + (bq² + 2bpq) + (ap² + aq²),
                 (aq² + 2apq) + (bp² + bq²))
              = ((q² + 2pq)(a + b) + (p² + q²)a, (q² + 2pq)a + (p² + q²)b)
              = Tp'q' where p' = p² + q² and q' = q² + 2pq
|#

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

(define r remainder)

#|
    Asterisks denote remainder operations that are actually performed.

    (gcd 206 40)
    (if (= 40 0) 206 (gcd 40 (r 206 40)))
    (if #f 206 (gcd 40 (r 206 40)))
    (gcd 40 (r 206 40))
    (if (= (r 206 40) 0) 40 (gcd (r 206 40) (r 40 (r 206 40))))
   *(if (= 6 0) 40 (gcd (r 206 40) (r 40 (r 206 40))))
    (if #f 40 (gcd (r 206 40) (r 40 (r 206 40))))
    (gcd (r 206 40) (r 40 (r 206 40)))
    (if (= (r 40 (r 206 40)) 0)
        (r 206 40)
        (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
   *(if (= (r 40 6) 0)
        (r 206 40)
        (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
   *(if (= 4 0)
        (r 206 40)
        (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
    (if #f (r 206 40) (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
    (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
    (if (= (r (r 206 40) (r 40 (r 206 40))) 0)
        (r 40 (r 206 40))
        (gcd (r (r 206 40) (r 40 (r 206 40)))
             (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))
  **(if (= (r 6 (r 40 6)) 0)
        (r 40 (r 206 40))
        (gcd (r (r 206 40) (r 40 (r 206 40)))
             (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))
   *(if (= (r 6 4) 0)
        (r 40 (r 206 40))
        (gcd (r (r 206 40) (r 40 (r 206 40)))
             (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))
   *(if (= 2 0)
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
        (gcd (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
             (r (r (r 206 40) (r 40 (r 206 40)))
                (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
 ***(if (= (r (r 40 6) (r 6 (r 40 6))) 0)
        (r (r 206 40) (r 40 (r 206 40)))
        (gcd (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
             (r (r (r 206 40) (r 40 (r 206 40)))
                (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
  **(if (= (r 4 (r 6 4)) 0)
        (r (r 206 40) (r 40 (r 206 40)))
        (gcd (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
             (r (r (r 206 40) (r 40 (r 206 40)))
                (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
   *(if (= (r 4 2) 0)
        (r (r 206 40) (r 40 (r 206 40)))
        (gcd (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
             (r (r (r 206 40) (r 40 (r 206 40)))
                (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
   *(if (= 0 0)
        (r (r 206 40) (r 40 (r 206 40)))
        (gcd (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
             (r (r (r 206 40) (r 40 (r 206 40)))
                (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
    (if #t
        (r (r 206 40) (r 40 (r 206 40)))
        (gcd (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
             (r (r (r 206 40) (r 40 (r 206 40)))
                (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
  **(r 6 (r 40 6))
   *(r 6 4)
    2

In the normal-order evaluation, as shown above, the remainder operation is
performed 17 times. In the applicative-order evaluation, it is performed only 4
times ((r 206 40), (r 40 6), (r 6 4), and (r 4 2)).

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

#|
(smallest-divisor 199)   ; => 199
(smallest-divisor 1999)  ; => 1999
(smallest-divisor 19999) ; => 7
|#

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

#|
(search-for-primes (expt 10 3) -1 3)
; primes: 1009, 1013, 1019; times to test: 0.00s, 0.00s, 0.00s

(search-for-primes (expt 10 4) -1 3)
; primes: 10007, 10009, 100037; times to test: 0.00s, 0.00s, 0.00s

(search-for-primes (expt 10 5) -1 3)
; primes: 100003, 100019, 100043; times to test: 0.01s, 0.00s, 0.00s

(search-for-primes (expt 10 6) -1 3)
; primes: 1000003, 1000033, 1000037; times to test: 0.00s, 0.01s, 0.00s
|#

#|
The timings for primes up to 10^6 do not reflect an order of growth of Θ(√n).
The reported timings are all 0.00s or 0.01s and do not follow a clear pattern.
The timings are apparently too short to show noticeable growth at this scale.

To see evidence of the order of growth we must use larger inputs, which have
longer runtimes that can be meaningfully reported. Above n > 10^9, each tenfold
increase in the value of n grows the timing by a factor of about √10, which
is indeed consistent with an order of growth of Θ(√n).
|#

#|
(search-for-primes (expt 10 7) -1 3) ; 0.01s, 0.00s, 0.01s
(search-for-primes (expt 10 8) -1 3) ; 0.01s, 0.01s, 0.01s
(search-for-primes (expt 10 9) -1 3) ; 0.04s, 0.03s, 0.04s
(search-for-primes (expt 10 10) -1 3) ; 0.10s, 0.09s, 0.09s
(search-for-primes (expt 10 11) -1 3) ; 0.30s, 0.31s, 0.31s
(search-for-primes (expt 10 12) -1 3) ; 1.01s, 0.97s, 0.95s
(search-for-primes (expt 10 13) -1 3) ; 3.02s, 3.05s, 3.11s
(search-for-primes (expt 10 14) -1 3) ; 9.72s, 9.70s, 9.67s
(search-for-primes (expt 10 15) -1 3) ; 30.67s, 31.07s, 31.35s
|#

;; Exercise 1.23 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (next x)
  (if (= x 2) 3 (+ x 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

#|
(search-for-primes (expt 10 10) -1 3) ; 0.06s, 0.06s, 0.06s
(search-for-primes (expt 10 11) -1 3) ; 0.19s, 0.19s, 0.18s
(search-for-primes (expt 10 12) -1 3) ; 0.56s, 0.56s, 0.58s
(search-for-primes (expt 10 13) -1 3) ; 1.85s, 1.86s, 1.86s
|#

#|
The new algorithm runs around in around 60% the time as the old algorithm, worse
than the expected 50%. The discrepancy could be explained by the observation
that, while the new approach eliminates half of the test steps, it also adds
new computations within each step. Namely, it replaces a single addition
(+ test-divisor 1) with a procedure call (next test-divisor) whose evaluation
will include an addition, an equality check and conditional branch. Roughly,
we've replaced n steps of size z with (n/2) steps of size (6z/5):

  original runtime = nz
  revised runtime = (n/2)(6z/5) = 3nz/5 = (60/100)nz = 60% * original runtime

|#

;; Exercise 1.24 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (prime? n)
  (fast-prime? n 1000))

#|
(search-for-primes (expt 10 3) -1 3) ; 0.02s, 0.02s, 0.02s
(search-for-primes (expt 10 6) -1 3) ; 0.03s, 0.03s, 0.03s
(search-for-primes (expt 10 10) -1 3) ; 0.06s, 0.07s, 0.06s
(search-for-primes (expt 10 20) -1 3) ; 0.13s, 0.13s, 0.13s
(search-for-primes (expt 10 40) -1 3) ; 0.30s, 0.31s, 0.30s
(search-for-primes (expt 10 80) -1 3) ; 0.78s, 0.82s, 0.78s
(search-for-primes (expt 10 160) -1 3) ; 2.22s, 2.24s, 2.26s
(search-for-primes (expt 10 320) -1 3) ; 9.52s, 9.75s, 9.81s
(search-for-primes (expt 10 640) -1 3) ; 53.41s, 53.09s, 53.25s
|#

#|
We would expect the Fermat test for primes near 1000000 to take double the time
needed for primes near 1000 since log 1000000 = log 1000² = 2 log 1000. This
does not quite hold up empirically on my machine: the increase in running time
for n=1000000 is 1.5x rather than the expected 2x. It is likely that the
non-logarithmic, constant-cost portion of the calculation accounts for most
of the running time at this scale and so partially masks the logarithmic growth
from n=1000 to n=1000000.

Continuing on to higher values of n, the situation is different: squaring n
grows the runtime by a factor that is near 2x. Although the factor generally
increases with n, the increase is small, and so this observation roughly
confirms the predicted logarithmic order of growth for n < 10^160.

At very high n (> 10^160) the growth factor from squaring n explodes to 3x or
even greater. Since we know that the number of recursive calls is logarithmic,
something else must account for the added running time in this range. The only
available explanation appears to be that some operations on very large numbers
-- such as +, remainder, the random procedure, or perhaps even operations on the
callstack -- exhibit worse-than-logarithmic performance.

Given the above, we cannot consider this implementation of the Fermat test to
have a logarithmic order of growth in the strictest sense. But for practical
purposes, we could say it exhibits logarithmic growth for n < 10^160 and
worse-than-logarithmic performance otherwise. Further testing and reasoning
would be needed to uncover the exact cause of the discrepancy.
|#

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
