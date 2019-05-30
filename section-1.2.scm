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
