;;;;CODE FROM CHAPTER 1 OF STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;; Examples from the book are commented out with ;: so that they
;;;  are easy to find and so that they will be omitted if you evaluate a
;;;  chunk of the file (programs with intervening examples) in Scheme.

;;; BEWARE: Although the whole file can be loaded into Scheme,
;;;  don't expect the programs to work if you do so.  For example,
;;;  the redefinition of + in exercise 1.9 wreaks havoc with the
;;;  last version of square defined here.


;;;SECTION 1.1.1

;; interpreter examples

;: 486

;: (+ 137 349)
;: (- 1000 334)
;: (* 5 99)
;: (/ 10 5)
;: (+ 2.7 10)

;: (+ 21 35 12 7)
;: (* 25 4 12)

;: (+ (* 3 5) (- 10 6))

;: (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))

;: (+ (* 3
;:       (+ (* 2 4)
;:          (+ 3 5)))
;:    (+ (- 10 7)
;:       6))


;;;SECTION 1.1.2

;: (define size 2)
;: size
;: (* 5 size)

;: (define pi 3.14159)
;: (define radius 10)
;: (* pi (* radius radius))
;: (define circumference (* 2 pi radius))
;: circumference


;;;SECTION 1.1.3

;: (* (+ 2 (* 4 6))
;:    (+ 3 5 7))


;;;SECTION 1.1.4

(define (square x) (* x x))

;: (square 21)
;: (square (+ 2 5))
;: (square (square 3))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;: (sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

;: (f 5)


;;;SECTION 1.1.5

;: (f 5)
;: (sum-of-squares (+ 5 1) (* 5 2))
;: (+ (square 6) (square 10))
;: (+ (* 6 6) (* 10 10))
;: (+ 36 100)

;: (f 5)
;: (sum-of-squares (+ 5 1) (* 5 2))
;: (+    (square (+ 5 1))      (square (* 5 2))  )
;: (+    (* (+ 5 1) (+ 5 1))   (* (* 5 2) (* 5 2)))
;: (+         (* 6 6)             (* 10 10))
;: (+           36                   100)
;:                     136


;;;SECTION 1.1.6

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

;: (and (> x 5) (< x 10))

(define (>= x y)
  (or (> x y) (= x y)))

(define (>= x y)
  (not (< x y)))


;;EXERCISE 1.1
;: 10
;10

;: (+ 5 3 4)
;12

;: (- 9 1)
;8

;: (/ 6 2)
;4

;: (+ (* 2 4) (- 4 6))
;6

;: (define a 3)
;a

;: (define b (+ a 1))
;b

;: (+ a b (* a b))
;19

;: (= a b)
;#f

;: (if (and (> b a) (< b (* a b)))
;:     b
;:     a)
;4

;: (cond ((= a 4) 6)
;:       ((= b 4) (+ 6 7 a))
;:       (else 25))
;16

;: (+ 2 (if (> b a) b a))
;6

;: (* (cond ((> a b) a)
;: 	 ((< a b) b)
;: 	 (else -1))
;:    (+ a 1))
;16

;;EXERCISE 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;;EXERCISE 1.3
(define (f x y z)
  (define (sum-squares a b) (+ (square a) (square b)))
  (if (>= x y)
      (if (>= y z)
          (sum-squares x y)
          (sum-squares x z))
      (sum-squares y z)))

;;EXERCISE 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; The body of the procedure consists of a single combination whose operator
; evaluates to + or - depending whether b is positive or nonpositive. The
; procedure is thus equivalent to (+ a b) for positive values of b and (- a b)
; for nonpositive values of b. Since adding a positive number or subtracting a
; nonpositive number are both equivalent to adding the number's absolute value,
; the body of the procedure is also equivalent to (+ a (abs b)).

;;EXERCISE 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;: (test 0 (p))

; With applicative-order evaluation: Evaluating the expression (test 0 (p))
; requires first to evaluate the operator and operands. The second operand (p)
; is a combination whose operator evaluates to itself due to the recursive
; definition (define (p) (p)). So evaluating the second operand never
; terminates, and evaluating (test 0 (p)) will cause the interpreter to hang
; indefinitely.
;
; With normal-order evaluation: The expression (test 0 (p)) expands to
; (if (= 0 0) 0 (p)) under evaluation. Since the predicate is true, the
; expression evaluates to the consequent branch automatically, and the
; alternative branch (p) need never be evaluated. Evaluating this expression
; yields 0.

;;;SECTION 1.1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))


;: (sqrt 9)
;: (sqrt (+ 100 37))
;: (sqrt (+ (sqrt 2) (sqrt 3)))
;: (square (sqrt 1000))


;;EXERCISE 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;: (new-if (= 2 3) 0 5)

;: (new-if (= 1 1) 0 5)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

; Using the new sqrt-iter procedure results in infinite recursion.
;
; Since new-if is an ordinary procedure, not a special form, it cannot be
; evaluated without first evaluating all its operands. Inside the new sqrt-iter,
; the alternative branch (third operand) to new-if consists of a recursive call
; to sqrt-iter which, for the reason just stated, will always be evaluated
; regardless of the predicate (first operand). Thus the evaluation of sqrt-iter
; results in an infinitely nested series of calls to sqrt-iter and new-if. Since
; none of these calls ever returns, we would expect evaluation to continue
; indefinitely until a stack overflow occurs. (In fact, in MIT/GNU Scheme
; running with --stack 1000, the error is "maximum recursion depth exceeded".)

;;EXERCISE 1.7
; We can use error as a percentage of the expected (correct) value as a measure
; of the effectiveness of our square root procedure. For example, for
; (sqrt 100), the expected value is 10. If our procedure returns 11, then the
; error is 10%.
;
; For very small radicands, the acceptable absolute error 0.001 will tend to be
; very high as a percentage of the expected value of the root, and so the
; returned values will tend to miss the mark by a high percentage as well. For
; example, for (sqrt 0.000009) the exact correct root is 0.003 but the
; calculated root is 0.03134584760656851, an error of more than 900%.
;
; For very large radicands, with a limited-precision floating-point
; representation, we can end up with a guess that is impossible to improve -- it
; may even be precisely the correct answer! -- yet never passes the good-enough?
; test, leading to infinite recursion.
;
; As an example, consider (sqrt 1e100). The expected value of the root is 1e50.
; Let's say that during evaluation of (sqrt 1e100), we arrive at 1e50 as a
; guess. We'd ideally like to stop here and return 1e50 as the answer. But
; observe that, due to lost precision, evaluating (square 1e50) in the REPL
; yields not 1e100 but 1.0000000000000002e100, which differs from the supplied
; radicand 1e100 by more than the cutoff value of 0.001. Thus, 1e50 doesn't pass
; the good-enough? test, and as the correct answer doesn't pass, we can see that
; any other answer we might arrive at will not pass either, and so our
; evaluation will continue to "improve" and test guesses indefinitely.

(define (sqrt-iter guess old-guess x)
  (if (good-enough? guess old-guess x)
      guess
      (sqrt-iter (improve guess x)
                 guess
                 x)))

(define (good-enough? guess old-guess x)
  (< (abs (/ (- guess old-guess) guess)) 1.0e-10))

(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))

; The revised square-root procedure above works better for both small and large
; numbers. It is more accurate overall, and for very large numbers, it returns a
; value in cases where the original procedure would have run indefinitely.

;;EXERCISE 1.8
(define (cbrt-iter guess old-guess x)
  (if (good-enough? guess old-guess x)
      guess
      (cbrt-iter (cbrt-improve guess x)
                 guess
                 x)))

(define (cbrt-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cbrt x)
  (cbrt-iter 1.0 0.0 x))

;;;SECTION 1.1.8

(define (square x) (* x x))

(define (square x)
  (exp (double (log x))))

(define (double x) (+ x x))


;; As in 1.1.7
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))


;; Block-structured
(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;; Taking advantage of lexical scoping
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

;;;SECTION 1.2.1

;; Recursive

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))


;; Iterative

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;; Iterative, block-structured (from footnote)
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))


;;EXERCISE 1.9
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

; The first procedure generates a recursive process.
;
; (+ 4 5)
; (if (= 0 4) 5 (inc (+ (dec 4) 5)))
; (inc (+ (dec 4) 5))
; (inc (+ 3 5))
; (inc (if (= 0 3) 5 (inc (+ (dec 3) 5))))
; (inc (inc (+ (dec 3) 5)))
; (inc (inc (+ 2 5)))
; (inc (inc (if (= 0 2) 5 (inc (+ (dec 2) 5)))))
; (inc (inc (inc (+ (dec 2) 5))))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (if (= 0 1) 5 (inc (+ (dec 1) 5))))))
; (inc (inc (inc (inc (+ (dec 1) 5)))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc (if (= 0 5) 5 (inc (+ (dec 0) 5)))))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

; The second procedure generates an iterative process.
;
; (+ 4 5)
; (if (= 4 0) 5 (+ (dec 4) (inc 5)))
; (+ (dec 4) (inc 5))
; (+ 3 6)
; (if (= 3 0) 6 (+ (dec 3) (inc 6)))
; (+ (dec 3) (inc 6))
; (+ 2 7)
; (if (= 2 0) 7 (+ (dec 2) (inc 7)))
; (+ (dec 2) (inc 7))
; (+ 1 8)
; (if (= 1 0) 8 (+ (dec 1) (inc 8)))
; (+ (dec 1) (inc 8))
; (+ 0 9)
; (if (= 0 0) 9 (+ (dec 0) (inc 9)))
; 9

;;EXERCISE 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;: (A 1 10)
;1024

;: (A 2 4)
;65536

;: (A 3 3)
;65536

(define (f n) (A 0 n))
;(f n) computes 2*n.

(define (g n) (A 1 n))
;(g n) computes 2^n.

(define (h n) (A 2 n))
;(h n) computes 2↑↑n.

; The last answer uses Knuth up-arrow notation. It is equivalent to saying that
; (h n) computes P(n) where
;
;          / 2          if n = 1
;  P(n) = {
;          \ 2^P(n-1)   otherwise.

(define (k n) (* 5 n n))


;;;SECTION 1.2.2

;; Recursive

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; Iterative

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))


;; Counting change

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

;: (count-change 100)

;CHALLENGE
; Here is a more efficient version without using memoization. It was inspired by
; the observation most kinds of coin are exact multiples of each other: one
; half-dollar equals two quarters, one quarter equals five nickels, and one
; nickel equals five pennies. One can use this information to build an efficient
; recursive algorithm for calculating change using only this limited subset of
; coins, then add in special handling for dimes.

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

;;EXERCISE 1.11
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

;;EXERCISE 1.12
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

;;EXERCISE 1.13
; Let
;
;   ϕ = (1 + √5)/2 and
;   ψ = (1 - √5)/2.
;
; Note that ϕ and ψ are positive and negative respectively, and that ψ² < 1 (the
; proofs are omitted for brevity).
;
; We will first prove that the following statement P(n) holds for all natural
; numbers n.
;
;   Fib(n) = (ϕⁿ - ψⁿ)/√5
;
; It is easy to show that P(0) holds:
;
;   Fib(0) = (((1 + √5)/2)⁰ - ((1 - √5)/2)⁰)/√5   by substitution
;   Fib(0) = 0
;   0      = 0                                    by the definition of Fib(n)
;
; And similarly P(1):
;
;   Fib(1) = (((1 + √5)/2)¹ - ((1 - √5)/2)¹)/√5   by substitution
;   Fib(1) = 1
;   1      = 1                                    by the definition of Fib(n)
;
; Now we will show that for some i, if P(i) holds and P(i-1) holds, then P(i+1)
; also holds.
;
;   Fib(i+1)                        = (ϕⁱ⁺¹ - ψⁱ⁺¹)/√5   P(i+1)
;   Fib(i)       + Fib(i-1)         = (ϕⁱ⁺¹ - ψⁱ⁺¹)/√5   by definition of Fib(n)
;   (ϕⁱ - ψⁱ)/√5 + (ϕⁱ⁻¹ - ψⁱ⁻¹)/√5 = (ϕⁱ⁺¹ - ψⁱ⁺¹)/√5   because P(i) and P(i-1)
;   ϕⁱ - ψⁱ + ϕⁱ⁻¹ - ψⁱ⁻¹           = ϕⁱ⁺¹ - ψⁱ⁺¹
;   ϕⁱ⁻¹ + ϕⁱ - ϕⁱ⁺¹                = ψⁱ⁻¹ + ψⁱ - ψⁱ⁺¹
;   ϕⁱ(ϕ⁻¹ + 1 - ϕ)                 = ψⁱ(ψ⁻¹ + 1 - ψ)    (reduction omitted for
;   ϕⁱ(0)                           = ψⁱ(0)              brevity)
;   0                               = 0
;
; To prove that Fib(n) is the closest integer to ϕⁿ/√5, we will show that
; |Fib(n) - ϕⁿ/√5| < 1/2 holds for all natural numbers n.
;
;   |Fib(n)       - ϕⁿ/√5| < 1/2
;   |(ϕⁿ - ψⁿ)/√5 - ϕⁿ/√5| < 1/2       because P(n)
;   |-ψⁿ|                  < √5/2
;   |-ψⁿ|²                 < (√5/2)²
;   (ψ²)ⁿ                  < 5/4
;   1ⁿ                     < 5/4      because ψ² < 1 and n ≥ 0
;   1                      < 5/4      QED

;;;SECTION 1.2.3

;;EXERCISE 1.14
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
          (else
            (let ((qty1 (begin (draw-child-prefix #t "")
                               (go amount
                                   (- kinds-of-coins 1)
                                   child-prefix
                                   #t))))
              (let ((qty2 (begin (draw-child-prefix #f "")
                                 (go (- amount
                                        (first-denomination kinds-of-coins))
                                     kinds-of-coins
                                     child-prefix
                                     #f))))
                       (+ qty1 qty2 1))))))
  (go amount 5 "" #f))

; The procedure above visualizes the process generated by count-change and
; returns the total number of recursive calls to cc. The space used by the
; process grows as Θ(n) and the number of steps as Θ(2ⁿ).
;
; (visualize-count-change 11)
;
; (cc 11 5)
;       ├─(cc 11 4)
;       │   ├─(cc 11 3)
;       │   │   ├─(cc 11 2)
;       │   │   │   ├─(cc 11 1)
;       │   │   │   │   ├─(cc 11 0)
;       │   │   │   │   │   └─0
;       │   │   │   │   └─(cc 10 1)
;       │   │   │   │       ├─(cc 10 0)
;       │   │   │   │       │   └─0
;       │   │   │   │       └─(cc 9 1)
;       │   │   │   │           ├─(cc 9 0)
;       │   │   │   │           │   └─0
;       │   │   │   │           └─(cc 8 1)
;       │   │   │   │               ├─(cc 8 0)
;       │   │   │   │               │   └─0
;       │   │   │   │               └─(cc 7 1)
;       │   │   │   │                   ├─(cc 7 0)
;       │   │   │   │                   │   └─0
;       │   │   │   │                   └─(cc 6 1)
;       │   │   │   │                       ├─(cc 6 0)
;       │   │   │   │                       │   └─0
;       │   │   │   │                       └─(cc 5 1)
;       │   │   │   │                           ├─(cc 5 0)
;       │   │   │   │                           │   └─0
;       │   │   │   │                           └─(cc 4 1)
;       │   │   │   │                               ├─(cc 4 0)
;       │   │   │   │                               │   └─0
;       │   │   │   │                               └─(cc 3 1)
;       │   │   │   │                                   ├─(cc 3 0)
;       │   │   │   │                                   │   └─0
;       │   │   │   │                                   └─(cc 2 1)
;       │   │   │   │                                       ├─(cc 2 0)
;       │   │   │   │                                       │   └─0
;       │   │   │   │                                       └─(cc 1 1)
;       │   │   │   │                                           ├─(cc 1 0)
;       │   │   │   │                                           │   └─0
;       │   │   │   │                                           └─(cc 0 1)
;       │   │   │   │                                               └─1
;       │   │   │   └─(cc 6 2)
;       │   │   │       ├─(cc 6 1)
;       │   │   │       │   ├─(cc 6 0)
;       │   │   │       │   │   └─0
;       │   │   │       │   └─(cc 5 1)
;       │   │   │       │       ├─(cc 5 0)
;       │   │   │       │       │   └─0
;       │   │   │       │       └─(cc 4 1)
;       │   │   │       │           ├─(cc 4 0)
;       │   │   │       │           │   └─0
;       │   │   │       │           └─(cc 3 1)
;       │   │   │       │               ├─(cc 3 0)
;       │   │   │       │               │   └─0
;       │   │   │       │               └─(cc 2 1)
;       │   │   │       │                   ├─(cc 2 0)
;       │   │   │       │                   │   └─0
;       │   │   │       │                   └─(cc 1 1)
;       │   │   │       │                       ├─(cc 1 0)
;       │   │   │       │                       │   └─0
;       │   │   │       │                       └─(cc 0 1)
;       │   │   │       │                           └─1
;       │   │   │       └─(cc 1 2)
;       │   │   │           ├─(cc 1 1)
;       │   │   │           │   ├─(cc 1 0)
;       │   │   │           │   │   └─0
;       │   │   │           │   └─(cc 0 1)
;       │   │   │           │       └─1
;       │   │   │           └─(cc -4 2)
;       │   │   │               └─0
;       │   │   └─(cc 1 3)
;       │   │       ├─(cc 1 2)
;       │   │       │   ├─(cc 1 1)
;       │   │       │   │   ├─(cc 1 0)
;       │   │       │   │   │   └─0
;       │   │       │   │   └─(cc 0 1)
;       │   │       │   │       └─1
;       │   │       │   └─(cc -4 2)
;       │   │       │       └─0
;       │   │       └─(cc -9 3)
;       │   │           └─0
;       │   └─(cc -14 4)
;       │       └─0
;       └─(cc -39 5)
;           └─0

;;EXERCISE 1.15
(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

; a. When evaluating (sine 12.15), p is applied five times.

; b. For (sine a), the order of growth in both space and # of steps is Θ(log a).

;;;SECTION 1.2.4

;; Linear recursion
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))


;; Linear iteration
(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product))))

;; Logarithmic iteration
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))


;;EXERCISE 1.16
(define (iterative-fast-expt b n)
  (define (go a b n)
    (cond ((= n 0) a)
          ((even? n) (go a (square b) (/ n 2)))
          (else (go (* a b) b (- n 1)))))
  (go 1 b n))

;;EXERCISE 1.17
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (*-recursive x y)
  (cond ((= y 0) 0)
        ((= y 1) x)
        ((even? y) (*-recursive (double x) (halve y)))
        (else (+ x (*-recursive x (- y 1))))))

;;EXERCISE 1.18
(define (*-iterative x y)
  (define (go acc x y)
    (cond ((= y 0) acc)
          ((even? y) (go acc (double x) (halve y)))
          (else (go (+ acc x) x (- y 1)))))
  (go 0 x y))

;;EXERCISE 1.19
; Given
;
;   Tpq(a,b) = (bq + aq + ap, bp + aq)
;
; we will show that Tpq(Tpq(a,b)) is equivalent to Tp'q'(a,b) where
;
;   p' = p² + q² and
;   q' = q² + 2pq.
;
; Tpq(Tpq(a,b)) = Tpq(bq + aq + ap, bp + aq)
;               = ((bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p,
;                  (bp + aq)p + (bq + aq + ap)q)
;               = (bpq + aq² + bq² + aq² + apq + bpq + apq + ap²,
;                  bp² + apq + bq² + aq² + apq)
;               = ((bq² + 2bpq) + (aq² + 2apq) + (ap² + aq²),
;                  (bp² + bq²) + (aq² + 2apq))
;               = (b(q² + 2pq) + a(q² + 2pq) + a(p² + q²),
;                  b(p² + q²) + a(q² + 2pq))
;               = (bq' + aq' + ap', bp' + aq')
;               = Tp'q'(a,b) QED.

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q)) ; compute p'
                   (+ (square q) (* 2 p q)) ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


;;;SECTION 1.2.5

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;EXERCISE 1.20
; (visualize-normal-order expr) generates a visualization of the normal-order
; evaluation of expr. The remainder procedure is abbreviated as r for brevity.
; Each step during which a remainder operation was actually performed is
; prefixed with an annotation, and the return value is the total number of
; remainder operations performed.

(define (visualize-normal-order expr)
  ; (eval e) returns a pair (e2 . r) where e2 is e evaluated one additional step
  ; and r is true iff a remainder operation was performed.
  (define (eval expr)
    (cond ((not (equal? (subst expr) expr))    (cons (subst expr) #f))
          ((or (boolean? expr) (number? expr)) (cons expr #f))
          ((eq? (car expr) 'if)                (eval-if expr))
          (else                                (eval-binop expr))))
  (define (eval-if expr)
    (let ((arg1 (cadr expr))
          (arg2 (caddr expr))
          (arg3 (cadddr expr)))
      (let ((pair1 (eval arg1)))
        (if (equal? (car pair1) arg1)
            (cons (if arg1 arg2 arg3) #f)
            (cons (list 'if (car pair1) arg2 arg3) (cdr pair1))))))
  (define (eval-binop expr)
    (let ((op (car expr))
          (arg1 (cadr expr))
          (arg2 (caddr expr)))
      (let ((pair1 (eval arg1))
            (pair2 (eval arg2)))
        (cond ((not (equal? (car pair1) arg1))
               (cons (list op (car pair1) arg2) (cdr pair1)))
              ((not (equal? (car pair2) arg2))
               (cons (list op arg1 (car pair2)) (cdr pair2)))
              ((eq? op '=) (cons (= arg1 arg2) #f))
              ((eq? op 'r) (cons (remainder arg1 arg2)
                                 (list 'remainder arg1 arg2)))
              (else (error "unknown operand" op))))))
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
    (let ((expr (car pair))
          (incr (if (cdr pair) 1 0)))
      (let ((pair2 (eval expr)))
        (if (not (equal? expr (car pair2)))
            (go (eval expr) (+ count incr))
            count))))
  (go (cons expr #f) 0))

; (count-remainder-ops a b) calculates the number of remainder operations that
; would be performed during applicative-order evaluation of (gcd a b).

(define (count-remainder-ops a b)
  (define (go a b count)
    (if (= b 0)
        count
        (go b (remainder a b) (+ count 1))))
  (go a b 0))

; The remainder operation is performed 17 times in normal-order evaluation, and
; only 4 times in applicative-order evaluation, as shown below.
;
; (visualize-normal-order '(gcd 206 40))
;
;   (gcd 206 40)
;
;   (if (= 40 0)
;       206
;       (gcd 40 (r 206 40)))
;
;   (if #f
;       206
;       (gcd 40 (r 206 40)))
;
;   (gcd 40 (r 206 40))
;
;   (if (= (r 206 40) 0)
;       40
;       (gcd (r 206 40) (r 40 (r 206 40))))
;
;   ; operation performed: (remainder 206 40)
;   (if (= 6 0)
;       40
;       (gcd (r 206 40) (r 40 (r 206 40))))
;
;   (if #f
;       40
;       (gcd (r 206 40) (r 40 (r 206 40))))
;
;   (gcd (r 206 40) (r 40 (r 206 40)))
;
;   (if (= (r 40 (r 206 40)) 0)
;       (r 206 40)
;       (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
;
;   ; operation performed: (remainder 206 40)
;   (if (= (r 40 6) 0)
;       (r 206 40)
;       (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
;
;   ; operation performed: (remainder 40 6)
;   (if (= 4 0)
;       (r 206 40)
;       (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
;
;   (if #f
;       (r 206 40)
;       (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
;
;   (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
;
;   (if (= (r (r 206 40) (r 40 (r 206 40))) 0)
;       (r 40 (r 206 40))
;       (gcd (r (r 206 40) (r 40 (r 206 40)))
;            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))
;
;   ; operation performed: (remainder 206 40)
;   (if (= (r 6 (r 40 (r 206 40))) 0)
;       (r 40 (r 206 40))
;       (gcd (r (r 206 40) (r 40 (r 206 40)))
;            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))
;
;   ; operation performed: (remainder 206 40)
;   (if (= (r 6 (r 40 6)) 0)
;       (r 40 (r 206 40))
;       (gcd (r (r 206 40) (r 40 (r 206 40)))
;            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))
;
;   ; operation performed: (remainder 40 6)
;   (if (= (r 6 4) 0)
;       (r 40 (r 206 40))
;       (gcd (r (r 206 40) (r 40 (r 206 40)))
;            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))
;
;   ; operation performed: (remainder 6 4)
;   (if (= 2 0)
;       (r 40 (r 206 40))
;       (gcd (r (r 206 40) (r 40 (r 206 40)))
;            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))
;
;   (if #f
;       (r 40 (r 206 40))
;       (gcd (r (r 206 40) (r 40 (r 206 40)))
;            (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))
;
;   (gcd (r (r 206 40) (r 40 (r 206 40)))
;        (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
;
;   (if (= (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))) 0)
;       (r (r 206 40) (r 40 (r 206 40)))
;       (gcd
;        (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
;        (r (r (r 206 40) (r 40 (r 206 40)))
;           (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
;
;   ; operation performed: (remainder 206 40)
;   (if (= (r (r 40 6) (r (r 206 40) (r 40 (r 206 40)))) 0)
;       (r (r 206 40) (r 40 (r 206 40)))
;       (gcd
;        (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
;        (r (r (r 206 40) (r 40 (r 206 40)))
;           (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
;
;   ; operation performed: (remainder 40 6)
;   (if (= (r 4 (r (r 206 40) (r 40 (r 206 40)))) 0)
;       (r (r 206 40) (r 40 (r 206 40)))
;       (gcd
;        (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
;        (r (r (r 206 40) (r 40 (r 206 40)))
;           (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
;
;   ; operation performed: (remainder 206 40)
;   (if (= (r 4 (r 6 (r 40 (r 206 40)))) 0)
;       (r (r 206 40) (r 40 (r 206 40)))
;       (gcd
;        (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
;        (r (r (r 206 40) (r 40 (r 206 40)))
;           (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
;
;   ; operation performed: (remainder 206 40)
;   (if (= (r 4 (r 6 (r 40 6))) 0)
;       (r (r 206 40) (r 40 (r 206 40)))
;       (gcd
;        (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
;        (r (r (r 206 40) (r 40 (r 206 40)))
;           (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
;
;   ; operation performed: (remainder 40 6)
;   (if (= (r 4 (r 6 4)) 0)
;       (r (r 206 40) (r 40 (r 206 40)))
;       (gcd
;        (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
;        (r (r (r 206 40) (r 40 (r 206 40)))
;           (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
;
;   ; operation performed: (remainder 6 4)
;   (if (= (r 4 2) 0)
;       (r (r 206 40) (r 40 (r 206 40)))
;       (gcd
;        (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
;        (r (r (r 206 40) (r 40 (r 206 40)))
;           (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
;
;   ; operation performed: (remainder 4 2)
;   (if (= 0 0)
;       (r (r 206 40) (r 40 (r 206 40)))
;       (gcd
;        (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
;        (r (r (r 206 40) (r 40 (r 206 40)))
;           (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
;
;   (if #t
;       (r (r 206 40) (r 40 (r 206 40)))
;       (gcd
;        (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
;        (r (r (r 206 40) (r 40 (r 206 40)))
;           (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
;
;   (r (r 206 40) (r 40 (r 206 40)))
;
;   ; operation performed: (remainder 206 40)
;   (r 6 (r 40 (r 206 40)))
;
;   ; operation performed: (remainder 206 40)
;   (r 6 (r 40 6))
;
;   ; operation performed: (remainder 40 6)
;   (r 6 4)
;
;   ; operation performed: (remainder 6 4)
;   2
;
; ;Value: 17
;
; (count-remainder-ops 206 40)
;
; ;Value: 4


;;;SECTION 1.2.6

;; prime?

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


;; fast-prime?

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


;;EXERCISE 1.21
;: (smallest-divisor 199)
; 199

;: (smallest-divisor 1999)
; 1999

;: (smallest-divisor 19999)
; 7

;;EXERCISE 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; This solution repeats each primality test `prime-test-reps` times. Repetition
; is necessary on a fast system where the resolution of timings reported by
; `runtime` is too coarse to permit meaningful comparison of small timings.
; Redefine `prime-test-reps` larger or smaller depending on your system.
(define prime-test-reps 10000)

; Set upper to `upper` for no upper bound. Search ends after `max-results`
; primes are discovered.
(define (search-for-primes lower upper max-results)
  (define next (+ lower (if (even? lower) 1 2)))
  (if (and (or (not upper) (<= lower upper))
           (> max-results 0))
      (if (timed-prime-test-with-reps lower (runtime) prime-test-reps)
          (search-for-primes next upper (- max-results 1))
          (search-for-primes next upper max-results))))

(define (timed-prime-test-with-reps n start-time reps)
  (define (iter reps)
    (if (prime? n)
        (begin (if (< reps 1)
                   (report-prime (- (runtime) start-time))
                   (iter (- reps 1)))
               true)
        false))
  (newline)
  (display n)
  (iter reps))

; (search-for-primes (expt 10 3) #f 3)
; primes: 1009, 1013, 1019; timings: 0.29s, 0.28s, 0.29s

; (search-for-primes (expt 10 4) #f 3)
; primes: 10007, 10009, 100037; timings: 0.92s, 0.93s, 0.93s

; (search-for-primes (expt 10 5) #f 3)
; primes: 100003, 100019, 100043; timings: 2.91s, 3.10s, 2.93s

; (search-for-primes (expt 10 6) #f 3)
; primes: 1000003, 1000033, 1000037; timings: 9.26s, 9.26s, 9.19s

; The timings for primes up to 1,000,000 reflect an order of growth of Θ(√n).
; For each tenfold increase in n, the runtime grows approximately √10 times.
; This behavior is consistent with an order of growth Θ(√n).

;;EXERCISE 1.23
(define (next x)
  (if (= x 2) 3 (+ x 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

;(search-for-primes (expt 10 3) #f 3) ; timings: 0.20s, 0.20s, 0.21s
;(search-for-primes (expt 10 4) #f 3) ; timings: 0.60s, 0.60s, 0.60s
;(search-for-primes (expt 10 5) #f 3) ; timings: 1.83s, 1.83s, 1.81s
;(search-for-primes (expt 10 6) #f 3) ; timings: 5.84s, 5.84s, 5.85s

; The new algorithm takes a bit more than 60% as long as the old algorithm,
; worse than the expected 50%. The discrepancy can be explained by the
; observation that, while the new approach eliminates half of the test steps, it
; also adds new computations within each step. Namely, it replaces a single
; addition (+ test-divisor 1) with a procedure call (next test-divisor) whose
; evaluation will include an addition, an equality check and conditional branch.
; Roughly speaking, we've replaced n steps of size z with (n/2) steps of size
; (6z/5):
;
;  original runtime = nz
;  revised runtime = (n/2)(6z/5) = 3nz/5 = (60/100)nz = 60% * original runtime


;;EXERCISE 1.24
(define (prime? n)
  (fast-prime? n 10))

;(search-for-primes (expt 10 3) #f 3) ; timings: 1.79s, 1.87s, 1.93s
;(search-for-primes (expt 10 4) #f 3) ; timings: 2.29s, 2.24s, 2.31s
;(search-for-primes (expt 10 5) #f 3) ; timings: 2.59s, 2.67s, 2.70s
;(search-for-primes (expt 10 6) #f 3) ; timings: 3.03s, 3.01s, 3.18s

; We would expect the Fermat test for primes near 1,000,000 to take double the
; time needed for primes near 1000 since log 1000000 = log 1000² = 2 log 1000.
; This expectation does not quite hold up on my machine: the increase is closer
; to 1.65x rather than the expected 2x. To understand the discrepancy, note that
; the calculation includes some constant-cost, non-logarithmic elements. Those
; elements must account for a non-trivial fraction of the processing time for
; n <= 1000000.
;
; Since we do not know what fraction of the reported timings is constant-cost
; and what fraction is logarithmic, it will be difficult to verify the order of
; growth empirically simply by comparing two absolute runtimes. We must instead
; look at the runtime deltas, which by virtue of being deltas will not be
; affected by constant costs. Considering the reported runtimes for n=10ⁱ,
; moving stepwise from i=3 to i=6, we find that the increase in runtime for each
; step is fairly consistent. Runtime increases about 0.4s for each tenfold
; increase in n. This observation confirms that the order of growth for the
; procedure is indeed logarithmic.

;;EXERCISE 1.25
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;;EXERCISE 1.26
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;;;SECTION 1.3

(define (cube x) (* x x x))

;;;SECTION 1.3.1

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


;; Using sum

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

;: (sum-cubes 1 10)


(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

;: (sum-integers 1 10)


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;: (* 8 (pi-sum 1 1000))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

;: (integral cube 0 1 0.01)

;: (integral cube 0 1 0.001)


;;EXERCISE 1.32
;: (accumulate combiner null-value term a next b)

;;;SECTION 1.3.2

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(define (plus4 x) (+ x 4))

(define plus4 (lambda (x) (+ x 4)))

;: ((lambda (x y z) (+ x y (square z))) 1 2 3)


;; Using let

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;: (+ (let ((x 3))
;:      (+ x (* x 10)))
;:    x)

;: (let ((x 3)
;:       (y (+ x 2)))
;:   (* x y))

(define (f x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))


;;EXERCISE 1.34
(define (f g)
  (g 2))

;: (f square)

;: (f (lambda (z) (* z (+ z 1))))


;;;SECTION 1.3.3

;; Half-interval method

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))


;: (half-interval-method sin 2.0 4.0)

;: (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
;:                       1.0
;:                       2.0)


;; Fixed points

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


;: (fixed-point cos 1.0)

;: (fixed-point (lambda (y) (+ (sin y) (cos y)))
;:              1.0)


(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))


;;EXERCISE 1.37
;: (cont-frac (lambda (i) 1.0)
;:            (lambda (i) 1.0)
;:            k)


;;;SECTION 1.3.4

(define (average-damp f)
  (lambda (x) (average x (f x))))

;: ((average-damp square) 10)

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))


;; Newton's method

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)


(define (cube x) (* x x x))

;: ((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))


;; Fixed point of transformed function

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))


;;EXERCISE 1.40
;: (newtons-method (cubic a b c) 1)


;;EXERCISE 1.41
;: (((double (double double)) inc) 5)


;;EXERCISE 1.42
;: ((compose square inc) 6)


;;EXERCISE 1.43
;: ((repeated square 2) 5)
