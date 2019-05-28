;; Exercise 1.1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

10
;10

(+ 5 3 4)
;12

(- 9 1)
;8

(/ 6 2)
;4

(+ (* 2 4) (- 4 6))
;6

(define a 3)
;nothing

(define b (+ a 1))
;nothing

(+ a b (* a b))
;19

(= a b)
;#f

(if (and (> b a) (< b (* a b)))
    b
    a)
;4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;16

(+ 2 (if (> b a) b a))
;6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;16

;; Exercise 1.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (f x y z)
  (define (sum-squares a b) (+ (square a) (square b)))
  (if (>= x y)
      (if (>= y z)
          (sum-squares x y)
          (sum-squares x z))
      (sum-squares y z)))

;; Exercise 1.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

#|
The body of the procedure consists of a single combination whose operator
evaluates to + or - depending whether b is positive or nonpositive. The
procedure is thus equivalent to (+ a b) for positive values of b and (- a b)
for nonpositive values of b. Since adding a positive number or subtracting a
nonpositive number are both equivalent to adding the number's absolute value,
the body of the procedure is also equivalent to (+ a (abs b)).
#|

;; Exercise 1.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

#|
With applicative-order evaluation: Evaluating the expression (test 0 (p))
requires first to evaluate the operator and operands. The second operand (p) is
a combination whose operator evaluates to itself due to the recursive definition
(define (p) (p)). So evaluating the second operand never terminates, and
evaluating (test 0 (p)) will cause the interpreter to hang indefinitely.

With normal-order evaluation: The expression (test 0 (p)) expands to
(if (= 0 0) 0 (p)) under evaluation. Since the predicate is true, the expression
evaluates to the consequent branch automatically, and the alternative branch (p)
need never be evaluated. Evaluating this expression yields 0.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Exercise 1.6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

#|
Using the new sqrt-iter procedure results in infinite recursion.

Since new-if is an ordinary procedure, not a special form, it cannot be
evaluated without first evaluating all its operands. Inside the new sqrt-iter,
the alternative branch (third operand) to new-if consists of a recursive call to
sqrt-iter which, for the reason just stated, will always be evaluated regardless
of the predicate (first operand). Thus the evaluation of sqrt-iter results in an
infinitely nested series of calls to sqrt-iter and new-if. Since none of these
calls ever returns, we would expect evaluation to continue indefinitely until a
stack overflow occurs. (In fact, in MIT/GNU Scheme running with --stack 1000,
the error is "maximum recursion depth exceeded".)
|#

;; Exercise 1.7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
We can use error as a percentage of the expected (correct) value as a measure
of the effectiveness of our square root procedure. For example, for (sqrt 100),
the expected value is 10. If our procedure returns 11, then the error is 10%.

For very small radicands, the acceptable absolute error 0.001 will tend to be
very high as a percentage of the expected value of the root, and so the returned
values will tend to miss the mark by a high percentage as well. For example, for
(sqrt 0.000009) the exact correct root is 0.003 but the calculated root is
0.03134584760656851, an error of more than 900%.

For very large radicands, with a limited-precision floating-point
representation, we can end up with a guess that is impossible to improve -- it
may even be precisely the correct answer! -- yet never passes the good-enough?
test, leading to infinite recursion.

As an example, consider (sqrt 1e100). The expected value of the root is 1e50.
Let's say that during evaluation of (sqrt 1e100), we arrive at 1e50 as a guess.
We'd ideally like to stop here and return 1e50 as the answer. But observe that,
due to lost precision, evaluating (square 1e50) in the REPL yields not 1e100
but 1.0000000000000002e100, which differs from the supplied radicand 1e100 by
more than the cutoff value of 0.001. Thus, 1e50 doesn't pass the good-enough?
test, and as the correct answer doesn't pass, we can see that any other answer
we might arrive at will not pass either, and so our evaluation will continue to
"improve" and test guesses indefinitely.
|#

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

#|
The revised square-root procedure above works better for both small and large
numbers. It is more accurate overall, and for very large numbers, it returns a
value in cases where the original procedure would have run indefinitely.
|#

;; Exercise 1.8 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
