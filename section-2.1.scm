;; Exercise 2.1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-rat n d)
  (let ((sign (if (= (positive? n) (positive? d)) 1 -1)))
    (cons (* sign (abs n)) d)))

;; Exercise 2.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment seg)
  (midpoint (start-segment seg) (end-segment seg)))

(define (midpoint p1 p2)
  (make-point (/ (+ (x-point p1) (x-point p2)) 2)
              (/ (+ (y-point p1) (y-point p2)) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(let ((p1 (make-point 3 -5))
      (p2 (make-point -5 9)))
  (print-point (midpoint-segment (make-segment p1 p2))))
; (-1,2)

;; Exercise 2.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; first representation

(define (make-rect corner opposite-corner)
  (let ((left   (min (x-point corner) (x-point opposite-corner)))
        (right  (max (x-point corner) (x-point opposite-corner)))
        (bottom (min (y-point corner) (y-point opposite-corner)))
        (top    (max (y-point corner) (y-point opposite-corner))))
    (cons (cons left right) (cons bottom top))))

(define (left rect)   (car (car rect)))
(define (right rect)  (cdr (car rect)))
(define (bottom rect) (car (cdr rect)))
(define (top rect)    (cdr (cdr rect)))

(define (width rect)  (- (right rect) (left rect)))
(define (height rect) (- (top rect) (bottom rect)))

; alternative representation

(define (make-rect bottom-left width height)
  (cons bottom-left (cons width height)))

(define (bottom-left rect) (car rect))
(define (width rect)       (car (cdr rect)))
(define (height rect)      (cdr (cdr rect)))

; higher-level procedures that work using either representation

(define (perimeter rect) (* 2 (+ (width rect) (height rect))))
(define (area rect)      (* (width rect) (height rect)))

;; Exercise 2.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

#| Here is the series of substitutions performed during evaluation of
(car (cons x y)), showing that this expression evaluates to x for any objects x
and y.

  (car (cons x y))
  (car (lambda (m) (m x y)))
  ((lambda (m) (m x y)) (lambda (p q) p))
  ((lambda (p q) p) x y)
  x |#

;; Exercise 2.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| To establish that 2ᵃ3ᵇ is a valid representation of the pair (a, b), we must
show that the customary invariants for cons, car, and cdr hold true for any
nonnegative integers a and b. Thus we must show that (car (cons a b)) is a and
(cdr (cons a b)) is b. Let f(a,b) = 2ᵃ3ᵇ, and observe that the prime
factorization of f(a,b) includes a twos and b threes. Now let g(z) be the
function that counts the number of twos in the prime factorization of z, and
similarly h(z) for the number of threes. By the prior observation, g(f(a,b)) = a
and h(f(a,b)) = b. If we make our cons, car, and cdr procedures equivalent to f,
g, and h respectively, then we know that (car (cons a b)) returns a and
(cdr (cons a b) returns b, so our representation is valid. |#

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (if (even? z)
      (+ 1 (car (/ z 2)))
      0))

(define (cdr z)
  (if (= (remainder z 3) 0)
      (+ 1 (cdr (/ z 3)))
      0))

;; Exercise 2.6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (+ a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

; Change a Church numeral to a standard number
(define (church->integer n) ((n 1+) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; Exercise 2.7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-interval a b) (cons a b))

(define (upper-bound i) (max (car i) (cdr i)))
(define (lower-bound i) (min (car i) (cdr i)))

;; Exercise 2.8 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| The minimum value of the difference of two intervals must be the minimum
value of the first interval minus the maximum value of the second. The maximum
value of the difference must be the maximum value of the first minus the
minimum value of the second. |#

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Exercise 2.9 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| First we are to show that the width of the sum of two intervals is a function
only of their widths. Let i1, i2 be any two intervals and let ub and lb be the
functions that compute an interval's upper and lower bounds respectively. By the
definition of the sum of two intervals given above, we know that

  ub(i1 + i2) = ub(i1) + ub(i2) and
  lb(i1 + i2) = lb(i1) + lb(i2).

The width of an interval is defined as

  width(i) = 1/2 * (ub(i) - lb(i)).

Thus:

  width(i1 + i2) = 1/2 * (ub(i1 + i2) - lb(i1 + i2))
                 = 1/2 * ((ub(i1) + ub(i2)) - (lb(i1) + lb(i2)))
                 = 1/2 * (ub(i1) - lb(i1)) + 1/2 * (ub(i2) - lb(i2))
                 = width(i1) + width(i2)

which shows that the width of a sum of two intervals is a function only of their
widths.

Second we are to show that the same property is true of the difference of two
intervals. By the definition of the difference of two intervals, for any two
intervals i3 and i4 we know that

  ub(i3 - i4) = lb(i3) - ub(i4) and
  lb(i3 - i4) = ub(i3) - lb(i4).

Thus:

  width(i3 - i4) = 1/2 * (ub(i3 - i4) - lb(i3 - i4))
                 = 1/2 * ((lb(i3) - ub(i4)) - (ub(i3) - lb(i4)))
                 = 1/2 * (lb(i3) - ub(i3)) + 1/2 * (lb(i4) - ub(i4))
                 = (-1/2) * (ub(i3) - lb(i3)) + (-1/2) * (ub(i4) - lb(i4))
                 = -(width(i3) + width(i4))

which shows that the width of a difference of two intervals is a function only
of their widths.

Finally we are to show by counterexample that the property does not hold for
multiplication and division of intervals. For a given operation f over two
intervals, if the property *does* hold for that operation, then for any
intervals a, b, c, and d,

  width(a) = width(b) and
  width(c) = width(d)

implies

  width(f(a,c)) = width(f(b,d)).

We can prove the property does not hold for f by finding an a, b, c, and d for
which the first and second equalities hold true but not the third.

The intervals a=(10,20), b=(20,30), c=(30,60), and d=(60,90) satisfactorily
prove the assertion for both multiplication and division, as shown by running
the small test program below. |#

(define (width i) (/ (- (upper-bound i) (lower-bound i)) 2))

(define (test-counterexample f a b c d)
  (display "test with (a b c d) = ") (display (list a b c d)) (newline)
  (display "width(a) = ") (display (width a)) (newline)
  (display "width(b) = ") (display (width b)) (newline)
  (display "width(c) = ") (display (width c)) (newline)
  (display "width(d) = ") (display (width d)) (newline)
  (display "width(f(a,c)) = ") (display (width (f a c))) (newline)
  (display "width(f(b,d)) = ") (display (width (f b d))) (newline)
  (if (and (= (width a) (width b))
           (= (width c) (width d))
           (not (= (width (f a c)) (width (f b d)))))
      (display "success: the property does not hold!")
      (display "inconclusive result: width(f(a,c)) = width(f(b,d))"))
  (newline))

(define a (make-interval 10 20))
(define b (make-interval 20 30))
(define c (make-interval 30 60))
(define d (make-interval 60 90))

(test-counterexample mul-interval a b c d)
(test-counterexample div-interval a b c d)
(test-counterexample add-interval a b c d)
(test-counterexample sub-interval a b c d)

;; Exercise 2.10 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (contains-interval? i n)
  (and (>= n (lower-bound i))
       (<= n (upper-bound i))))

(define (div-interval x y)
  (if (contains-interval? y 0)
      (error "division by zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; Exercise 2.11 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mul-interval x y)
  (define (both p i) (and (p (upper-bound i)) (p (lower-bound i))))
  (define (pos? i) (both (lambda (x) (>= x 0)) i))
  (define (neg? i) (both (lambda (x) (<= x 0)) i))

  ; Define the following values as procedures instead of precomputing them per
  ; the exercise prompt, which requires limiting the multiplications performed.
  (define (p1) (* (lower-bound x) (lower-bound y)))
  (define (p2) (* (lower-bound x) (upper-bound y)))
  (define (p3) (* (upper-bound x) (lower-bound y)))
  (define (p4) (* (upper-bound x) (upper-bound y)))

  (cond ((pos? x)
         (cond ((pos? y) (make-interval (p1) (p4)))
               ((neg? y) (make-interval (p2) (p3)))
               (else     (make-interval (p3) (p4)))))
        ((neg? x)
         (cond ((pos? y) (make-interval (p2) (p3)))
               ((neg? y) (make-interval (p1) (p4)))
               (else     (make-interval (p1) (p2)))))
        (else
         (cond ((pos? y) (make-interval (p2) (p4)))
               ((neg? y) (make-interval (p1) (p3)))
               (else     (let ((p1 (p1)) (p2 (p2)) (p3 (p3)) (p4 (p4)))
                           (make-interval (max p1 p2 p3 p4)
                                          (min p1 p2 p3 p4))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Exercise 2.12 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-center-percent c p)
  (let ((width (* c (/ p 100))))
    (make-interval (- c width) (+ c width))))

(define (percent i)
  (* (/ (width i) (center i)) 100))

;; Exercise 2.13 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| We are to derive a formula for approximating the tolerance of the product of
two intervals in terms of the tolerances of the factors.

Using our previous definitions of operations on intervals, for some interval i,
the upper bound, lower bound, width, center, and tolerance have the following
equivalencies:

  ub(i) = center(i) * (1 + tol(i))
  lb(i) = center(i) * (1 - tol(i))
  width(i) = (ub(i) - lb(i)) / 2
  center(i) = (ub(i) + lb(i)) / 2
  tol(i) = width(i) / center(i)

The bounds of the product of two intervals i and j (with all endpoints positive)
are defined as

  ub(ij) = ub(i) * ub(j)
  lb(ij) = lb(i) * lb(j).

Using these facts we can derive the approximation formula:

  ub(ij) = ub(i)                    * ub(j)
         = center(i) * (1 + tol(i)) * center(j) * (1 + tol(j))

  lb(ij) = lb(i)                    * lb(j)
         = center(i) * (1 - tol(i)) * center(j) * (1 - tol(j))

  tol(ij) = width(ij)                        / center(ij)
          = ((ub(ij)        - lb(ij)) / 2)   / ((ub(ij)        + lb(ij)) / 2)
          =  (ub(ij)        - lb(ij))        /  (ub(ij)        + lb(ij))
          =  (ub(i) * ub(j) - lb(i) * lb(j)) /  (ub(i) * ub(j) + lb(i) * lb(j))

            center(i) * (1 + tol(i)) * center(j) * (1 + tol(j)) -
            center(i) * (1 - tol(i)) * center(j) * (1 - tol(j))
          = —————————————————————————————————————————————————————
            center(i) * (1 + tol(i)) * center(j) * (1 + tol(j)) +
            center(i) * (1 - tol(i)) * center(j) * (1 - tol(j))

            (1 + tol(i)) * (1 + tol(j)) - (1 - tol(i)) * (1 - tol(j))
          = —————————————————————————————————————————————————————————
            (1 + tol(i)) * (1 + tol(j)) + (1 - tol(i)) * (1 - tol(j))

            (1 + tol(i) + tol(j) + tol(i) * tol(j)) -
            (1 - tol(i) - tol(j) + tol(i) * tol(j))
          = —————————————————————————————————————————
            (1 + tol(i) + tol(j) + tol(i) * tol(j)) +
            (1 - tol(i) - tol(j) + tol(i) * tol(j))

            2 * tol(i) + 2 * tol(j)     tol(i) + tol(j)
          = ——————————————————————— = ———————————————————
            2 + 2 * tol(i) * tol(j)   1 + tol(i) * tol(j)

Since we assume small tolerances, tol(i) and tol(j) are small fractions, and so
their product will be very small. As an approximation we can assume it is 0,
which yields this simple approximation for the tolerance of the product of
intervals:

  tol(ij) = tol(i) + tol(j) |#
