;;;;CODE FROM CHAPTER 2 OF STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;; Examples from the book are commented out with ;: so that they
;;;  are easy to find and so that they will be omitted if you evaluate a
;;;  chunk of the file (programs with intervening examples) in Scheme.

;;; BEWARE: Although the whole file can be loaded into Scheme,
;;;  you won't want to do so.  For example, you generally do
;;;  not want to use the procedural representation of pairs
;;;  (cons, car, cdr as defined in section 2.1.3) instead of
;;;  Scheme's primitive pairs.

;;; Some things require code from other chapters -- see ch2support.scm


(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))


;;;SECTION 2.1.1

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;: (define x (cons 1 2))
;:
;: (car x)
;: (cdr x)

;: (define x (cons 1 2))
;: (define y (cons 3 4))
;: (define z (cons x y))
;: (car (car z))
;: (car (cdr z))

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

;;footnote -- alternative definitions
(define make-rat cons)
(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


;: (define one-half (make-rat 1 2))
;:
;: (print-rat one-half)
;:
;: (define one-third (make-rat 1 3))
;:
;: (print-rat (add-rat one-half one-third))
;: (print-rat (mul-rat one-half one-third))
;: (print-rat (add-rat one-third one-third))


;; reducing to lowest terms in constructor
;; (uses gcd from 1.2.5 -- see ch2support.scm)

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))


;: (print-rat (add-rat one-third one-third))


;;EXERCISE 2.1
(define (make-rat n d)
  (let ((sign (if (= (positive? n) (positive? d)) 1 -1)))
    (cons (* sign (abs n)) d)))


;;;SECTION 2.1.2

;; reducing to lowest terms in selectors
;; (uses gcd from 1.2.5 -- see ch2support.scm)

(define (make-rat n d)
  (cons n d))

(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))


;; EXERCISE 2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

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

;(let ((p1 (make-point 3 -5))
;      (p2 (make-point -5 9)))
;  (print-point (midpoint-segment (make-segment p1 p2))))
; (-1,2)

;;EXERCISE 2.3
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

(define (left rect)   (x-point (car rect)))
(define (right rect)  (+ (left rect) (width rect)))
(define (bottom rect) (y-point (car rect)))
(define (top rect)    (+ (bottom rect) (height rect)))
(define (width rect)  (car (cdr rect)))
(define (height rect) (cdr (cdr rect)))

; higher-level procedures that work using either representation
(define (perimeter rect) (* 2 (+ (width rect) (height rect))))
(define (area rect)      (* (width rect) (height rect)))


;;;SECTION 2.1.3
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))

(define (cdr z) (z 1))


;; EXERCISE 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

; Here is the series of substitutions performed during evaluation of
; (car (cons x y)), showing that this expression evaluates to x for any objects
; x and y.
;
; (car (cons x y))
; (car (lambda (m) (m x y)))
; ((lambda (m) (m x y)) (lambda (p q) p))
; ((lambda (p q) p) x y)
; x

;;EXERCISE 2.5
; To establish that 2ᵃ3ᵇ is a valid representation of the pair (a, b), we must
; show that the customary invariants for cons, car, and cdr hold true for any
; nonnegative integers a and b. Thus we must show that (car (cons a b)) is a and
; (cdr (cons a b)) is b. Let f(a,b) = 2ᵃ3ᵇ, and observe that the prime
; factorization of f(a,b) includes a twos and b threes. Now let g(z) be the
; function that counts the number of twos in the prime factorization of z, and
; similarly h(z) for the number of threes. By the prior observation,
; g(f(a,b)) = a and h(f(a,b)) = b. If we make our cons, car, and cdr procedures
; equivalent to f, g, and h respectively, then we know that (car (cons a b))
; returns a and (cdr (cons a b) returns b, so our representation is valid.

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

;; EXERCISE 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (+ a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

; Change a Church numeral to a standard number
(define (church->integer n) ((n 1+) 0))


;;;SECTION 2.1.4

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

;; EXERCISE 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound i) (max (car i) (cdr i)))
(define (lower-bound i) (min (car i) (cdr i)))

;;EXERCISE 2.8
; The minimum value of the difference of two intervals must be the minimum
; value of the first interval minus the maximum value of the second. The maximum
; value of the difference must be the maximum value of the first minus the
; minimum value of the second.

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;;EXERCISE 2.9
; First we are to show that the width of the sum of two intervals is a function
; only of their widths. Let i1, i2 be any two intervals and let ub and lb be the
; functions that compute an interval's upper and lower bounds respectively. By
; definition of the sum of two intervals given above, we know that
;
;   ub(i1 + i2) = ub(i1) + ub(i2) and
;   lb(i1 + i2) = lb(i1) + lb(i2).
;
; The width of an interval is defined as
;
;   width(i) = 1/2 * (ub(i) - lb(i)).
;
; Thus:
;
;   width(i1 + i2) = 1/2 * (ub(i1 + i2) - lb(i1 + i2))
;                  = 1/2 * ((ub(i1) + ub(i2)) - (lb(i1) + lb(i2)))
;                  = 1/2 * (ub(i1) - lb(i1)) + 1/2 * (ub(i2) - lb(i2))
;                  = width(i1) + width(i2)
;
; which shows that the width of a sum of two intervals is a function only of
; their widths.
;
; Second we are to show that the same property is true of the difference of two
; intervals. By the definition of the difference of two intervals, for any two
; intervals i3 and i4 we know that
;
;   ub(i3 - i4) = lb(i3) - ub(i4) and
;   lb(i3 - i4) = ub(i3) - lb(i4).
;
; Thus:
;
;   width(i3 - i4) = 1/2 * (ub(i3 - i4) - lb(i3 - i4))
;                  = 1/2 * ((lb(i3) - ub(i4)) - (ub(i3) - lb(i4)))
;                  = 1/2 * (lb(i3) - ub(i3)) + 1/2 * (lb(i4) - ub(i4))
;                  = (-1/2) * (ub(i3) - lb(i3)) + (-1/2) * (ub(i4) - lb(i4))
;                  = -(width(i3) + width(i4))
;
; which shows that the width of a difference of two intervals is a function only
; of their widths.
;
; Finally we are to show by counterexample that the property does not hold for
; multiplication and division of intervals. For a given operation f over two
; intervals, if the property *does* hold for that operation, then for any
; intervals a, b, c, and d,
;
;   width(a) = width(b) and
;   width(c) = width(d)
;
; implies
;
;   width(f(a,c)) = width(f(b,d)).
;
; We can prove the property does not hold for f by finding an a, b, c, and d for
; which the first and second equalities hold true but not the third.
;
; The intervals a=(10,20), b=(20,30), c=(30,60), and d=(60,90) satisfactorily
; prove the assertion for both multiplication and division, as shown by running
; the small test program below.

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

;(define a (make-interval 10 20))
;(define b (make-interval 20 30))
;(define c (make-interval 30 60))
;(define d (make-interval 60 90))

;(test-counterexample mul-interval a b c d) ; success
;(test-counterexample div-interval a b c d) ; success
;(test-counterexample add-interval a b c d) ; success
;(test-counterexample sub-interval a b c d) ; success

;;EXERCISE 2.10
(define (contains-interval? i n)
  (and (>= n (lower-bound i))
       (<= n (upper-bound i))))

(define (div-interval x y)
  (if (contains-interval? y 0)
      (error "division by zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;;EXERCISE 2.11
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


;;;SECTION 2.1.4 again

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;EXERCISE 2.12
(define (make-center-percent c p)
  (let ((width (* c (/ p 100))))
    (make-interval (- c width) (+ c width))))

(define (percent i)
  (* (/ (width i) (center i)) 100))

;;EXERCISE 2.13
; We are to derive a formula for approximating the tolerance of the product of
; two intervals in terms of the tolerances of the factors.
;
; Using our previous definitions of operations on intervals, for some interval
; i, the upper bound, lower bound, width, center, and tolerance have the
; following equivalencies:
;
;   ub(i) = center(i) * (1 + tol(i))
;   lb(i) = center(i) * (1 - tol(i))
;   width(i) = (ub(i) - lb(i)) / 2
;   center(i) = (ub(i) + lb(i)) / 2
;   tol(i) = width(i) / center(i)
;
; The bounds of the product of two intervals i and j (with all endpoints
; positive) are defined as
;
;   ub(ij) = ub(i) * ub(j)
;   lb(ij) = lb(i) * lb(j).
;
; Using these facts we can derive the approximation formula:
;
;   ub(ij) = ub(i)                    * ub(j)
;          = center(i) * (1 + tol(i)) * center(j) * (1 + tol(j))
;
;   lb(ij) = lb(i)                    * lb(j)
;          = center(i) * (1 - tol(i)) * center(j) * (1 - tol(j))
;
;   tol(ij) = width(ij)                        / center(ij)
;           = ((ub(ij)        - lb(ij)) / 2)   / ((ub(ij)        + lb(ij)) / 2)
;           =  (ub(ij)        - lb(ij))        /  (ub(ij)        + lb(ij))
;           =  (ub(i) * ub(j) - lb(i) * lb(j)) /  (ub(i) * ub(j) + lb(i)*lb(j))
;
;             center(i) * (1 + tol(i)) * center(j) * (1 + tol(j)) -
;             center(i) * (1 - tol(i)) * center(j) * (1 - tol(j))
;           = —————————————————————————————————————————————————————
;             center(i) * (1 + tol(i)) * center(j) * (1 + tol(j)) +
;             center(i) * (1 - tol(i)) * center(j) * (1 - tol(j))
;
;             (1 + tol(i)) * (1 + tol(j)) - (1 - tol(i)) * (1 - tol(j))
;           = —————————————————————————————————————————————————————————
;             (1 + tol(i)) * (1 + tol(j)) + (1 - tol(i)) * (1 - tol(j))
;
;             (1 + tol(i) + tol(j) + tol(i) * tol(j)) -
;             (1 - tol(i) - tol(j) + tol(i) * tol(j))
;           = —————————————————————————————————————————
;             (1 + tol(i) + tol(j) + tol(i) * tol(j)) +
;             (1 - tol(i) - tol(j) + tol(i) * tol(j))
;
;             2 * tol(i) + 2 * tol(j)     tol(i) + tol(j)
;           = ——————————————————————— = ———————————————————
;             2 + 2 * tol(i) * tol(j)   1 + tol(i) * tol(j)
;
; Since we assume small tolerances, tol(i) and tol(j) are small fractions, and
; so their product will be very small. As an approximation we can assume it's 0,
; which yields this simple approximation for the tolerance of the product of
; intervals:
;
;   tol(ij) = tol(i) + tol(j) |#

;; parallel resistors

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;;EXERCISE 2.14
; Below, a test procedure is used to evaluate different implementations of
; several intervallic expressions. For a given expression, implementations may
; produce inconsistent results when the same inputs are used in positions that
; affect the final result differently. For example:
;
  ; par1(r1,r2) = (a*b)/(c+d) where a=r1 b=r2 c=r1 d=r2
  ; par2(r1,r2) = 1/(1/a+1/b) where a=r1 b=r2
;
; In par1, input r1 is used in terms (a, c) that affect the final result in
; opposite directions whereas only one direction in par2. For the same input
; values, par1 generates a wider result interval than par2.
;
; Where implementations do not use inputs in "contradictory" ways, the results
; are consistent; see xxy1 and xxy2 below for example.

(define (test f a-center a-width b-center b-width)
  (let ((a (make-center-width a-center a-width))
        (b (make-center-width b-center b-width)))
    (let ((result (f a b)))
      (let ((result-center (center result))
            (result-width (width result)))
        (display "; c=") (display result-center)
        (display " w=") (display result-width) (newline)))))

(define (percent-error expected value)
  (* 100 (abs (/ (- value expected) expected))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;(test par1 100 1 100 1)  ; c=50.02000200020002  w=1.50020002000200
;(test par2 100 1 100 1)  ; c=50.                w=.5000000000000036
;(test par1 150 5 300 10) ; c=100.44493882091213 w=10.014831294030401
;(test par2 150 5 300 10) ; c=100.               w=3.3333333333333286

(define (div-twice a b)
  (div-interval (mul-interval (div-interval a b) b) b))

(define (div-thrice a b)
  (div-interval (mul-interval (div-twice a b) b) b))

;(test div-interval 100 1 100 1)  ; c=1.0002000200020003 w=2.0002000200020076e-2
;(test div-twice 100 1 100 1)     ; c=1.0008001600240033 w=.0400120020002801
;(test div-thrice 100 1 100 1)    ; c=1.0018006601460259 w=.06003801020198035
;(test div-interval 150 5 300 10) ; c=.5011123470522802  w=3.3370411568409336e-2
;(test div-twice 150 5 300 10)    ; c=.5044543374729801  w=.06688930105258473
;(test div-thrice 150 5 300 10)   ; c=.5100408410748722  w=.10070580700417978

(define (xxy1 x y)
  (mul-interval (mul-interval x x) y))

(define (xxy2 x y)
  (mul-interval x (mul-interval x y)))

;(test xxy1 100 1 100 1) ; c=1000300 w=30001
;(test xxy2 100 1 100 1) ; c=1000300 w=30001
;(test xxy1 150 5 300 10) ; c=6772500 w=675250
;(test xxy2 150 5 300 10) ; c=6772500 w=675250

(define (add-twice a b)
  (add-interval (sub-interval (add-interval a b) b) b))

(define (add-thrice a b)
  (add-interval (sub-interval (add-twice a b) b) b))

(define (add-alt a b)
  (sub-interval (mul-interval (add-interval a b)
                              (make-interval 2 2))
                (add-interval a b)))

;(test add-interval 150 5 300 10) ; c=450 w=15
;(test add-twice 150 5 300 10)    ; c=450 w=35
;(test add-thrice 150 5 300 10)   ; c=450 w=55
;(test add-alt 150 5 300 10)      ; c=450 w=45

(define (quad-sum1 a b)
  (define sum (add-interval a b))
  (define double-sum (add-interval sum sum))
  (add-interval double-sum double-sum))

(define (quad-sum2 a b)
  (let ((2a+b (add-interval a (add-interval a b)))
        (2b (add-interval b b)))
    (add-interval (add-interval 2a+b 2a+b) 2b)))

;(test quad-sum1 150 5 300 10) ; c=1800 w=60
;(test quad-sum2 150 5 300 10) ; c=1800 w=60

;;EXERCISE 2.15
; The observation is correct: formulas produce tighter error bounds when
; variables that carry uncertainty are not repeated inside the formula. Not only
; are the error bounds on the results tighter, but also they are more correct
; for this (and probably most) problem domains. The reason is that variables
; which simulate a real-world quantity can only have one actual value at any
; given time. When we include such a variable in a formula more than once, and
; that variable carries uncertainty, then the overall uncertainty of the formula
; will erroneously reflect that *each* occurrence of that variable could have
; any value within the variable's entire range of possibilities. It is as though
; we had used several variables with identical uncertainties instead of just
; one! Using an uncertainty-carrying variable only once inside a formula avoids
; this problem.

;;EXERCISE 2.16
; TODO


;;;SECTION 2.2.1

;: (cons 1
;:       (cons 2
;:             (cons 3
;:                   (cons 4 nil))))


;: (define one-through-four (list 1 2 3 4))
;:
;: one-through-four
;: (car one-through-four)
;: (cdr one-through-four)
;: (car (cdr one-through-four))
;: (cons 10 one-through-four)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

;: (define squares (list 1 4 9 16 25))

;: (list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

;: (define odds (list 1 3 5 7))

;: (length odds)

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

;: (append squares odds)
;: (append odds squares)


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))


;; EXERCISE 2.17
(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))
;: (last-pair (list 23 72 149 34)) ; 34

;; EXERCISE 2.18
(define (reverse list)
  (define (iter old new)
    (if (null? old)
        new
        (iter (cdr old) (cons (car old) new))))
  (iter list '()))

;: (reverse (list 1 4 9 16 25)) ; (25 16 9 4 1)

;; EXERCISE 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;(cc 100 us-coins) ; 292
;(cc 100 uk-coins) ; 104561

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (first-denomination coin-values) (car coin-values))
(define (except-first-denomination coin-values) (cdr coin-values))
(define (no-more? coin-values) (null? coin-values))

;; EXERCISE 2.20
(define (same-parity x . xs)
  (define filter (if (even? x) even? odd?))
  (define (iter acc xs)
    (if (null? xs)
        acc
        (iter (if (filter (car xs))
                  (cons (car xs) acc)
                  acc)
              (cdr xs))))
  (reverse (iter (list x) xs)))

;: (same-parity 1 2 3 4 5 6 7) ; (1 3 5 7)
;: (same-parity 2 3 4 5 6 7) ; (2 4 6)


;; Mapping over lists

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

;: (scale-list (list 1 2 3 4 5) 10)

;: (map + (list 1 2 3) (list 40 50 60) (list 700 800 900))

;: (map (lambda (x y) (+ x (* 2 y)))
;:      (list 1 2 3)
;:      (list 4 5 6))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;: (map abs (list -10 2.5 -11.6 17))

;: (map (lambda (x) (* x x))
;:      (list 1 2 3 4))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))


;; EXERCISE 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;: (square-list (list 1 2 3 4)) ; (1 4 9 16)

;; EXERCISE 2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

; The result is in reverse order because iter consumes items from the input
; list moving from head to tail, while building up the output list in
; tail-to-head direction.

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

; A list is a pair consisting of a list item and another list, in that order.
; By transposing the arguments, the above procedure builds up not a list but a
; nested pair of the form (cons (cons (cons '() item1) item2) item3).

;; EXERCISE 2.23
(define (for-each f xs)
  (if (null? xs)
      #t
      (begin (f (car xs))
             (for-each f (cdr xs)))))

;: (for-each (lambda (x) (newline) (display x))
;:           (list 57 321 88))
;57
;321
;88
;Value: #t


;;;SECTION 2.2.2
;: (cons (list 1 2) (list 3 4))
;:
;: (define x (cons (list 1 2) (list 3 4)))
;: (length x)
;: (count-leaves x)
;:
;: (list x x)
;: (length (list x x))
;: (count-leaves (list x x))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; EXERCISE 2.24
;: (list 1 (list 2 (list 3 4)))

; Interpreter result:
;
;   (1 (2 (3 4)))
;
; Box-and-pointer structure:
;
;   (1 (2 (3 4)))──>[•][•]────>[•|/]
;                    ↓          ↓
;                   [1]  ┌───>[•|•]─────>[•|/]
;                        │     ↓          ↓
;                 (2 (3 4))   [2]  ┌───>[•|•] → [•|/]
;                                  │     ↓       ↓
;                               (3 4)   [3]     [4]
;
; Tree:
;
;   (1 (2 (3 4)))
;     ├─ 1
;     └─ (2 (3 4))
;          ├─ 2
;          └─ (3 4)
;               ├─ 3
;               └─ 4

;; EXERCISE 2.25
;(define x '(1 3 (5 7) 9))
;(car (cdr (car (cdr (cdr x))))) ; 7

;(define y '((7)))
;(car (car y)) ; 7

;(define z '(1 (2 (3 (4 (5 (6 7)))))))
;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z)))))))))))) ; 7

;; EXERCISE 2.26
;: (define x (list 1 2 3))
;: (define y (list 4 5 6))
;:
;: (append x y) ; (1 2 3 4 5 6)
;: (cons x y) ; ((1 2 3) 4 5 6)
;: (list x y) ; ((1 2 3) (4 5 6))

;; EXERCISE 2.27
(define (deep-reverse item)
  (define (iter old new)
    (if (null? old)
        new
        (iter (cdr old)
              (cons (deep-reverse (car old))
                    new))))
  (if (list? item)
      (iter item '())
      item))

;: (define x (list (list 1 2) (list 3 4)))
;: x ; ((1 2) (3 4))
;: (reverse x) ; ((3 4) (1 2))
;: (deep-reverse x) ; ((4 3) (2 1))

;; EXERCISE 2.28
(define (fringe tree)
  (cond ((null? tree) '())
        ((not (list? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

;: (define x (list (list 1 2) (list 3 4)))
;: (fringe x) ; (1 2 3 4)
;: (fringe (list x x)) ; (1 2 3 4 1 2 3 4)

;; EXERCISE 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a.
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

; b.
(define (total-weight mobile)
  (define (weight branch)
    (let ((structure (branch-structure branch)))
      (if (number? structure)
          structure
          (total-weight structure))))
  (+ (weight (left-branch mobile)) (weight (right-branch mobile))))

;(total-weight
;  (make-mobile (make-branch 5 20)
;               (make-branch 6 (make-mobile (make-branch 7 30)
;                                           (make-branch 8 40))))) ; 90

; c.
(define (balanced? mobile)
  (define (weight branch)
    (let ((structure (branch-structure branch)))
      (if (number? structure)
          structure
          (total-weight structure))))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (= (* (branch-length left) (weight left))
       (* (branch-length right) (weight right)))))

;(balanced?
;  (make-mobile (make-branch 5 20)
;               (make-branch 6 (make-mobile (make-branch 7 30)
;                                           (make-branch 8 40))))) ; #f

;(balanced?
;  (make-mobile (make-branch 7 3)
;               (make-branch 3 (make-mobile (make-branch 4 5)
;                                           (make-branch 10 2))))) ; #t

; d.
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

; To adapt to this new representation, only the selectors left-branch,
; right-branch, branch-length, and branch-structure need be updated. The other
; procedures are implemented in terms of these and do not access the underlying
; structure directly.


;; Mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))


;: (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
;:             10)


(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))


;; EXERCISE 2.30
(define (square-tree tree)
  (if (null? tree)
      tree
      (cons (if (number? (car tree))
                (square (car tree))
                (square-tree (car tree)))
            (square-tree (cdr tree)))))

(define (square-tree tree)
  (map (lambda (item)
         ((if (number? item)
              square
              square-tree)
          item))
       tree))

;: (square-tree
;:  (list 1
;:        (list 2 (list 3 4) 5)
;:        (list 6 7))) ; (1 (4 (9 16) 25) (36 49))

;; EXERCISE 2.31
(define (square-tree tree) (tree-map square tree))

(define (tree-map f tree)
  (if (null? tree)
      tree
      (cons (if (list? (car tree))
                (tree-map f (car tree))
                (f (car tree)))
            (tree-map f (cdr tree)))))

;; EXERCISE 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (ss) (cons (car s) ss))
                     rest)))))

;(subsets (list 1 2 3)) ; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; This implementation works based on the following rationale. A subset of set S
; is a combination of zero or more elements of S. To find all subsets of S we
; must find all possible combinations of elements in S. For empty S the only
; subset is is the empty set, the only possible combination of zero elements.
; For non-empty S, observe that for any element e in S, the possible subsets of
; S are the union of all combinations that include e and those that don't.
; Observe also that to get the combinations that include e, we can simply add e
; to every other possible combination. All that remains is to calculate the
; combinations that do not include e, which is simply done by applying
; recursively the logic just laid out.


;;;SECTION 2.2.3

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))


;; Sequence operations

;: (map square (list 1 2 3 4 5))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;: (filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;: (accumulate + 0 (list 1 2 3 4 5))
;: (accumulate * 1 (list 1 2 3 4 5))
;: (accumulate cons nil (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;: (enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;: (enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

;: (list-fib-squares 10)


(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

;: (product-of-squares-of-odd-elements (list 1 2 3 4 5))

(define (salary-of-highest-paid-programmer records)
  (accumulate max
              0
              (map salary
                   (filter programmer? records))))


;;EXERCISE 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;; EXERCISE 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;: (horner-eval 2 (list 1 3 0 5 0 1)) ; 79

;;EXERCISE 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (list? x)
                             (count-leaves x)
                             1))
                       t)))

;; EXERCISE 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) ; (22 26 30)

;; EXERCISE 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (accumulate cons nil (map (lambda (col)
                                       (dot-product row col))
                                     cols)))
         m)))

;(define v1 '(1 3 5 7))
;(define v2 '(2 4 6 8))
;(define m1 '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
;(define m2 '((1 2 3) (3 3 4) (4 5 6) (7 8 8)))
;(define m3 '((1 2) (3 5) (4 5) (6 9)))

;(dot-product v1 v2) ; 100
;(matrix-*-vector m1 v1) ; (50 91 130)
;(transpose m1) ; ((1 4 6) (2 5 7) (3 6 8) (4 6 9))
;(matrix-*-matrix m1 m2) ; ((47 55 61) (85 101 116) (122 145 166))
;(matrix-*-matrix m1 m3) ; ((43 63) (79 117) (113 68))

;; EXERCISE 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;: (fold-right / 1 (list 1 2 3)) ; 3/2
;: (fold-left / 1 (list 1 2 3)) ; 1/6
;: (fold-right list nil (list 1 2 3)) ; (1 (2 (3 ())))
;: (fold-left list nil (list 1 2 3)) ; (((() 1) 2) 3)

; To guarantee that fold-right and fold-left produce the same result for every
; sequence, (op a b) should be equivalent to (op b a).

;; EXERCISE 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

;(reverse '(1 2 3)) ; (3 2 1)


;;Nested mappings

;: (accumulate append
;:             nil
;:             (map (lambda (i)
;:                    (map (lambda (j) (list i j))
;:                         (enumerate-interval 1 (- i 1))))
;:                  (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))


(define (permutations s)
  (if (null? s)                         ; empty set?
      (list nil)                        ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;;EXERCISE 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;(unique-pairs 3) ; ((2 1) (3 1) (3 2)

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;(prime-sum-pairs 6) ;((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

;; EXERCISE 2.41
(define (find-triples-with-sum n s)
  (let ((triples
         (flatmap (lambda (i)
                    (map (lambda (j)
                           (list i j (- s i j)))
                         (enumerate-interval 1 (min i (- s 1 i)))))
                  (enumerate-interval 1 (min n (- s 2))))))
       (filter (lambda (ijk) (<= (caddr ijk) (cadr ijk)))
               triples)))

;:(find-triples-with-sum 5 9) ; ((3 3 3) (4 3 2) (4 4 1) (5 2 2) (5 3 1))

;; EXERCISE 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (row-of-queen-in-col positions col)
  (list-ref positions (- col 1)))

(define empty-board '())

(define (adjoin-position new-row k position)
  (append position (list new-row)))

(define (safe? k positions)
  (fold-left (lambda (acc col)
               (if (not acc)
                   #f
                   (safe-from k col positions)))
             #t
             (enumerate-interval 1 (length positions))))

(define (safe-from k1 k2 positions)
  (let ((row1 (row-of-queen-in-col positions k1))
        (row2 (row-of-queen-in-col positions k2)))
       (cond ((= k1 k2) #t)
             ((= row1 row2) #f)
             ((= (abs (- row1 row2))
                 (abs (- k1 k2)))
              #f)
             (else #t))))

(queens 5)
; ((1 3 5 2 4) (1 4 2 5 3) (2 4 1 3 5) (2 5 3 1 4) (3 1 4 2 5)
;  (3 5 2 4 1) (4 1 3 5 2) (4 2 5 3 1) (5 2 4 1 3) (5 3 1 4 2))

;; EXERCISE 2.43
;; Louis's version of queens
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
	        ;; next expression changed
          (flatmap
	          (lambda (new-row)
	            (map (lambda (rest-of-queens)
		                 (adjoin-position new-row k rest-of-queens))
		               (queen-cols (- k 1))))
	          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))


;;;SECTION 2.2.4

;: (define wave2 (beside wave (flip-vert wave)))
;: (define wave4 (below wave2 wave2))


(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))


;: (define wave4 (flipped-pairs wave))


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))


(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))


;; Higher-order operations

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))


(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

; footnote
;: (define flipped-pairs
;:   (square-of-four identity flip-vert identity flip-vert))


(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


;; EXERCISE 2.45

;: (define right-split (split beside below))
;: (define up-split (split below beside))


;; Frames

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


;: ((frame-coord-map a-frame) (make-vect 0 0))

;: (origin-frame a-frame)


;; EXERCISE 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))


;; Painters

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))


(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)    ; new origin
                     (make-vect 1.0 1.0)    ; new end of edge1
                     (make-vect 0.0 0.0)))  ; new end of edge2


(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))


(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;;;SECTION 2.3.1

;: (a b c d)
;: (23 45 17)
;: ((Norah 12) (Molly 9) (Anna 7) (Lauren 6) (Charlotte 3))

;: (* (+ 23 45) (+ x 9))

(define (fact n) (if (= n 1) 1 (* n (fact (- n 1)))))


;: (define a 1)
;: (define b 2)
;: (list a b)
;: (list 'a 'b)
;: (list 'a b)

;: (car '(a b c))
;: (cdr '(a b c))


(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;: (memq 'apple '(pear banana prune))
;: (memq 'apple '(x (apple sauce) y apple pear))


;; EXERCISE 2.53
;: (list 'a 'b 'c)
;:
;: (list (list 'george))
;:
;: (cdr '((x1 x2) (y1 y2)))
;:
;: (cadr '((x1 x2) (y1 y2)))
;:
;: (pair? (car '(a short list)))
;:
;: (memq 'red '((red shoes) (blue socks)))
;:
;: (memq 'red '(red shoes blue socks))


;; EXERCISE 2.54
;: (equal? '(this is a list) '(this is a list))
;: (equal? '(this is a list) '(this (is a) list))

;; EXERCISE 2.55
;: (car ''abracadabra)


;;;SECTION 2.3.2

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; representing algebraic expressions

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))


;: (deriv '(+ x 3) 'x)
;: (deriv '(* x y) 'x)
;: (deriv '(* (* x y) (+ x 3)) 'x)


;; With simplification

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


;: (deriv '(+ x 3) 'x)
;: (deriv '(* x y) 'x)
;: (deriv '(* (* x y) (+ x 3)) 'x)


;; EXERCISE 2.57
;: (deriv '(* x y (+ x 3)) 'x)


;;;SECTION 2.3.3

;; UNORDERED

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


;; ORDERED

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;; BINARY TREES
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


;; EXERCISE 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


;; EXERCISE 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; INFORMATION RETRIEVAL

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))


;;;SECTION 2.3.3

;; representing

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; sets

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;; EXERCISE 2.67

;: (define sample-tree
;:   (make-code-tree (make-leaf 'A 4)
;:                   (make-code-tree
;:                    (make-leaf 'B 2)
;:                    (make-code-tree (make-leaf 'D 1)
;:                                    (make-leaf 'C 1)))))

;: (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


;; EXERCISE 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; EXERCISE 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;;;SECTION 2.4.1

;: (make-from-real-imag (real-part z) (imag-part z))

;: (make-from-mag-ang (magnitude z) (angle z))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


;; Ben (rectangular)

(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))


;; Alyssa (polar)

(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang r a) (cons r a))


;;;SECTION 2.4.2

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))


;; Ben (rectangular)

(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

;; Alyssa (polar)

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
               (cons (sqrt (+ (square x) (square y)))
                     (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))


;; Generic selectors

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))

;; same as before
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

;; Constructors for complex numbers

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;;;SECTION 2.4.3

;; uses get/put (from 3.3.3) -- see ch2support.scm

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;;footnote
;: (apply + (list 1 2 3 4))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

;; Generic selectors

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


;; Constructors for complex numbers

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))



;; EXERCISE 2.73
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else (error "unknown expression type -- DERIV" exp))))


(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;: ((get (operator exp) 'deriv) (operands exp) var)


;; Message passing
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

;;;SECTION 2.5.1

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


;; EXERCISE 2.77
;; to put in complex package

;: (put 'real-part '(complex) real-part)
;: (put 'imag-part '(complex) imag-part)
;: (put 'magnitude '(complex) magnitude)
;: (put 'angle '(complex) angle)


;;;SECTION 2.5.2

;; to be included in the complex package
;: (define (add-complex-to-schemenum z x)
;:   (make-from-real-imag (+ (real-part z) x)
;:                        (imag-part z)))
;:
;: (put 'add '(complex scheme-number)
;:      (lambda (z x) (tag (add-complex-to-schemenum z x))))


;; Coercion

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

;: (put-coercion 'scheme-number 'complex scheme-number->complex)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; EXERCISE 2.81

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
;: (put-coercion 'scheme-number 'scheme-number
;:               scheme-number->scheme-number)
;: (put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))
;: (put 'exp '(scheme-number scheme-number)
;:      (lambda (x y) (tag (expt x y))))

;;;SECTION 2.5.3

;;; ALL procedures in 2.5.3 except make-polynomial
;;; should be inserted in install-polynomial-package, as indicated

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))

;; *incomplete* skeleton of package
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;;[procedures same-variable? and variable? from section 2.3.2]

  ;; representation of terms and term lists
  ;;[procedures adjoin-term ... coeff from text below]

  ;;(define (add-poly p1 p2) ... )
  ;;[procedures used by add-poly]

  ;;(define (mul-poly p1 p2) ... )
  ;;[procedures used by mul-poly]

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))


;; Representing term lists

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))


;; Constructor
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))


;; EXERCISE 2.91

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     ??FILL-THIS-IN?? ;compute rest of result recursively
                     ))
                ??FILL-THIS-IN?? ;form complete result
                ))))))


;; EXERCISE 2.93
;: (define p1 (make-polynomial 'x '((2 1)(0 1))))
;: (define p2 (make-polynomial 'x '((3 1)(0 1))))
;: (define rf (make-rational p2 p1))


;; Rational functions

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))


;; EXERCISE 2.94
;: (define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
;: (define p2 (make-polynomial 'x '((3 1) (1 -1))))
;: (greatest-common-divisor p1 p2)


;; EXERCISE 2.97

(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

;: (define p1 (make-polynomial 'x '((1 1)(0 1))))
;: (define p2 (make-polynomial 'x '((3 1)(0 -1))))
;: (define p3 (make-polynomial 'x '((1 1))))
;: (define p4 (make-polynomial 'x '((2 1)(0 -1))))

;: (define rf1 (make-rational p1 p2))
;: (define rf2 (make-rational p3 p4))

;: (add rf1 rf2)
