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

; Louis's version runs more slowly because each invocation of queen-cols now
; redundantly makes board-size recursive calls to itself instead of just one,
; leading to an explosion in the total number of steps performed.
;
; To estimate how much slower Louis's version runs, I will start by comparing
; the processes generated by the two procedures. The structure of the code is
; identical except for the transposition of the two loops, which does not
; change the result of flatmap, nor does it significantly change the number of
; operations performed within a single iteration of queen-cols. What does
; change is the total number of recursive calls performed.
;
; In the first version, for board size n there are n recursive calls in
; total, as each invocation of queen-cols recurses once while decrementing
; k. I ignore the calls with k=0 as they add a trivial number of steps.
;
;   R₁(n) = n
;
; In the second version, the total number of recursive calls for board size n
; (again ignoring k=0) is
;
;            / 1                if n = 1
;   R₂(n) = {
;            \ 1 + n * R(n - 1) otherwise
;
; since the outermost call (1) invokes itself with n-1 inside a loop that
; repeats n times. This definition is equivalent to
;
;   R₂(n) =      Σ (i=1..n) (n! / (n-i)!)
;         = n! * Σ (i=1..n) (1  / i!)
;
; Thus for a given n, the slow version makes R₂(n) / R₁(n) times as many
; recursive calls as the fast version:
;
;   R₂₁(n) = R₂(n) / R₁(n) = (n - 1)! * Σ (i=1..n) (1 / i!)
;
; Additionally, as n goes to infinity, noting that Σ (i=0..i) (1 / i!) = e:
;
;   R₂(n) = n! * ((Σ (i=0..n) (1 / i!)) - (1 / 0!))
;         = n! * (e - 1)
;
; And so we have the orders of growth in the number of recursive calls: Θ(n) for
; the first version and Θ(n!) for the second.
;
; We can use the number of recursive calls as a proxy for running time if we
; assume that each call adds the same number of steps. That is the case if
; safe? and adjoin-position are Θ(1). This is a reasonable assumption to
; make for estimation purposes. We don't know how Louis has implemented these
; procedures, but based on my own implementations, they likely exhibit linear or
; polynomial growth (Θ(nⁱ) for some positive i). That growth will be dominated
; by the Θ(n!) growth in the number of recursive calls as n grows large, so we
; can treat them as though they are Θ(1). (Of course, the higher the n value,
; the more supportable this assumption becomes.)
;
; Now, finally, we can calculate the relative running times of the two versions.
; For the eight-queens puzzle:
;
;   R₂₁(8) = (8 - 1)! * Σ (i=1..8) (1 / i!) ≈ 8660
;
; If the original version runs in time T then the slow version should take about
; 8660 T.
;
; Below I test R₂₁ empirically using a board size of 7 (8 takes way too long!)
; with my own implementations of safe? and adjoin-position. The predicted value
; is
;
;   R₂₁(7) = (7 - 1)! * Σ (i=1..7) (1 / i!) ≈ 1237
;
; and the observed value, 369, is within an order of magnitude.

(define (time-queens n reps)
  (define t0 (runtime))
  (define (iter count)
    (if (= count 0)
        (- (runtime) t0)
        (begin (queens n)
               (iter (- count 1)))))
  (/ (iter reps) reps))

; using original version
;(define time-orig (time-queens 7 100)) ; .04419999999999998

; using Louis's version
;(define time-slow (time-queens 7 1)) ; 16.309999999999995

; ratio
; (/ time-slow time-orig) ; 369.00452488687785


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


;; EXERCISE 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


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
(define (split repeat-large repeat-small)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split repeat-large repeat-small) painter (- n 1))))
           (repeat-large painter (repeat-small smaller smaller))))))

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


;; EXERCISE 2.46
(define (make-vect xcor ycor) (cons xcor ycor))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v w)
  (make-vect (+ (xcor-vect v) (xcor-vect w))
             (+ (ycor-vect v) (ycor-vect w))))

(define (sub-vect v w)
  (make-vect (- (xcor-vect v) (xcor-vect w))
             (- (ycor-vect v) (ycor-vect w))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

;; EXERCISE 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (cddr f))


;; Painters

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))


;; EXERCISE 2.48
(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

;; EXERCISE 2.49
; a.
(define outline
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 0 1))
                           (make-segment (make-vect 0 1) (make-vect 1 1))
                           (make-segment (make-vect 1 1) (make-vect 1 0))
                           (make-segment (make-vect 1 0) (make-vect 0 0)))))

; b.
(define x
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                           (make-segment (make-vect 0 1) (make-vect 1 0)))))

; c.
(define diamond
  (segments->painter (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                           (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
                           (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
                           (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))))

; d.
(define wave
  (segments->painter
    (list ; bottom-left path
          (make-segment (make-vect 0.2 0.0) (make-vect 0.4 0.5))
          (make-segment (make-vect 0.4 0.5) (make-vect 0.3 0.6))
          (make-segment (make-vect 0.3 0.6) (make-vect 0.2 0.4))
          (make-segment (make-vect 0.2 0.4) (make-vect 0.0 0.6))
          ; top-left path
          (make-segment (make-vect 0.0 0.8) (make-vect 0.2 0.6))
          (make-segment (make-vect 0.2 0.6) (make-vect 0.3 0.7))
          (make-segment (make-vect 0.3 0.7) (make-vect 0.4 0.7))
          (make-segment (make-vect 0.4 0.7) (make-vect 0.3 0.85))
          (make-segment (make-vect 0.3 0.85) (make-vect 0.4 1.0))
          ; top-right path
          (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.85))
          (make-segment (make-vect 0.7 0.85) (make-vect 0.6 0.7))
          (make-segment (make-vect 0.6 0.7) (make-vect 0.7 0.7))
          (make-segment (make-vect 0.7 0.7) (make-vect 1.0 0.4))
          ; bottom-right path
          (make-segment (make-vect 1.0 0.2) (make-vect 0.6 0.6))
          (make-segment (make-vect 0.6 0.6) (make-vect 0.8 0.0))
          ; bottom-center path
          (make-segment (make-vect 0.6 0.0) (make-vect 0.5 0.3))
          (make-segment (make-vect 0.5 0.3) (make-vect 0.4 0.0)))))


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


;; EXERCISE 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter) (rotate90 (rotate90 painter)))
(define (rotate270 painter) (rotate90 (rotate180 painter)))

;; EXERCISE 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below painter1 painter2)
  (rotate270 (beside (rotate90 painter2) (rotate90 painter1))))

;; EXERCISE 2.52
; a. adding a smile
(define (wave-and-smile frame)
  (define smile
    (segments->painter
      (list (make-segment (make-vect 0.4 0.8) (make-vect 0.45 0.7))
            (make-segment (make-vect 0.45 0.7) (make-vect 0.55 0.7))
            (make-segment (make-vect 0.55 0.7) (make-vect 0.6 0.8)))))
  (wave frame)
  (smile frame))

; b. using only one copy of the up-split and right-split images instead of two
(define (corner-split-less painter n)
  (if (= n 0)
      painter
      (let ((top-left (up-split painter (- n 1)))
            (bottom-right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner)))))

; c. assemble the corners in a different pattern
(define (square-limit-alt painter n)
  (let ((combine4 (square-of-four rotate90 identity
                                  rotate180 rotate270)))
    (combine4 (corner-split painter n))))


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
;: (list 'a 'b 'c) ; (a b c)
;:
;: (list (list 'george)) ; ((george))
;:
;: (cdr '((x1 x2) (y1 y2))) ; ((y1 y2))
;:
;: (cadr '((x1 x2) (y1 y2))) ; (y1 y2)
;:
;: (pair? (car '(a short list))) ; #f
;:
;: (memq 'red '((red shoes) (blue socks))) ; #f
;:
;: (memq 'red '(red shoes blue socks)) ; (red shoes blue socks)

;; EXERCISE 2.54
(define (equal? a b)
  (or (and (symbol? a) (symbol? b)
           (eq? a b))
      (and (pair? a) (pair? b)
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))
      (and (null? a) (null? b))))

;: (equal? '(this is a list) '(this is a list))
;: (equal? '(this is a list) '(this (is a) list))

;; EXERCISE 2.55
;: (car ''abracadabra) ;Value: quote

; The special form quote and its abbreviation are evaluated recursively, so the
; whole expression evaluates as follows:
;
; (car ''abracadabra)
; (car (quote 'abracadabra))
; (car (quote (quote abracadabra)))
; (car (quote abracadabra)
; quote


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


;: (deriv '(+ x 3) 'x) ; (+ 1 0)
;: (deriv '(* x y) 'x) ; (+ (* x 0) (+ 1 y))
;: (deriv '(* (* x y) (+ x 3)) 'x)
; (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))


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


;: (deriv '(+ x 3) 'x) ; 1
;: (deriv '(* x y) 'x) ; y
;: (deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))


;; EXERCISE 2.56
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
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation
                                       (base exp)
                                       (make-sum (exponent exp) -1)))
                       (deriv (base exp) var)))
        (else
          (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

;; EXERCISE 2.57
(define (make-sum a b . as)
  (define (go as)
    (let ((number (fold-right + 0
                              (filter number? as)))
          (rest (fold-right cons '()
                            (filter (lambda (x) (not (number? x))) as))))
      (let ((terms (if (= number 0) rest (cons number rest))))
        (cond ((null? terms) 0)
              ((null? (cdr terms)) (car terms))
              (else (cons '+ terms))))))
  (go (append (list a b) as)))

(define (addend s) (cadr s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (apply make-sum (cddr s))))

(define (make-product m n . ms)
  (define (go ms)
    (let ((number (fold-right * 1
                              (filter number? ms)))
          (rest (fold-right cons '()
                            (filter (lambda (x) (not (number? x))) ms))))
      (let ((terms (cond ((= number 0) (list 0))
                         ((= number 1) rest)
                         (else (cons number rest)))))
        (cond ((null? terms) 1)
              ((null? (cdr terms)) (car terms))
              (else (cons '* terms))))))
  (go (append (list m n) ms)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (apply make-product (cddr p))))

;: (deriv '(* x y (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))

;; EXERCISE 2.58
; a. using infix notation
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

;(deriv '(x + 3) 'x) ; 1
;(deriv '(x * y) 'x) ; y
;(deriv '((x * y) * (x + 3)) 'x) ; ((x * y) + (y * (x + 3)))
;(deriv '(x + (3 * (x + (y + 2)))) 'x) ; 4

; b. infix notation with more than two arguments
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (append (if (or (number? a1) (variable? a1)) (list a1) a1)
                      (list '+)
                      (if (or (number? a2) (variable? a2)) (list a2) a2)))))

(define (sum? x) (if (and (pair? x) (memq '+ x)) #t #f))
(define (addend s) (simplify (before '+ s)))
(define (augend s) (simplify (cdr (memq '+ s))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (append (if (product? m1) m1 (list m1))
                      (list '*)
                      (if (product? m2) m2 (list m2))))))

(define (product? x) (if (and (pair? x) (not (sum? x)) (memq '* x)) #t #f))
(define (multiplier p) (simplify (before '* p)))
(define (multiplicand p) (simplify (cdr (memq '* p))))

(define (before sym exp)
  (cond ((null? exp) exp)
        ((eq? (car exp) sym) '())
        (else (cons (car exp) (before sym (cdr exp))))))

(define (simplify x)
  (if (and (pair? x) (null? (cdr x)))
      (car x)
      x))

;(deriv '(x + 3) 'x) ; 1
;(deriv '(x * y) 'x) ; y
;(deriv '((x * y) * (x + 3)) 'x) ; ((x * y) + (y * (x + 3)))
;(deriv '(x + (3 * (x + (y + 2)))) 'x) ; 4
;(deriv '(x + 3 * (x + y + 2)) 'x) ; 4


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


;; EXERCISE 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

;; EXERCISE 2.60
; element-of-set? is unchanged: Θ(n). Note that the size of n will generally be
; larger than before, since we permit duplicates.
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; adjoin-set is more efficient: previously Θ(n), now Θ(1).
(define (adjoin-set x set) (cons x set))

; intersection-set is equally efficient: Θ(n²). Again, n values will be larger
; than in the original implementation.
(define (intersection-set set1 set2)
  (filter (lambda (x) (element-of-set? x set2)) set1))

; union-set is more efficient: previously Θ(n²), now Θ(n).
(define (union-set set1 set2) (append set1 set2))

; This representation might be preferable for an application that does a lot of
; intense set-building but only needs to query the sets infrequently. For
; example, say we are tech support for a large SaaS company. When investigating
; customer firewall issues, it is occasionally helpful to have the ability to
; know whether any of our servers have received traffic from a given IP address
; over the past, say, 24 hours. So we create a service that periodically
; collects the last day's worth of logs from all the servers and exposes a
; queryable set for checking the IPs. We don't use the query functionality too
; frequently but we are constantly collecting and aggregating the data, meaning
; the bulk of our set API usage is adjoin and union operations. So we save
; computing resources by choosing the implementation where those operations are
; more efficient. However, our queries will be slower due to higher n values, so
; if we start making frequent queries, this implementation choice might be worth
; revisiting.


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

;; EXERCISE 2.61
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((<= x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;;EXERCISE 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2) (union-set (cdr set1) set2))
                  ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                  (else (cons x2 (union-set set1 (cdr set2)))))))))


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

; a.
; tree->list-1 and tree->list-2 produce identical results for all trees.

(define tree1 (make-tree 7 (make-tree 3 (make-tree 1 '() '())
                                        (make-tree 5 '() '()))
                           (make-tree 9 '()
                                        (make-tree 11 '() '()))))

(define tree2 (make-tree 3 (make-tree 1 '() '())
                           (make-tree 7 (make-tree 5 '() '())
                                        (make-tree 9 '()
                                                     (make-tree 11 '() '())))))

(define tree3 (make-tree 5 (make-tree 3 (make-tree 1 '() '())
                                        '())
                           (make-tree 9 (make-tree 7 '() '())
                                        (make-tree 11 '() '()))))

;(tree->list-1 tree1) ; (1 3 5 7 9 11)
;(tree->list-2 tree1) ; (1 3 5 7 9 11)
;(tree->list-1 tree2) ; (1 3 5 7 9 11)
;(tree->list-2 tree2) ; (1 3 5 7 9 11)
;(tree->list-1 tree3) ; (1 3 5 7 9 11)
;(tree->list-2 tree3) ; (1 3 5 7 9 11)

; b.
; The orders of growth in the number of steps differ. tree->list-1 is Θ(n²).
; In each iteration we make two recursive calls with arguments of size n/2, and
; we also traverse a list of size n/2 with append. So the total number of steps
; approaches
;
;   n/2 + 2(n/4) + 4(n/8) ... 2ⁿn/2ⁿ⁺¹ = Σ (i=1..n) i/2 = (n² + n)/4
;
; which gives us Θ(n²) as we only care about the most significant power of n.
;
; tree->list-2 is Θ(n), because at each iteration it cons's one additional tree
; entry onto the result list until the entire tree has been consumed.

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

; a.
; To understand partial-tree, think of elts as the concatenation of four
; sublists:
;
;   (a) elements in the left branch of the balanced tree;
;   (b) the middle value (entry) of the balanced tree;
;   (c) elements in the right branch of the balanced tree;
;   (d) elements excluded from the balanced tree.
;
; (a) and (c) each account for half the elements in the balanced tree other than
; its entry, and their sizes are chosen to reflect this. partial-tree
; recursively balances (a) and (c) and builds the balanced tree by connecting
; them with (b). The balanced tree and (d) are returned as the result.

; b.
; The order of growth in number of steps for a list of n elements is Θ(n). Each
; iteration does the same steps so the number of steps is proportional to the
; total number of recursive calls. Each iteration makes two recursive calls with
; n cut in half. So the recursion depth i grows as (log₂ n) while the number of
; recursive calls grows as 2ⁱ = 2^(log₂ n) = n, yielding Θ(n).

;; EXERCISE 2.65
(define (union-set set1 set2)
  (define (iter list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          (else
            (let ((hd1 (car list1)) (hd2 (car list2)))
              (cond ((= hd1 hd2) (iter (cdr list1) list2))
                    ((< hd1 hd2) (cons hd1 (iter (cdr list1) list2)))
                    (else (cons hd2 (iter list1 (cdr list2)))))))))
  (list->tree (iter (tree->list-2 set1)
                    (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (define (iter list1 list2)
    (display (list 'iter list1 list2)) (newline)
    (if (or (null? list1) (null? list2))
        '()
        (let ((hd1 (car list1)) (hd2 (car list2)))
          (cond ((< hd1 hd2) (iter (cdr list1) list2))
                ((> hd1 hd2) (iter list1 (cdr list2)))
                (else (cons hd1 (iter (cdr list1) (cdr list2))))))))
  (list->tree (iter (tree->list-2 set1)
                    (tree->list-2 set2))))


;; INFORMATION RETRIEVAL

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))


;; EXERCISE 2.66
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let (entry (entry set-of-records))
        (cond ((< given-key (key entry))
               (lookup given-key (left-branch set-of-records)))
              ((> given-key (key entry))
               (lookup given-key (right-branch set-of-records)))
              (else entry)))))


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

;(decode sample-message sample-tree) ; (a d a b b c a)

;; EXERCISE 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (iter bits tree)
    (if (memq symbol (symbols tree))
        (cond ((leaf? tree) bits)
              ((memq symbol (symbols (left-branch tree)))
               (iter (cons 0 bits) (left-branch tree)))
              (else (iter (cons 1 bits) (right-branch tree))))))
  (reverse (iter '() tree)))

(equal? (encode (decode sample-message sample-tree)
                sample-tree)
        sample-message) ; #t

;; EXERCISE 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (define (insert tree set)
    (if (or (null? set)
            (<= (weight tree) (weight (car set))))
        (cons tree set)
        (cons (car set) (insert tree (cdr set)))))
  (cond ((null? leaf-set) (error "leaf-set is empty"))
        ((null? (cdr leaf-set)) (car leaf-set))
        (else (successive-merge (insert (make-code-tree (cadr leaf-set)
                                                        (car leaf-set))
                                        (cddr leaf-set))))))

;; EXERCISE 2.70
(define pairs
  '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1)))

(define plaintext
  '(Get a job
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip yip yip yip yip yip
    Sha boom))

(define tree (generate-huffman-tree pairs))

(define message (encode plaintext tree))
; (1 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 1 1 1 1 0
;  1 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1
;  0 1 1 0 0 1)

(length message) ; 84

(* (length plaintext) (/ (log (length pairs)) (log 2))) ; 108

; The Huffman encoding requires 84 bits, whereas 108 bits would be the
; minimum required for a fixed-length encoding.

;; EXERCISE 2.71
(define (sketch-tree n)
  (define symbols '(a b c d e f g h i j))
  (define (display+ . xs) (map display xs))
  (define (iter syms indent)
    (display+ "{" (car syms))
    (map (lambda (sym) (display+ " " sym))
                       (cdr syms))
    (display+ "} " (- (expt 2 (length syms)) 1))
    (newline)
    (indent)
    (display+ "├─" (car syms) " " (expt 2 (- (length syms) 1)))
    (newline)
    (indent)
    (display+ "└─")
    (if (null? (cddr syms))
        (display+ (cadr syms) " 1")
        (iter (cdr syms) (lambda () (display "  ")
                                    (indent)))))
  (iter (sublist symbols 0 n)
        (lambda () #t)))

; The procedure above generates a visualization of the Huffman tree for n
; symbols with relative frequencies 1, 2, 4, ..., 2ⁿ⁻¹.
;
; (sketch-tree 5)
;
;   {a b c d e} 31
;   ├─a 16
;   └─{b c d e} 15
;     ├─b 8
;     └─{c d e} 7
;       ├─c 4
;       └─{d e} 3
;         ├─d 2
;         └─e 1
;
; (sketch-tree 10)
;
;   {a b c d e f g h i j} 1023
;   ├─a 512
;   └─{b c d e f g h i j} 511
;     ├─b 256
;     └─{c d e f g h i j} 255
;       ├─c 128
;       └─{d e f g h i j} 127
;         ├─d 64
;         └─{e f g h i j} 63
;           ├─e 32
;           └─{f g h i j} 31
;             ├─f 16
;             └─{g h i j} 15
;               ├─g 8
;               └─{h i j} 7
;                 ├─h 4
;                 └─{i j} 3
;                   ├─i 2
;                   └─j 1
;
; In trees with this pattern of frequencies, it takes 1 bit to encode the most
; frequent symbol and (n-1) bits to encode the least frequent symbol.

;; EXERCISE 2.72
(define (encode-symbol symbol tree)
  (define (iter bits tree)
    (if (memq symbol (symbols tree))
        (cond ((leaf? tree) bits)
              ((memq symbol (symbols (left-branch tree)))
               (iter (cons 0 bits) (left-branch tree)))
              (else (iter (cons 1 bits) (right-branch tree))))))
  (reverse (iter '() tree)))

; For encode-symbol from Ex. 2.68, when the relative frequencies of the n
; symbols are as in Ex. 2.71, and limiting ourselves to trees generated by
; the successive-merge implementation given in Ex. 2.69, the order of growth of
; the number of steps to encode the most frequent symbol is Θ(1). This symbol is
; always located in the left branch of the toplevel tree in and so its
; single-digit code is always reached in a fixed number of steps.
;
; The order of growth for encoding the least frequent symbol is Θ(n²). On each
; iteration encode-symbol performs an Θ(n) memq check, does a couple fixed-cost
; operations (checking the first two clauses of cond), and then calls itself
; recursively on the right branch, which reduces n by one. The Θ(n) operation
; dominates the others, and thus the total number of steps grows as
; n + (n - 1) + (n - 2) ... 1 = Σ (i=1..n) i = (n² + n)/2, giving Θ(n²).


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

  ;; added from exercise 2.79
  (put 'equ? '(rectangular rectangular)
    (lambda (x y) (and (= (real-part x) (real-part y))
                       (= (imag-part x) (imag-part y)))))

  ;; added from exercise 2.80
  (put '=zero? '(rectangular)
    (lambda (x) (and (= (real-part x) 0)
                     (= (imag-part x) 0))))
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

  ;; added from exercise 2.79
  (put 'equ? '(polar polar)
    (lambda (x y) (and (= (magnitude x) (magnitude y))
                       (= (angle x) (angle y)))))

  ;; added from exercise 2.80
  (put 'zero? '(polar) (lambda (x) (= (magnitude x) 0)))
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

; a.
; The sum? and product? predicates of the conditional have been replaced by a
; lookup into the dispatch table. This only works for expressions that are
; operations and have an operator symbol that can be used as a type tag. Numbers
; and variables aren't operations, don't have an operator symbol, and thus can't
; be handled generically in this way without further changes.

; b.
(define (install-derivatives-package)
  ;; internal procedures
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (define (deriv-product exp var)
    (make-sum
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                    (multiplicand exp))))

  ;; interface to the rest of the system
  (register-deriv '+ deriv-sum)
  (register-deriv '* deriv-product))

(define (register-deriv op impl)
  (put 'deriv op (lambda (operands var)
                   (impl (list op (car operands) (cadr operands)) var))))

;(install-derivatives-package)

;(deriv '(+ x 3) 'x) ; 1
;(deriv '(* x y) 'x) ; y
;(deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))

; c.
(define (install-deriv-exponentiation)
  (define (deriv-exponentiation exp var)
    (make-product (make-product
                    (exponent exp)
                    (make-exponentiation (base exp)
                                         (make-sum (exponent exp) -1)))
                  (deriv (base exp) var)))
  (register-deriv '** deriv-exponentiation))

;(install-deriv-exponentiation)

;(deriv '(** x 3) 'x) ; (* 3 (** x 2))
;(deriv '(+ (* 5 (** x 3)) (* 4 x)) 'x) ; (+ (* 5 (* 3 (** x 2))) 4)

; d.
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp) var))))

; The order of the first two arguments in all invocations of put would have to
; be reversed (e.g. in the register-deriv procedure above). No other changes
; are necessary to accommodate the new version of deriv, as shown below.

(define (register-deriv op impl)
  (put op 'deriv (lambda (operands var)
                   (impl (list op (car operands) (cadr operands)) var))))

;(install-derivatives-package)
;(install-deriv-exponentiation)

;(deriv '(+ x 3) 'x) ; 1
;(deriv '(* x y) 'x) ; y
;(deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))
;(deriv '(** x 3) 'x) ; (* 3 (** x 2))
;(deriv '(+ (* 5 (** x 3)) (* 4 x)) 'x) ; (+ (* 5 (* 3 (** x 2))) 4)

;;EXERCISE 2.74
; To run the examples, use the data-directed implementations of sets in
; ch2tests.scm and these sample data files:
;(define file1 "ch2-data-files/section-2.4.ex-74.data1")
;(define file2 "ch2-data-files/section-2.4.ex-74.data2")

; a.
; The contents of a personnel data file should consist of a single Scheme value,
; a pair of the form (type-tag . set) where type-tag refers to an available
; implementation of sets, and set is a set of employee records that conforms to
; that implementation. Valid set implementations should expose a lookup
; procedure via the data-directed dispatch table. (lookup key set) should return
; data in the form of a pair (key . value), or #f if the requested key wasn't
; found.

(define (get-record file-path employee-name)
  (with-input-from-file file-path
    (lambda ()
      (let ((set (read)))
        (if (eof-object? set)
            (error "file format incorrect" file-path)
            (lookup employee-name set))))))

;(get-record file1 "Jane Trent")
; ("Jane Trent" tree-set (position . "prod mgr") () ((salary . 150000) () ()))

;(get-record file2 "Betty Burn")
; ("Betty Burn" ordered-list-set (position . "researcher") (salary . 160000))

; b.
; Each employee record should be a pair of the form (type-tag . set) where,
; again, type-tag refers to an available implementation of sets, and set is a
; set of employee records conforming to that implementation.

(define (get-salary file-path employee-name)
  (let ((record (get-record file-path employee-name)))
    (if (not record)
        (error "employee record not found" employee-name)
        (let ((pair (lookup 'salary (cdr record))))
          (if pair (cdr pair) false)))))

;(get-salary file1 "Jane Trent") ; 150000
;(get-salary file2 "Betty Burn") ; 160000

; c.
(define (find-employee-record employee-name division-files)
  (if (null? division-files)
      false
      (let ((record (get-record (car division-files) employee-name)))
        (if record
            record
            (find-employee-record employee-name (cdr division-files))))))

;(find-employee-record "Jane Trent" (list file1 file2))
; ("Jane Trent" tree-set (position . "prod mgr") () ((salary . 150000) () ()))

;(find-employee-record "Betty Burn" (list file1 file2))
; ("Betty Burn" ordered-list-set (position . "researcher") (salary . 160000))

; d.
; When Insatiable takes over a new company it must do the following to
; integrate new personnel information:
;
;   - Ensure the new division's personnel files are implemented as Scheme sets
;     and meet the structural requirements expressed in (a) and (b) above.
;
;   - If the new files don't use an existing implementation of sets, either for
;     the files themselves or for the individual records, ensure that any novel
;     implementations they use are packaged up as installation procedures
;     compatible with data-directed dispatch using get/put, and ensure the
;     installers are called in the initialization section of our
;     personnel-processing application.
;
;   - Ensure the new division's files are accessible on the company-wide shared
;     filesystem.


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


;; EXERCISE 2.75
(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* (magnitude z) (cos (angle z))))
          ((eq? op 'imag-part) (* (magnitude z) (sin (angle z))))
          ((eq? op 'magnitude) (car z))
          ((eq? op 'angle) (cdr z))
          (else
            (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; EXERCISE 2.76
; For generic operations with explicit dispatch, adding a new type requires
; modifying every generic procedure to handle the new type. Adding a new
; operation requires implementing a single procedure with logic to handle all
; existing types.
;
; For data-directed style, adding a new type requires writing a procedure to
; implement every existing operation for the new type, as well as creating an
; installation procedure which registers these procedures. Adding a new
; operation requires writing procedures to implement that operation for all
; existing types, and updating all existing types' registration code to register
; those procedures.
;
; For message-passing style, adding a new type requires implementing a single
; procedure with logic to handle all existing operations. Adding a new operation
; requires modifying all existing types' implementation procedures to handle the
; new operation.
;
; In a system where new types must often be added, the data-directed and
; message-passing styles are better suited, as the addition of a new type does
; not require changes to existing code. (The data-directed style may be slightly
; nicer than message-passing because it cleanly separates the dispatch logic
; from the application logic.)
;
; If new operations must often be added, then generic operations with explicit
; dispatch will be best, for a similar reason -- existing code does not have to
; move to accommodate the new operations.


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

  ;; added from exercise 79
  (put 'equ? '(scheme-number scheme-number) =)

  ;; added from exercise 80
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))

  ;; added from exercise 86
  (put 'square '(scheme-number) square)
  (put 'sqrt '(scheme-number) sqrt)
  (put 'sin '(scheme-number) sin)
  (put 'cos '(scheme-number) cos)
  (put 'atan '(scheme-number scheme-number) atan)
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

   ;; added from exercise 79
  (put 'equ? '(rational rational)
    (lambda (x y)
      (= (/ (numer x) (denom x))
         (/ (numer y) (denom y)))))

  ;; added from exercise 80
  (put '=zero? '(rational) (lambda (x) (= (numer x) 0)))

  ;; added from exercise 86 (requires apply-rats to use)
  (put 'square '(rational) (lambda (x) (attach-tag 'rational (mul-rat x x))))
  (put 'sqrt '(rational) (lambda (x) ((apply-rats sqrt) x)))
  (put 'sin '(rational) (lambda (x) ((apply-rats sin) x)))
  (put 'cos '(rational) (lambda (x) ((apply-rats cos) x)))
  (put 'atan '(rational rational) (lambda (x y) ((apply-rats atan) x y)))
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

  ;; added from exercise 77
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  ;; added from exercise 79
  (put 'equ? '(complex complex)
    (lambda (x y)
      (and (= (real-part x) (real-part y))
           (= (imag-part x) (imag-part y)))))

  ;; added from exercise 80
  (put '=zero? '(complex) =zero?)
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

; For convenience:
;
; (define x (cons 3 4))
; (define y (cons 'rectangular x))
; (define z (cons 'complex y))
;
; Evaluating (magnitude z) with the definitions in place, the procedure calls
; are as follows. ─> denotes a return value.
;
;   (magnitude z)
;   ├─(apply-generic 'magnitude z)
;   │ ├─(map type-tag '(z))
;   │ │ ├─(type-tag '(z))
;   │ │ │ ├─(pair? '(z)) ─> true
;   │ │ │ ├─(car '(z)) ─> 'complex
;   │ │ │ └─> 'complex
;   │ │ └─> '(complex)
;   │ ├─(get 'magnitude '(complex)) ─> magnitude
;   │ ├─(map contents '(z))
;   │ │ ├─(cdr z) ─> y
;   │ │ └─> '(y)
;   │ ├─(apply magnitude '(y))
;   │ │ ├─(magnitude y)
;   │ │ │ ├─(apply-generic 'magnitude y)
;   │ │ │ │ ├─(map type-tag '(y))
;   │ │ │ │ │ ├─(type-tag '(y))
;   │ │ │ │ │ │ ├─(pair? '(y)) ─> true
;   │ │ │ │ │ │ ├─(car '(y)) ─> 'rectangular
;   │ │ │ │ │ │ └─> 'rectangular
;   │ │ │ │ │ └─> '(rectangular)
;   │ │ │ │ ├─(get 'magnitude '(rectangular)) ─> magnitude-from-rect-pkg
;   │ │ │ │ ├─(map contents '(y))
;   │ │ │ │ │ ├─(cdr 'y) ─> x
;   │ │ │ │ │ └─> '(x)
;   │ │ │ │ ├─(apply magnitude-from-rect-pkg '(x))
;   │ │ │ │ │ ├─(magnitude-from-rect-pkg x)
;   │ │ │ │ │ │ ├─(real-part x)
;   │ │ │ │ │ │ │ ├─(car x) ─> 3
;   │ │ │ │ │ │ │ └─> 3
;   │ │ │ │ │ │ ├─(square 3) ─> 9
;   │ │ │ │ │ │ ├─(imag-part x)
;   │ │ │ │ │ │ │ ├─(cdr x) ─> 4
;   │ │ │ │ │ │ │ └─> 4
;   │ │ │ │ │ │ ├─(square 4) ─> 16
;   │ │ │ │ │ │ ├─(+ 9 16) ─> 25
;   │ │ │ │ │ │ ├─(sqrt 25) ─> 5
;   │ │ │ │ │ │ └─> 5
;   │ │ │ │ │ └─> 5
;   │ │ │ │ └─> 5
;   │ │ │ └─> 5
;   │ │ └─> 5
;   │ └─> 5
;   └─> 5
;
; apply-generic is invoked twice. The first time, it determines the type of the
; arguments to be '(complex) and uses this to successfully look up the magnitude
; procedure, to which it dispatches control. magnitude in turn calls
; apply-generic, which this time determines the type of the arguments to be
; '(rectangular) and uses this to find and dispatch to the internal magnitude
; procedure defined inside install-rectangular-package. This procedure
; calculates the magnitude of the rectangularly-represented complex number
; as expected.


;;EXERCISE 2.78
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      (if (number? contents)
          contents
          (error "Not a Scheme number -- ATTACH-TAG" contents))
      (cons type-tag contents)))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))


;;EXERCISE 2.79
(define (equ? a b) (apply-generic 'equ? a b))

; add to install-scheme-number-package
;: (put 'equ? '(scheme-number scheme-number) =)

; add to install-rational-package
;: (put 'equ? '(rational rational)
;:   (lambda (x y)
;:     (= (/ (numer x) (denom x))
;:        (/ (numer y) (denom y)))))

;; add to install-rectangular-package
;(put 'equ? '(rectangular rectangular)
;  (lambda (x y) (and (= (real-part x) (real-part y))
;                     (= (imag-part x) (imag-part y)))))

;; add to install-polar-package
;(put 'equ? '(polar polar)
;  (lambda (x y) (and (= (magnitude x) (magnitude y))
;                     (= (angle x) (angle y)))))

; add to install-complex-package
;: (put 'equ? '(complex complex)
;:   (lambda (x y)
;:     (and (= (real-part x) (real-part y))
;:          (= (imag-part x) (imag-part y)))))

;;EXERCISE 2.80
(define (=zero? x) (apply-generic '=zero? x))

; add to install-scheme-number-package
;: (put '=zero? '(scheme-number) (lambda (x) (= x 0)))

; add to install-rational-package
;: (put '=zero? '(rational) (lambda (x) (= (numer x) 0)))

;; add to install-rectangular-package
;(put '=zero? '(rectangular)
;  (lambda (x) (and (= (real-part x) 0)
;                   (= (imag-part x) 0))))

;; add to install-polar-package
;(put 'zero? '(polar) (lambda (x) (= (magnitude x) 0)))

; add to install-complex-package
;: (put '=zero? '(complex) =zero?)


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

;; following added to Scheme-number package
;: (put 'exp '(scheme-number scheme-number)
;:      (lambda (x y) (tag (expt x y))))

; a.
; If we call exp with two complex numbers as arguments, apply-generic will enter
; an infinite loop. After it initially checks and fails to find a matching
; procedure in the dispatch table, it looks up the coercion procedures for
; converting between the arguments. Since these are now (incorrectly) present in
; the table, it finds them. Then it calls a coercion procedure on the first
; argument, yielding the first argument again, and recursively calls itself
; arguments. This is identical to the previous apply-generic call, and the
; process repeats indefinitely. (It will not overflow since apply-generic is
; tail-recursive.)

; b.
; Louis is right to be concerned about coercion with arguments of the same type.
; If apply-generic tries to apply such coercions, we run the risk of the problem
; described in (a) if a matching coercion happens to be present in the coercion
; table. That *should* only happen due to programmer error; however, it doesn't
; hurt to ignore these coercions if present, and indeed it will make the intent
; of apply-generic's code clearer if we explicitly don't look for them.

; c.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2)
                   (not (eq? (car type-tags) (cadr type-tags))))
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


;; EXERCISE 2.82
;; Requires the rational number procedures from section 2.1.1.
;; Requires the implementation of sets as unordered lists from section 2.3.3.
;;
;; As an example of when trying to coerce all arguments to the same type is
;; inadequate, imagine we have extended the rationals package with a procedure
;; that computes the nth term in a geometric series. The parameters of the
;; series are to be supplied as rationals but n as an ordinary number. If we
;; call geom-series with an ordinary number as the first or second argument, we
;; need the dispatch mechanism to find the '(rational, rational, scheme-number)
;; implementation and coerce the arguments accordingly. But under the proposed
;; coercion strategy, it only looks for '(rational, rational, rational) and
;; '(scheme-number, scheme-number, scheme-number) implementations. This fails
;; to turn up a matching procedure and so we get an error.

(define (geom-series a r n)
  (apply-generic 'geom-series a r n))

(define (install-geom-series-extension)
  (define (geom-series a r n)
    (if (<= n 1)
        a
        (geom-series (mul-rat a r) r (- n 1))))
  (define (tag x) (attach-tag 'rational x))
  (put 'geom-series '(rational rational scheme-number)
     (lambda (a r n) (tag (geom-series a r n)))))

(define (scheme-number->rational x)
  (let ((x2 (inexact->exact x)))
    (make-rational (numerator x2) (denominator x2))))

;(install-geom-series-extension)
;(put-coercion 'scheme-number 'rational scheme-number->rational)

;(geom-series (make-rational 1 2) (make-rational 1 2) 3)
;Value: (rational 1 . 8)

;(geom-series 1 2 3)
;No method for these types

;(geom-series 1 (make-rational 2 1) 3)
;No method for these types

;; The following implementation of apply-generic attempts all possible coercions
;; among the types of the passed arguments. It will still fail in situations
;; where the only implementation(s) of the requested operation require a type
;; that has not been passed in the argument list but for which a coercion from
;; the passed type is available. Thus, even with the below implementation,
;; (geom-series 1 2 3) still fails but (geom-series 1 (make-rational 2 1) 3)
;; works, assuming a coercion from scheme-number to rational is available.

(define (apply-generic op . args)
  (define (possible-sigs types)
    (define (iter unique-types len)
      (if (< len 1)
          (list nil)
          (flatmap
            (lambda (ts)
              (map
                (lambda (t) (cons t ts))
                unique-types))
            (iter unique-types (- len 1)))))
    (iter (uniques types) (length types)))
  (define (make-list-coercion sig1 sig2)
    (if (or (null? sig1) (null? sig2))
        (lambda (items) nil)
        (let ((type1 (car sig1)) (type2 (car sig2)))
          (let ((coerce-item (if (eq? type1 type2)
                                 identity
                                 (get-coercion type1 type2))))
            (if coerce-item
                (let ((coerce-tail
                       (make-list-coercion (cdr sig1) (cdr sig2))))
                  (if coerce-tail
                      (lambda (items)
                        (cons (coerce-item (car items))
                              (coerce-tail (cdr items))))
                      #f))
                #f)))))
  (define (make-apply-proc type-tags sig)
    (let ((proc (get op sig))
          (coerce-args (make-list-coercion type-tags sig)))
      (if (and proc coerce-args)
          (lambda () (apply proc (map contents (coerce-args args))))
          false)))
  (define (iter type-tags sigs)
    (if (null? sigs)
        (error "No method for these types" (list op type-tags))
        (let ((thunk (make-apply-proc type-tags (car sigs))))
          (if thunk (thunk) (iter type-tags (cdr sigs))))))
  (let ((type-tags (map type-tag args)))
    (let ((sigs (cons type-tags
                      (filter (lambda (sig) (not (equal? sig type-tags)))
                              (possible-sigs type-tags)))))
      (iter type-tags sigs))))

(define (uniques xs)
  (fold-left (lambda (xs x)
               (if (element-of-set? x xs)
               xs
               (cons x xs)))
             nil
             xs))

;(geom-series (make-rational 1 2) (make-rational 1 2) 3)
;Value: (rational 1 . 8)

;(geom-series 1 2 3)
;No method for these types

;(geom-series 1 (make-rational 2 1) 3)
;Value: (rational 4 . 1)


;; EXERCISE 2.83
;; The instructions imply the availability of integer and real types in the
;; data-directed dispatch system, so I have added them. I have chosen to
;; represent them explicitly with type tags instead of piggybacking on Scheme's
;; internal type system as in exercise 2.78.

(define (make-integer x) ((get 'make 'integer) x))

(define (install-integer-package)
  (define (make-int x) (if (integer? x) x (error "Not an integer" x)))
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer) (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer) make-rational)
  (put 'make 'integer (lambda (x) (tag (make-int x))))
  ;; added from exercise 85
  (put 'equ? '(integer integer) =))

(define (make-real x) ((get 'make 'real) x))

(define (install-real-package)
  (define (make-real x)
    (if (number? x)
        (exact->inexact x)
        (error "Not a real" x)))
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real) (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real) (lambda (x y) (tag (- x y))))
  (put 'mul '(real real) (lambda (x y) (tag (* x y))))
  (put 'div '(real real) (lambda (x y) (tag (/ x y))))
  (put 'make 'real (lambda (x) (tag (make-real x))))
  ;; added from exercise 85
  (put 'equ? '(real real) =))

(define (raise x) (apply-generic 'raise x))

(define (install-tower-package)
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  (put 'raise '(rational) (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'raise '(real) (lambda (x) (make-complex-from-real-imag x 0.0))))


;; EXERCISE 2.84
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((fail
           (lambda () (error "No method for these types"
                             (list op type-tags)))))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (if (= (length args) 2)
                (let ((pair (try-raise (car args) (cadr args))))
                  (if pair
                      (let ((args2 (list (car pair) (cdr pair))))
                        (let ((proc2 (get op (map type-tag args2))))
                          (if proc2
                              (apply proc2 (map contents args2))
                              (fail))))
                      (fail)))
                (fail)))))))

;; Raise a to the same type as b, returning #f if this is not possible.
(define (try-raise a b)
  (define (iter a b)
    (if (eq? (type-tag a) (type-tag b))
        (cons a b)
        (let ((proc (get 'raise (list (type-tag a)))))
          (if proc
              (iter (proc (contents a)) b)
              false))))
  (let ((pair (iter a b)))
    (if pair
        pair
        (let ((pair2 (iter b a)))
          (if pair2 (flip-pair pair2) false)))))

(define (flip-pair p)
  (cons (cdr p) (car p)))


;; EXERCISE 2.85
;; Requires the equ? implementation from exercise 2.79. I have added
;; implementations for integers and reals.

;; add to install-integer-package
;(put 'equ? '(integer integer) =)

;; add to install-real-package
;(put 'equ? '(real real) =)

(define (drop x)
  (if (get 'project (list (type-tag x)))
      (let ((projection (project x)))
        (if (and (not (eq? (type-tag projection) (type-tag x)))
                 (equ? (raise projection) x))
            (drop projection)
            x))
      x))

(define (project x) (apply-generic 'project x))

(define (install-projection-package)
  (define (real->rational x)
    (let ((frac (inexact->exact x)))
      (make-rational (numerator frac) (denominator frac))))

  (define (rational->integer x)
    (make-integer (round (/ (numer x) (denom x)))))

  (put 'project '(complex) (lambda (x) (make-real (real-part x))))
  (put 'project '(real) (lambda (x) (real->rational x)))
  (put 'project '(rational) (lambda (x) (rational->integer x)))
  (put 'project '(integer) (lambda (x) (make-integer x))))

(define (apply-generic op . args)
  ;; avoid infinite loops
  (define (finalize x)
    (if (and (not (eq? op 'project))
             (not (eq? op 'raise)))
        (drop x)
        x))
  (let ((type-tags (map type-tag args)))
    (let ((fail
           (lambda () (error "No method for these types"
                             (list op type-tags)))))
      (let ((proc (get op type-tags)))
        (if proc
            (finalize (apply proc (map contents args)))
            (if (= (length args) 2)
                (let ((pair (try-raise (car args) (cadr args))))
                  (if pair
                      (let ((args2 (list (car pair) (cdr pair))))
                        (let ((proc2 (get op (map type-tag args2))))
                          (if proc2
                              (finalize (apply proc2 (map contents args2)))
                              (fail))))
                      (fail)))
                (fail)))))))

;; EXERCISE 2.86
;; Requires the original implementation of apply-generic from section 2.5.2.
;; Requires equ? from exercise 2.79.

;; To allow complex numbers whose components can be ordinary or rational
;; numbers, the low-level procedures used to implement operations on complex
;; numbers (square, sqrt, sin, cos atan) have to be implemented for ordinary and
;; rational numbers and registered as generic operations in the dispatch table.
;; The implementations in the rectangular, polar, and complex packages then must
;; be updated to use these generic operations.

(define (square-generic x) (apply-generic 'square x))
(define (sqrt-generic x) (apply-generic 'sqrt x))
(define (sin-generic x) (apply-generic 'sin x))
(define (cos-generic x) (apply-generic 'cos x))
(define (atan-generic x y) (apply-generic 'atan x y))

;; Make a procedure that takes rational arguments, converts them to ordinary
;; numbers, applies f to the list, then converts the result back from an
;; ordinary number to a rational if possible.
(define (apply-rats f)
  (lambda xs
    (let ((result
           (apply f (map (lambda (x) (/ (numer x) (denom x)))
                         xs))))
      (if (exact? result)
          (make-rational (numerator result) (denominator result))
          (make-scheme-number result)))))

;; add to install-scheme-number-package
;(put 'square '(scheme-number) square)
;(put 'sqrt '(scheme-number) sqrt)
;(put 'sin '(scheme-number) sin)
;(put 'cos '(scheme-number) cos)
;(put 'atan '(scheme-number scheme-number) atan)

;; add to install-rational-package
;(put 'square '(rational) (lambda (x) (attach-tag 'rational (mul-rat x x))))
;(put 'sqrt '(rational) (apply-rats sqrt))
;(put 'sin '(rational) (apply-rats sin))
;(put 'cos '(rational) (apply-rats cos))
;(put 'atan '(rational rational) (apply-rats atan))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt-generic (add (square-generic (real-part z))
                       (square-generic (imag-part z)))))
  (define (angle z)
    (atan-generic (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cos-generic a)) (mul r (sin-generic a))))

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

  ;; added from exercise 2.79
  (put 'equ? '(rectangular rectangular)
    (lambda (x y) (and (equ? (real-part x) (real-part y))
                       (equ? (imag-part x) (imag-part y)))))

  ;; added from exercise 2.80
  (put '=zero? '(rectangular)
    (lambda (x) (and (=zero? (real-part x))
                     (=zero? (imag-part x)))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cos-generic (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sin-generic (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt-generic (add (square-generic x) (square-generic y)))
          (atan-generic y x)))

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

  ;; added from exercise 2.79
  (put 'equ? '(polar polar)
    (lambda (x y) (and (equ? (magnitude x) (magnitude y))
                       (equ? (angle x) (angle y)))))

  ;; added from exercise 2.80
  (put 'zero? '(polar) (lambda (x) (equ? (magnitude x) 0)))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))

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

  ;; added from exercise 2.77
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  ;; added from exercise 2.79
  (put 'equ? '(complex complex)
    (lambda (x y)
      (and (equ? (real-part x) (real-part y))
           (equ? (imag-part x) (imag-part y)))))

  ;; added from exercise 2.80
  (put '=zero? '(complex) =zero?)
  'done)


;;;SECTION 2.5.3

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? x) (symbol? x))

  ;; Representation of terms and term lists is kept external to this procedure
  ;; so it can be reimplemented independently.

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

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

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

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

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

  ;; EXERCISE 2.87
  (define (=zero?-poly p)
    (or (empty-termlist? (term-list p))
        (= 0 (coeff (first-term (term-list p))))))
  (put '=zero? '(polynomial) =zero?-poly)

  ;; EXERCISE 2.88
  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))
  (define (neg-poly p)
    (if (=zero?-poly p)
        p
        (mul-poly p (make-poly (variable p) '((0 -1))))))
  (put 'neg '(polynomial) (lambda (p) (tag (neg-poly p))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))

  ;; EXERCISE 2.91
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((result (div-terms (term-list p1) (term-list p2))))
          (list (make-poly (variable p1) (car result))
                (make-poly (variable p1) (cadr result))))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))

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
                       (div-terms
                         (add-terms
                           L1
                           (mul-term-by-all-terms
                             (make-term 0 -1)
                             (mul-term-by-all-terms (make-term new-o new-c)
                                                    L2)))
                         L2)))
                  (list (adjoin-term (make-term new-o new-c)
                                     (car rest-of-result))
                        (cadr rest-of-result))))))))

  (put 'div '(polynomial polynomial)
    (lambda (p1 p2)
      (let ((result (div-poly p1 p2)))
        (list (tag (car result)) (tag (cadr result))))))
  'done)


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


;; EXERCISES 2.87-88 see install-polynomial-package above


;; EXERCISE 2.89
;; Representation of individual terms (make-term, order, coeff) is unchanged.

;; Per the text we assume term is higher-ordered than term-list.
(define (adjoin-term term term-list)
  (cond ((=zero? (coeff term)) term-list)
        ((= (order term) (length term-list))
         (cons (coeff term) term-list))
        (else (adjoin-term term (cons 0 term-list)))))

(define (the-empty-termlist) '())
(define (first-term term-list)
  (make-term (- (length term-list) 1) (car term-list)))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))


;; EXERCISE 2.90
(define (adjoin-term term term-list)
  ((apply-generic 'make-adjoin-term term-list) term))
(define (the-empty-termlist) (empty-sparse-termlist))
(define (first-term termlist) (apply-generic 'first-term termlist))
(define (rest-terms termlist) (apply-generic 'rest-terms termlist))
(define (empty-termlist? termlist) (apply-generic 'empty-termlist? termlist))
(define (make-polynomial var terms) (make-sparse-polynomial var terms))

(define (make-sparse-polynomial var terms)
  ((get 'make 'polynomial) var (make-sparse-termlist terms)))
(define (make-sparse-termlist terms) ((get 'make 'sparse-termlist) terms))
(define (empty-sparse-termlist) ((get 'empty-termlist 'sparse-termlist)))

(define (install-sparse-termlist-package)
  ;; internal procedures
  (define (make-termlist-sparse terms)
    (fold-right adjoin-term-sparse (empty-termlist-sparse) terms))
  (define (adjoin-term-sparse term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (empty-termlist-sparse) '())
  (define (first-term-sparse term-list) (car term-list))
  (define (rest-terms-sparse term-list) (cdr term-list))
  (define (empty-termlist?-sparse term-list) (null? term-list))

  ;; interface
  (define (tag x) (attach-tag 'sparse-termlist x))
  (put 'make-adjoin-term '(sparse-termlist)
    (lambda (term-list)
      (lambda (term) (tag (adjoin-term-sparse term term-list)))))
  (put 'empty-termlist 'sparse-termlist
    (lambda () (tag (empty-termlist-sparse))))
  (put 'first-term '(sparse-termlist) first-term-sparse)
  (put 'rest-terms '(sparse-termlist)
    (lambda (term-list) (tag (rest-terms-sparse term-list))))
  (put 'empty-termlist? '(sparse-termlist) empty-termlist?-sparse)
  (put 'make 'sparse-termlist
    (lambda (terms) (tag (make-termlist-sparse terms)))))

(define (make-dense-polynomial var terms)
  ((get 'make 'polynomial) var (make-dense-termlist terms)))
(define (make-dense-termlist terms) ((get 'make 'dense-termlist) terms))
(define (empty-dense-termlist) ((get 'empty-termlist 'dense-termlist)))

(define (install-dense-termlist-package)
  ;; internal procedures
  (define (make-termlist-dense terms)
    (fold-right adjoin-term-dense (empty-termlist-dense) terms))
  (define (adjoin-term-dense term term-list)
    (cond ((=zero? (coeff term)) term-list)
          ((= (order term) (length term-list))
           (cons (coeff term) term-list))
          (else (adjoin-term-dense term (cons 0 term-list)))))
  (define (empty-termlist-dense) '())
  (define (first-term-dense term-list)
    (make-term (- (length term-list) 1) (car term-list)))
  (define (rest-terms-dense term-list) (cdr term-list))
  (define (empty-termlist?-dense term-list) (null? term-list))

  ;; interface
  (define (tag x) (attach-tag 'dense-termlist x))
  (put 'make-adjoin-term '(dense-termlist)
    (lambda (term-list)
      (lambda (term) (tag (adjoin-term-dense term term-list)))))
  (put 'empty-termlist 'dense-termlist
    (lambda () (tag (empty-termlist-dense))))
  (put 'first-term '(dense-termlist) first-term-dense)
  (put 'rest-terms '(dense-termlist)
    (lambda (term-list) (tag (rest-terms-dense term-list))))
  (put 'empty-termlist? '(dense-termlist) empty-termlist?-dense)
  (put 'make 'dense-termlist
    (lambda (terms) (tag (make-termlist-dense terms)))))


;; EXERCISE 2.91 see install-polynomial-package above


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
