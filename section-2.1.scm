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
