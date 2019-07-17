;;; EXAMPLES OF TESTING CODE (IN MIT SCHEME)
;;; FROM CHAPTER 2 OF STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS


;;;SECTION 2.2.4
; Here is test code for the picture language example (exercises 44-52). My
; Scheme lacks graphics support so the test code outputs an HTML containing an
; SVG image that can be viewed in a browser. Sample outputs are in ch2-piclang
; folder.

(define (draw-line v w)
  ; omit decimal points, this can screw up the SVG rendering
  (let ((x1 (inexact->exact (round (xcor-vect v))))
        (y1 (inexact->exact (round (ycor-vect v))))
        (x2 (inexact->exact (round (xcor-vect w))))
        (y2 (inexact->exact (round (ycor-vect w)))))
    (map display (list "<line x1=\"" x1 "\" y1=\"" y1
                       "\" x2=\"" x2 "\" y2=\"" y2
                       "\" stroke=\"black\" stroke-width=\"3\" />"))
    (newline)))

(define (paint-svg painter width height)
  (define (start)
    (display "<!DOCTYPE html>") (newline)
    (display "<html>") (newline)
    (display "<body>") (newline)
    (map display (list "<svg width=\"" width "\" height=\"" height "\" >"))
    (newline))
  (define (end)
    (display "</svg>") (newline)
    (display "</body>") (newline)
    (display "</html>") (newline))
  (let ((frame (make-frame (make-vect 0 0)
                           (make-vect width 0)
                           (make-vect 0 height))))
    (start)
    ((flip-vert painter) frame) ; flip as SVG has origin at upper-left
    (end)))

(define (test-painter painter filename)
  (with-output-to-file filename
    (lambda () (paint-svg painter 500 500))))

(test-painter outline "ch2-piclang/outline.html")
(test-painter diamond "ch2-piclang/diamond.html")
(test-painter x "ch2-piclang/x.html")
(test-painter wave "ch2-piclang/wave.html")
(test-painter wave-and-smile "ch2-piclang/wave-and-smile.html")
(test-painter (right-split wave 4) "ch2-piclang/right-split-wave.html")
(test-painter (corner-split wave 4) "ch2-piclang/corner-split.html")
(test-painter (corner-split-less wave 4) "ch2-piclang/corner-split-less.html")
(test-painter (square-limit wave 4) "ch2-piclang/square-limit.html")
(test-painter (square-limit-alt wave 4) "ch2-piclang/square-limit-alt.html")


;;;SECTION 2.4.1

;; Ben's rectangular

(define z1 (make-from-real-imag 1 1))
;Value: z1

(real-part z1)
;Value: 1
(imag-part z1)
;Value: 1
(magnitude z1)
;Value: 1.4142135623730951
(angle z1)
;Value: .7853981633974483
(* 4 (angle z1))
;Value: 3.141592653589793
(define z2 (make-from-mag-ang 1.4142135623730951 .7853981633974483))
;Value: z2

(real-part z2)
;Value: 1.
(imag-part z2)
;Value: 1.

z1
;Value 10: (1 . 1)

z2
;Value 14: (1. . 1.)

(add-complex z1 z2)
;Value 16: (2. . 2.)

(sub-complex z1 z2)
;Value 17: (0. . 0.)


;; Alyssa's polar

(define z1 (make-from-real-imag 1 1))
;Value: z1

(real-part z1)
;Value: 1.

(imag-part z1)
;Value: 1.

(magnitude z1)
;Value: 1.4142135623730951

(angle z1)
;Value: .7853981633974483

(* 4 (angle z1))
;Value: 3.141592653589793

(define z2 (make-from-mag-ang 1.4142135623730951 .7853981633974483))
;Value: z2

(real-part z2)
;Value: 1.

(imag-part z2)
;Value: 1.

z1
;Value 12: (1.4142135623730951 . .7853981633974483)

z2
;Value 13: (1.4142135623730951 . .7853981633974483)

(mul-complex z1 z2)
;Value 18: (2.0000000000000004 . 1.5707963267948966)

(div-complex z1 z2)
;Value 19: (1. . 0.)

;;;SECTION 2.4.2

(define z1 (make-from-real-imag 1 1))
;Value: z1

z1
;Value 20: (rectangular 1 . 1)
(real-part z1)
;Value: 1
(imag-part z1)
;Value: 1
(magnitude z1)
;Value: 1.4142135623730951
(angle z1)
;Value: .7853981633974483

(define z2 (make-from-mag-ang 1.4142135623730951 .7853981633974483))
;Value: z2

z2
;Value 22: (polar 1.4142135623730951 . .7853981633974483)

(magnitude z2)
;Value: 1.4142135623730951
(angle z2)
;Value: .7853981633974483
(real-part z2)
;Value: 1.
(imag-part z2)
;Value: 1.

z1
;Value 20: (rectangular 1 . 1)
z2
;Value 22: (polar 1.4142135623730951 . .7853981633974483)

(add-complex z1 z2)
;Value 23: (rectangular 2. . 2.)
(sub-complex z1 z2)
;Value 24: (rectangular 0. . 0.)
(mul-complex z1 z2)
;Value 25: (polar 2.0000000000000004 . 1.5707963267948966)
(div-complex z1 z2)
;Value 26: (polar 1. . 0.)

;;;SECTION 2.4.3
; sample set implementations for testing exercise 2.74

(define (lookup given-key set-of-records)
  ((get 'lookup (type-tag set-of-records)) given-key (contents set-of-records)))

(define (compare-keys k1 k2)
  (define (fail) (error "invalid key type(s)" k1 k2))
  (define (compare-strings s1 s2)
    (cond ((string<? s1 s2) 'lt)
          ((string>? s1 s2) 'gt)
          (else 'eq)))
  (cond ((and (string? k1) (string? k2)) (compare-strings k1 k2))
        ((and (symbol? k1) (symbol? k2)) (compare-strings (symbol->string k1)
                                                          (symbol->string k2)))
        ((and (string? k1) (symbol? k2)) 'gt)
        ((and (symbol? k1) (string? k2)) 'lt)
        (else (fail))))

(define (key<? k1 k2) (eq? (compare-keys k1 k2) 'lt))
(define (key>? k1 k2) (eq? (compare-keys k1 k2) 'gt))

(define (install-ordered-list-set)
  (define (key item) (car item))
  (define (lookup given-key set-of-records)
    (cond ((null? set-of-records) false)
          ((key<? given-key (key (car set-of-records)))
           false)
          ((key>? given-key (key (car set-of-records)))
           (lookup given-key (cdr set-of-records)))
          (else (car set-of-records))))
  (put 'lookup 'ordered-list-set lookup))

(define (install-tree-set)
  (define (entry tree) (car tree))
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (key entry) (car entry))
  (define (lookup given-key set-of-records)
    (if (null? set-of-records)
        false
        (let ((entry (entry set-of-records)))
          (cond ((key<? given-key (key entry))
                 (lookup given-key (left-branch set-of-records)))
                ((key>? given-key (key entry))
                 (lookup given-key (right-branch set-of-records)))
                (else entry)))))
  (put 'lookup 'tree-set lookup))

(install-ordered-list-set)
(install-tree-set)

;;;SECTION 2.5.2

(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(define z1 (make-complex-from-real-imag 1 1))

;; Before coercion mechanism

(add z1 (make-scheme-number 3))
;No method for the given types (add (scheme-number complex))

(add (make-scheme-number 3) z1)
;No method for the given types (add (scheme-number complex))


;; With coercion mechanism

(add z1 (make-scheme-number 3))
;Value 6: (complex rectangular 4 . 1)

(add (make-scheme-number 3) z1)
;Value 7: (complex rectangular 4 . 1)

;; EXERCISE 2.83
(install-integer-package)
(install-real-package)
(install-tower-package)

(raise (make-integer 5))
;Value: (rational 5 . 1)

(raise (make-rational -3 4))
;Value: (real . -.75)

(raise (make-real 0.21))
;Value: (complex rectangular .21 . 0.)

;;EXERCISE 2.84
(define (add-imag-parts x y)
  (apply-generic 'add-imag-parts x y))

(put 'add-imag-parts
     '(complex complex)
     (lambda (x y) (make-real (+ (imag-part x) (imag-part y)))))

(add-imag-parts (make-integer 5) (make-complex-from-real-imag 4 3))
;Value: (real . 3.)

;;EXERCISE 2.85
(install-projection-package)

(drop (make-integer 5))
;Value: (integer . 5)

(drop (make-real 6.7))
;Value: (rational 7543529375845581 . 1125899906842624)

(drop (make-real 6.0))
;Value: (integer . 6)

(drop (make-rational 3 5))
;Value: (rational 3 . 5)

(drop (make-rational -8 4))
;Value: (integer . -2)

(drop (make-complex-from-real-imag 8 9))
;Value: (complex rectangular 8 . 9)

(drop (make-complex-from-real-imag 1 0))
;Value: (integer . 1)

(mul (make-rational 1 4) (make-integer 4))
;Value: (integer . 1)

(add (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 2 -4))
;Value: (integer . 5)

;EXERCISE 2.86
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define c1 (make-complex-from-real-imag 3 (make-rational 4 5)))
(define c2 (make-complex-from-mag-ang 6 (make-rational 7 8)))

(magnitude (div (mul c1 c2) c1))
;Value: 6.

(angle (div (mul c1 c2) c1))
;Value: (rational 7 . 8)

(real-part (div (mul c1 c2) c2))
;Value: 3.

(imag-part (div (mul c1 c2) c2))
;Value: .8000000000000002


;;;SECTION 2.5.3

(install-polynomial-package)

(define a (make-polynomial 'x '((5 1) (4 2) (2 3) (1 -2) (0 -5))))

a
;Value 3: (polynomial x (5 1) (4 2) (2 3) (1 -2) (0 -5))

(add a a)
;Value 4: (polynomial x (5 2) (4 4) (2 6) (1 -4) (0 -10))

(define b (make-polynomial 'x '((100 1) (2 2) (0 1))))

b
;Value 5: (polynomial x (100 1) (2 2) (0 1))

(mul b b)
;Value 6: (polynomial x (200 1) (102 4) (100 2) (4 4) (2 4) (0 1))

;;EXERCISE 2.87
(=zero? a)
;Value: #f

(=zero? (make-polynomial 'x '((2 0))))
;Value: #t

;;EXERCISE 2.88
(=zero? (sub a a))
;Value: #t

;;EXERCISE 2.89
(adjoin-term '(3 0) '(4 5 6))
;Value: (4 5 6)

(adjoin-term '(5 2) (the-empty-termlist))
;Value: (2 0 0 0 0 0)

(adjoin-term '(5 2) '(1 2 3))
;Value: (2 0 0 1 2 3)

;;EXERCISE 2.90
(install-sparse-termlist-package)
(install-dense-termlist-package)

(define a (make-dense-polynomial 'x '((5 1) (4 2) (2 3) (1 -2) (0 -5))))

a
;Value: (polynomial x dense-termlist 1 2 0 3 -2 -5)

(add a a)
;Value: (polynomial x dense-termlist 2 4 0 6 -4 -10)

(define b (make-sparse-polynomial 'x '((100 1) (2 2) (0 1))))

b
;Value: (polynomial x sparse-termlist (100 1) (2 2) (0 1))

(mul b b)
;Value: (polynomial x sparse-termlist (200 1) (102 4) (100 2) (4 4) (2 4) (0 1))

(mul a b)
;Value: (polynomial x sparse-termlist (105 1) (104 2) (102 3) (101 -2) (100 -5)
;                                     (7 2) (6 4) (5 1) (4 8) (3 -4) (2 -7)
;                                     (1 -2) (0 -5))

;;EXERCISE 2.91
(install-polynomial-package)

(div (make-polynomial 'x '((5 1) (0 -1)))
     (make-polynomial 'x '((2 1) (0 -1))))
;Value: ((polynomial x sparse-termlist (3 1) (1 1))
;        (polynomial x sparse-termlist (1 1) (0 -1)))

;;EXERCISE 2.92
(define c (make-polynomial 'y '((1 2) (0 4))))
(define d (make-polynomial 'y '((2 -5) (1 1) (3 4))))
(define e (make-polynomial 'x (list (list 3 c) (list 2 d))))
(define f (make-polynomial 'y (list (list 4 a) (list 2 b))))

d
;Value: (polynomial y (3 4) (2 -5) (1 1))

e
;Value: (polynomial x (3 (polynomial y (1 2) (0 4)))
;                     (2 (polynomial y (3 4) (2 -5) (1 1))))

f
;Value: (polynomial x (100 (polynomial y (2 1)))
;                     (5   (polynomial y (4 1)))
;                     (4   (polynomial y (4 2)))
;                     (2   (polynomial y (4 3) (2 2)))
;                     (1   (polynomial y (4 -2)))
;                     (0   (polynomial y (4 -5) (2 1))))

(add e f)
;Value: (polynomial x (100 (polynomial y (2 1)))
;                     (5   (polynomial y (4 1)))
;                     (4   (polynomial y (4 2)))
;                     (3   (polynomial y (1 2) (0 4)))
;                     (2   (polynomial y (4 3) (3 4) (2 -3) (1 1)))
;                     (1   (polynomial y (4 -2)))
;                     (0   (polynomial y (4 -5) (2 1))))

(mul e f)
; (polynomial x (103 (polynomial y (3 2) (2 4)))
;               (102 (polynomial y (5 4) (4 -5) (3 1)))
;               (8   (polynomial y (5 2) (4 4)))
;               (7   (polynomial y (7 4) (6 -5) (5 5) (4 8)))
;               (6   (polynomial y (7 8) (6 -10) (5 2)))
;               (5   (polynomial y (5 6) (4 12) (3 4) (2 8)))
;               (4   (polynomial y (7 12) (6 -15) (5 7) (4 -18) (3 2)))
;               (3   (polynomial y (7 -8) (6 10) (5 -12) (4 -20) (3 2) (2 4)))
;               (2   (polynomial y (7 -20) (6 25) (5 -1) (4 -5) (3 1))))
;Value: (polynomial x)

(equ? (add e f) (add f e))
;Value: #t

(equ? (mul e f) (mul f e))
;Value: #t

;; Testing complex coefficients
;; Use install-rectangular-package, install-polar-package, and generic selcetors
;; (angle, etc.) from section 2.4.3.
;; Use install-rational-package and install-complex-package from section 2.5.1.
;; Use apply-generic from section 2.5.2.
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-rational-package)

(put-coercion 'scheme-number 'complex
  (lambda (n) (make-complex-from-real-imag (contents n) 0)))
(put-coercion 'rational 'scheme-number
  (lambda (n) (/ (numer (contents n)) (denom (contents n)))))
(put-coercion 'rational 'complex
  (lambda (n) (make-complex-from-real-imag
                (/ (numer (contents n)) (denom (contents n)))
                0)))

(define g (make-polynomial 'x (list
  '(2 3)
  (list 1 (make-complex-from-real-imag 2 3))
  '(0 7))))

(define h (make-polynomial 'x (list
  '(4 1)
  (list 2 (make-rational 2 3))
  (list 0 (make-complex-from-real-imag 5 3)))))

(define gh-expected (make-polynomial 'x (list
  '(6 3)
  (list 5 (make-complex-from-real-imag 2 3))
  '(4 9)
  (list 3 (make-complex-from-real-imag 4/3 2))
  (list 2 (make-complex-from-real-imag 59/3 9))
  (list 1 (make-complex-from-real-imag 1 21))
  (list 0 (make-complex-from-real-imag 35 21)))))

(define gh-actual (mul g h))

(add gh-actual (mul -1 gh-expected)) ;; differences should be very small!
