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
