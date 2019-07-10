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

;;;SECTION 2.5.2

(define z1 (make-complex-from-real-imag 1 1))

;; Before coercion mechanism

(add z1 (make-scheme-number 3))
;Value 1: (complex rectangular 4 . 1)

(add (make-scheme-number 3) z1)
;No method for the given types (add (scheme-number complex))


;; With coercion mechanism

(add z1 (make-scheme-number 3))
;Value 6: (complex rectangular 4 . 1)

(add (make-scheme-number 3) z1)
;Value 7: (complex rectangular 4 . 1)

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
