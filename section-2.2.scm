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

;; Exercise 2.44 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (identity x) x)

;; Exercise 2.45 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (split repeat-large repeat-small)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split repeat-large repeat-small) painter (- n 1))))
           (repeat-large painter (repeat-small smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

;; Exercise 2.46 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Exercise 2.47 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Exercise 2.48 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
        segment-list)))

;; Exercise 2.49 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

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

;; Exercise 2.50 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter) (rotate90 (rotate90 painter)))
(define (rotate270 painter) (rotate90 (rotate180 painter))  )

;; Exercise 2.51 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Exercise 2.52 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| Here is test code for the picture language example (exercises 44-52). My
Scheme lacks graphics support so the test code outputs SVG that can be viewed in
a browser. |#

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

(define (test-painter painter)
  (with-output-to-file "/tmp/sicp-picture.html"
    (lambda () (paint-svg painter 500 500))))

(test-painter outline)
(test-painter diamond)
(test-painter x)
(test-painter wave)
(test-painter wave-and-smile)
(test-painter (right-split wave 4))
(test-painter (corner-split wave 4))
(test-painter (corner-split-less wave 4))
(test-painter (square-limit wave 4))
(test-painter (square-limit-alt wave 4))
