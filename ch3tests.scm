;;; SECTION 3.5.3
(define (stream-of-list l)
  (if (null? l)
      the-empty-stream
      (cons-stream (car l) (stream-of-list (cdr l)))))

(define sense-data (stream-of-list '(1 3 -1 2 -2 2 -1 1 -3 1 -2)))

(define (sign-change-detector x1 x0)
  (let ((sign0 (>= x0 0)) (sign1 (>= x1 0)))
    (if sign0
        (if sign1 0 -1)
        (if sign1 1  0))))

;; EXERCISE 3.74
(first-n-of-series zero-crossings 10)
;Value: (0 0 -1 1 -1 1 -1 1 -1 1)

;; EXERCISE 3.75
(first-n-of-series zero-crossings 10)
;Value: (0 0 0 0 0 0 0 0 -1 0)

;; EXERCISE 3.76
(first-n-of-series zero-crossings 10)
;Value: (0 0 0 0 0 0 0 0 -1 0)
