;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq i tem (cdr x)))))

;; Exercise 2.53 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(list 'a 'b 'c) ; (a b c)
(list (list 'george)) ; ((george))
(cdr '((x1 x2) (y1 y2))) ; ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; (y1 y2)
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socks)) ; (red shoes blue socks)

;; Exercise 2.54 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (equal? a b)
  (or (and (symbol? a) (symbol? b)
           (eq? a b))
      (and (pair? a) (pair? b)
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))
      (and (null? a) (null? b))))

;; Exercise 2.55 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(car ''abracadabra) ; quote

#| The special form quote and its abbreviation are evaluated recursively, so the
whole expression evaluates as follows:

(car ''abracadabra)
(car (quote 'abracadabra))
(car (quote (quote abracadabra)))
(car (quote abracadabra)
quote |#
