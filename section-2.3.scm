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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (=number? exp num) (and (number? exp) (= exp num)))

(deriv '(+ x 3) 'x) ; (+ 1 0)
(deriv '(* x y) 'x) ; (+ (* x 0) (+ 1 y))
(deriv '(* (* x y) (+ x 3)) 'x)
; (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(deriv '(+ x 3) 'x) ; 1
(deriv '(* x y) 'x) ; y
(deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))

;; Exercise 2.56 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

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
         (make-product (exponent exp)
                       (make-exponent (base exp)
                                      (make-sum (exponent exp) -1))
                       (deriv (base exp) var)))
        (else
          (error "unknown expression type -- DERIV" exp))))

;; Exercise 2.57 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Exercise 2.58 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(deriv '(x + 3) 'x) ; 1
(deriv '(x * y) 'x) ; y
(deriv '((x * y) * (x + 3)) 'x) ; ((x * y) + (y * (x + 3)))
(deriv '(x + (3 * (x + (y + 2)))) 'x) ; 4

; b. infix notation with more than two arguments

(define (before sym exp)
  (cond ((null? exp) exp)
        ((eq? (car exp) sym) '())
        (else (cons (car exp) (before sym (cdr exp))))))

(define (simplify x)
  (if (and (pair? x) (null? (cdr x)))
      (car x)
      x))

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

(deriv '(x + 3) 'x) ; 1
(deriv '(x * y) 'x) ; y
(deriv '((x * y) * (x + 3)) 'x) ; ((x * y) + (y * (x + 3)))
(deriv '(x + (3 * (x + (y + 2)))) 'x) ; 4
(deriv '(x + 3 * (x + y + 2)) 'x) ; 4
