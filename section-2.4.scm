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

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
              "No method for these types -- APPLY-GENERIC"
              (list op type-tags))))))

;; Exercise 2.73 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; derivatives code from section 2.3

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
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (=number? exp num) (and (number? exp) (= exp num)))

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

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

; mock dispatch table

(define (lookup-dispatch op type records)
  (cond ((null? records) false)
         ((and (eq? op (car (car records)))
               (equal? type (cadr (car records))))
          (caddr (car records)))
         (else (lookup-dispatch op type (cdr records)))))

(define table
  (let ((records '()))
    (lambda (action)
      (cond ((eq? action 'get)
             (lambda (op type)
               (lookup-dispatch op type records)))
            ((eq? action 'put)
              (lambda (op type item)
                (set! records (cons (list op type item) records))))
            (else (error "unknown action" action))))))

(define get (table 'get))
(define put (table 'put))

; new derivatives code

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;;;;;;;;;;;;;;;;;;;;
; a.

#| The sum? and product? predicates of the conditional have been replaced by a
lookup into the dispatch table. This only works for expressions that are
operations and have an operator symbol that can be used as a type tag. Numbers
and variables aren't operations, don't have an operator symbol, and thus can't
be handled generically in this way without further changes. |#

;;;;;;;;;;;;;;;;;;;;
; b.

(define (register-deriv op impl)
  (put 'deriv op (lambda (operands var)
                   (impl (list op (car operands) (cadr operands)) var))))

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

(install-derivatives-package)

; test

(deriv '(+ x 3) 'x) ; 1
(deriv '(* x y) 'x) ; y
(deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))

;;;;;;;;;;;;;;;;;;;;
; c.

(define (install-deriv-exponentiation)
  (define (deriv-exponentiation exp var)
    (make-product (make-product
                    (exponent exp)
                    (make-exponentiation (base exp)
                                         (make-sum (exponent exp) -1)))
                  (deriv (base exp) var)))
  (register-deriv '** deriv-exponentiation))

(install-deriv-exponentiation)

; test

(deriv '(** x 3) 'x) ; (* 3 (** x 2))
(deriv '(+ (* 5 (** x 3)) (* 4 x)) 'x) ; (+ (* 5 (* 3 (** x 2))) 4)

;;;;;;;;;;;;;;;;;;;;
; d.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp) var))))

#| The order of the first two arguments in all invocations of put would have to
be reversed (e.g. in the register-deriv procedure above). No additional changes
are necessary to accommodate the new version of deriv. |#

(define (register-deriv op impl)
  (put op 'deriv (lambda (operands var)
                   (impl (list op (car operands) (cadr operands)) var))))

(install-derivatives-package)
(install-deriv-exponentiation)

; test

(deriv '(+ x 3) 'x) ; 1
(deriv '(* x y) 'x) ; y
(deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))
(deriv '(** x 3) 'x) ; (* 3 (** x 2))
(deriv '(+ (* 5 (** x 3)) (* 4 x)) 'x) ; (+ (* 5 (* 3 (** x 2))) 4)

;; Exercise 2.74 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; sample set implementations for testing

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

; sample data files

(define (pwd-path fname)
  (merge-pathnames (pwd) (->pathname fname)))

(define file1 (pwd-path "section-2.4.ex-74.data1"))
(define file2 (pwd-path "section-2.4.ex-74.data2"))

;;;;;;;;;;;;;;;;;;;;
; a.

#| The contents of a personnel data file should consist of a single Scheme
value, a pair of the form (type-tag . set) where type-tag refers to an available
implementation of sets, and set is a set of employee records that conforms to
that implementation. Valid set implementations should expose a lookup procedure
via the data-directed dispatch table. (lookup key set) should return data in the
form of a pair (key . value), or #f if the requested key wasn't found. |#

(define (get-record file-path employee-name)
  (with-input-from-file file-path
    (lambda ()
      (let ((set (read)))
        (if (eof-object? set)
            (error "file format incorrect" file-path)
            (lookup employee-name set))))))

(get-record file1 "Jane Trent")
; ("Jane Trent" tree-set (position . "prod mgr") () ((salary . 150000) () ()))

(get-record file2 "Betty Burn")
; ("Betty Burn" ordered-list-set (position . "researcher") (salary . 160000))

;;;;;;;;;;;;;;;;;;;;
; b.

#| Each employee record should be a pair of the form (type-tag . set) where,
again, type-tag refers to an available implementation of sets, and set is a set
of employee records conforming to that implementation.

(define (get-salary file-path employee-name)
  (let ((record (get-record file-path employee-name)))
    (if (not record)
        (error "employee record not found" employee-name)
        (let ((pair (lookup 'salary (cdr record))))
          (if pair (cdr pair) false)))))

(get-salary file1 "Jane Trent") ; 150000
(get-salary file2 "Betty Burn") ; 160000

;;;;;;;;;;;;;;;;;;;;
; c.

(define (find-employee-record employee-name division-files)
  (if (null? division-files)
      false
      (let ((record (get-record (car division-files) employee-name)))
        (if record
            record
            (find-employee-record employee-name (cdr division-files))))))

(find-employee-record "Jane Trent" (list file1 file2))
; ("Jane Trent" tree-set (position . "prod mgr") () ((salary . 150000) () ()))

(find-employee-record "Betty Burn" (list file1 file2))
; ("Betty Burn" ordered-list-set (position . "researcher") (salary . 160000))

;;;;;;;;;;;;;;;;;;;;
; d.

#| When Insatiable takes over a new company it must do the following to
integrate new personnel information:

  - Ensure the new division's personnel files are implemented as Scheme sets and
    meet the structural requirements expressed in (a) and (b) above.
  - If the new files don't use an existing implementation of sets, either for
    the files themselves or for the individual records, ensure that any novel
    implementations they use are packaged up as installation procedures
    compatible with data-directed dispatch using get/put, and ensure the
    installers are called in the initialization section of our
    personnel-processing application.
  - Ensure the new division's files are accessible on the company-wide shared
    filesystem. |#

;; Exercise 2.75 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* (magnitude z) (cos (angle z))))
          ((eq? op 'imag-part) (* (magnitude z) (sin (angle z))))
          ((eq? op 'magnitude) (car z))
          ((eq? op 'angle) (cdr z))
          (else
            (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; Exercise 2.76 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| For generic operations with explicit dispatch, adding a new type requires
modifying every generic procedure to handle the new type. Adding a new operation
requires implementing a single procedure with logic to handle all existing
types.

For data-directed style, adding a new type requires writing a procedure to
implement every existing operation for the new type, as well as creating an
installation procedure which registers these procedures. Adding a new operation
requires writing procedures to implement that operation for all existing types,
and updating all existing types' registration code to register those procedures.

For message-passing style, adding a new type requires implementing a single
procedure with logic to handle all existing operations. Adding a new operation
requires modifying all existing types' implementation procedures to handle the
new operation.

In a system where new types must often be added, the data-directed and
message-passing styles are better suited, as the addition of a new type does not
require changes to existing code. (The data-directed style may be slightly
nicer than message-passing because it cleanly separates the dispatch logic from
the application logic.)

If new operations must often be added, then generic operations with explicit
dispatch will be best, for a similar reason -- existing code does not have to
move to accommodate the new operations. |#
