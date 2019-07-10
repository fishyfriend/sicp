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
