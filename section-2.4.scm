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
