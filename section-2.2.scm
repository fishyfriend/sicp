;; Exercise 2.17 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))

;; Exercise 2.18 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reverse list)
  (define (iter old new)
    (if (null? old)
        new
        (iter (cdr old) (cons (car old) new))))
  (iter list '()))

;; Exercise 2.19 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

(define (first-denomination coin-values) (car coin-values))
(define (except-first-denomination coin-values) (cdr coin-values))
(define (no-more? coin-values) (null? coin-values))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins) ; 292
(cc 100 uk-coins) ; 104561

;; Exercise 2.20 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (same-parity x . xs)
  (define filter (if (even? x) even? odd?))
  (define (iter acc xs)
    (if (null? xs)
        acc
        (iter (if (filter (car xs))
                  (cons (car xs) acc)
                  acc)
              (cdr xs))))
  (reverse (iter (list x) xs)))

(same-parity 1 2 3 4 5 6 7) ; (1 3 5 7)
(same-parity 2 3 4 5 6 7) ; (2 4 6)

;; Exercise 2.21 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(square-list (list 1 2 3 4)) ; (1 4 9 16)

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;; Exercise 2.22 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

#| The result is in reverse order because iter consumes items from the input
list moving from head to tail, while building up the output list in tail-to-head
direction. |#

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

#| A list is a pair consisting of a list item and another list, in that order.
By transposing the arguments, the above procedure builds up not a list but a
nested pair of the form (cons (cons (cons '() item1) item2) item3). |#

;; Exercise 2.23 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (for-each f xs)
  (if (null? xs)
      #t
      (begin (f (car xs))
             (for-each f (cdr xs)))))

;; Exercise 2.24 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(list 1 (list 2 (list 3 4)))

#| Interpreter result:

  (1 (2 (3 4)))

Box-and-pointer structure:

  (1 (2 (3 4)))──>[•][•]────>[•|/]
                   ↓          ↓
                  [1]  ┌───>[•|•]─────>[•|/]
                       │     ↓          ↓
                (2 (3 4))   [2]  ┌───>[•|•] → [•|/]
                                 │     ↓       ↓
                              (3 4)   [3]     [4]

Tree:

  (1 (2 (3 4)))
    ├─ 1
    └─ (2 (3 4))
         ├─ 2
         └─ (3 4)
              ├─ 3
              └─ 4 |#

;; Exercise 2.25 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define x '(1 3 (5 7) 9))
(car (cdr (car (cdr (cdr x))))) ; 7

(define y '((7)))
(car (car y)) ; 7

(define z '(1 (2 (3 (4 (5 (6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z)))))))))))) ; 7

;; Exercise 2.26 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ; (1 2 3 4 5 6)
(cons x y) ; ((1 2 3) 4 5 6)
(list x y) ; ((1 2 3) (4 5 6))

;; Exercise 2.27 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deep-reverse item)
  (define (iter old new)
    (if (null? old)
        new
        (iter (cdr old)
              (cons (deep-reverse (car old))
                    new))))
  (if (list? item)
      (iter item '())
      item))

(define x (list (list 1 2) (list 3 4)))
x ; ((1 2) (3 4))
(reverse x) ; ((3 4) (1 2))
(deep-reverse x) ; ((4 3) (2 1))

;; Exercise 2.28 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fringe tree)
  (cond ((null? tree) '())
        ((not (list? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(define x (list (list 1 2) (list 3 4)))
(fringe x) ; (1 2 3 4)
(fringe (list x x)) ; (1 2 3 4 1 2 3 4)

;; Exercise 2.29 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a.

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

; b.

(define (total-weight mobile)
  (define (weight branch)
    (let ((structure (branch-structure branch)))
      (if (number? structure)
          structure
          (total-weight structure))))
  (+ (weight (left-branch mobile)) (weight (right-branch mobile))))

(total-weight
  (make-mobile (make-branch 5 20)
               (make-branch 6 (make-mobile (make-branch 7 30)
                                           (make-branch 8 40))))) ; 90

; c.

(define (balanced? mobile)
  (define (weight branch)
    (let ((structure (branch-structure branch)))
      (if (number? structure)
          structure
          (total-weight structure))))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (= (* (branch-length left) (weight left))
       (* (branch-length right) (weight right)))))

(balanced?
  (make-mobile (make-branch 5 20)
               (make-branch 6 (make-mobile (make-branch 7 30)
                                           (make-branch 8 40))))) ; #f

(balanced?
  (make-mobile (make-branch 7 3)
               (make-branch 3 (make-mobile (make-branch 4 5)
                                           (make-branch 10 2))))) ; #t

; d.

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

#| To adapt to this new representation, only the selectors left-branch,
right-branch, branch-length, and branch-structure need be updated. All the other
procedures are implemented in terms of these and do not access the underlying
structure directly. |#

;; Exercise 2.30 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square-tree tree)
  (if (null? tree)
      tree
      (cons (if (number? (car tree))
                (square (car tree))
                (square-tree (car tree)))
            (square-tree (cdr tree)))))

(define (square-tree tree)
  (map (lambda (item)
         ((if (number? item)
              square
              square-tree)
          item))
       tree))

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7))) ; (1 (4 (9 16) 25) (36 49))

;; Exercise 2.31 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree-map f tree)
  (if (null? tree)
      tree
      (cons (if (list? (car tree))
                (tree-map f (car tree))
                (f (car tree)))
            (tree-map f (cdr tree)))))

(define (square-tree tree) (tree-map square tree))

;; Exercise 2.32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (ss) (cons (car s) ss))
                     rest)))))

(subsets (list 1 2 3)) ; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

#| This implementation works based on the following rationale. A subset of set S
is a combination of zero or more elements of S. To find all subsets of S we must
find all possible combinations of elements in S. For empty S the only subset is
is the empty set, the only possible combination of zero elements. For non-empty
S, observe that for any element e in S, the possible subsets of S are the union
of all combinations that include e and those that don't. Observe also that to
get the combinations that include e, we can simply add e to every other possible
combination. All that remains is to calculate the combinations that do not
include e, which is simply done by applying recursively the logic just laid out.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; Exercise 2.33 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;; Exercise 2.34 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1)) ; 79

;; Exercise 2.35 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (list? x)
                             (count-leaves x)
                             1))
                       t)))

;; Exercise 2.36 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) ; (22 26 30)

;; Exercise 2.37 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; n.b. must use the stdlib definition of map

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (accumulate cons '() (map (lambda (col)
                                       (dot-product row col))
                                     cols)))
         m)))

(define v1 '(1 3 5 7))
(define v2 '(2 4 6 8))
(define m1 '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define m2 '((1 2 3) (3 3 4) (4 5 6) (7 8 8)))
(define m3 '((1 2) (3 5) (4 5) (6 9)))

(dot-product v1 v2) ; 100
(matrix-*-vector m1 v1) ; (50 91 130)
(transpose m1) ; ((1 4 6) (2 5 7) (3 6 8) (4 6 9))
(matrix-*-matrix m1 m2) ; ((47 55 61) (85 101 116) (122 145 166))
(matrix-*-matrix m1 m3) ; ((43 63) (79 117) (113 68))

;; Exercise 2.38 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3)) ; 1/6
(fold-right list '() (list 1 2 3)) ; (1 (2 (3 ())))
(fold-left list '() (list 1 2 3)) ; (((() 1) 2) 3)

#| To guarantee that fold-right and fold-left produce the same result for every
sequence, (op a b) should be equivalent to (op b a). |#

;; Exercise 2.39 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(reverse '(1 2 3)) ; (3 2 1)
