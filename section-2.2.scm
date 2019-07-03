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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

; prime-finding procedures from section 1.2

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; Exercise 2.40 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(unique-pairs 3) ; ((2 1) (3 1) (3 2)

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6) ; ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

;; Exercise 2.41 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-triples-with-sum n s)
  (let ((triples
         (flatmap (lambda (i)
                    (map (lambda (j)
                           (list i j (- s i j)))
                         (enumerate-interval 1 (min i (- s 1 i)))))
                  (enumerate-interval 1 (min n (- s 2))))))
       (filter (lambda (ijk) (<= (caddr ijk) (cadr ijk)))
               triples)))

(find-triples-with-sum 5 9) ; ((3 3 3) (4 3 2) (4 4 1) (5 2 2) (5 3 1))

;; Exercise 2.42 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (row-of-queen positions col)
  (list-ref positions (- col 1)))

(define empty-board '())

(define (adjoin-position new-row k position)
  (append position (list new-row)))

(define (safe-from k1 k2 positions)
  (let ((row1 (row-of-queen positions k1))
        (row2 (row-of-queen positions k2)))
       (cond ((= k1 k2) #t)
             ((= row1 row2) #f)
             ((= (abs (- row1 row2))
                 (abs (- k1 k2)))
              #f)
             (else #t))))

(define (safe? k positions)
  (fold-left (lambda (acc col)
               (if (not acc)
                   #f
                   (safe-from k col positions)))
             #t
             (enumerate-interval 1 (length positions))))

(queens 5)
; ((1 3 5 2 4) (1 4 2 5 3) (2 4 1 3 5) (2 5 3 1 4) (3 1 4 2 5)
;  (3 5 2 4 1) (4 1 3 5 2) (4 2 5 3 1) (5 2 4 1 3) (5 3 1 4 2))

;; Exercise 2.43 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (queens-slow board-size)
  (define (queen-cols k)
    (set! count (+ 1 count))
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (new-row)
              (map (lambda (rest-of-queens)
                     (adjoin-position new-row k rest-of-queens))
                   (queen-cols (- k 1))))
            (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(define (time-queens f n reps)
  (define t0 (runtime))
  (define (iter count)
    (if (= count 0)
        (- (runtime) t0)
        (begin (f n)
               (iter (- count 1)))))
  (/ (iter reps) reps))

(define time-orig (time-queens queens 8 100)) ; .21199999999999988
(define time-slow (time-queens queens-slow 8 1)) ; 361.36
(/ time-slow time-orig) ; 1704.5283018867935

#| The revised version runs slowly because moving the recursive call to the
inner loop changes the runtime order of growth.

In the original version, queen-cols is called approximately board-size times,
as there is a single recursive call per invocation of the procedure, and the
parameter that supplies the stop condition, k, is decremented each time.

In the revised version, each invocation of queen-cols makes board-size
additional recursive calls. Thus the toplevel invocation of queen-cols will make
about (board-size)^(board-size) recursive calls in total.

The above gives us a runtime order of growth of Θ(n) for the original queen-cols
and Θ(nⁿ) for the revised version. Note that we are making two important
assumptions: we are using the number of recursive calls as a proxy for the
running time, and we are estimating the actual number of calls. These
assumptions make the reasoning easier but will distort the answer somewhat.

For the eight-queens puzzle, n=8, so if the original solves it in time T then
based on these orders of growth, we expect the revised version to take about
2,000,000 T:

  nⁿ / n = 8⁸ / 8 = 8⁷ = 2097152

The difference I measured empirically (see above) was 1705 T, so the revised
version performed more than 1000x better than predicted by the estimate.

To improve this estimate we would want to break down the two assumptions. The
number of recursive calls is pretty close to the prediction (I checked) so it is
the first assumption -- the number of recursive calls as a proxy for the
running time -- that would have to be reexamined. We would want to develop a
formula for predicting the running time of queen-cols from k. This is surely
possible, but does not appear straightforward as the runtime depends on the
number of results returned by (queen-cols (- k 1)). |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
