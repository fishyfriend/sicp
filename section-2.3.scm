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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Exercise 2.59 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

;; Exercise 2.60 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; element-of-set? is unchanged: Θ(n). Note that the size of n will generally be
; larger than before, since we permit duplicates.
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; adjoin-set is more efficient: previously Θ(n), now Θ(1).
(define (adjoin-set x set) (cons x set))

; intersection-set is equally efficient: Θ(n²). Again, n values will be larger
; than in the original implementation.
(define (intersection-set set1 set2)
  (filter (lambda (x) (element-of-set? x set2)) set1))

; union-set is more efficient: previously Θ(n²), now Θ(n).
(define (union-set set1 set2) (append set1 set2))

#| This representation might be preferable for an application that does a lot of
intense set-building but only needs to query the sets infrequently. For example,
say we are tech support for a large SaaS company. When investigating customer
firewall issues, it is occasionally helpful to have the ability to know whether
any of our servers have received traffic from a given IP address over the past,
say, 24 hours. So we create a service that periodically collects the last day's
worth of logs from all the servers and exposes a queryable set for checking the
IPs. We don't use the query functionality too frequently but we are constantly
collecting and aggregating the data, meaning the bulk of our set API usage is
adjoin and union operations. So we save computing resources by choosing the
implementation where those operations are more efficient. However, our queries
will be slower due to higher n values, so if we start making frequent queries,
this implementation choice might be worth revisiting. |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;; Exercise 2.61 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((<= x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;; Exercise 2.62 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2) (union-set (cdr set1) set2))
                  ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                  (else (cons x2 (union-set set1 (cdr set2)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch-set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; Exercise 2.63 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; a.

#| tree->list-1 and tree->list-2 produce identical results for all trees. |#

(define tree1 (make-tree 7 (make-tree 3 (make-tree 1 '() '())
                                        (make-tree 5 '() '()))
                           (make-tree 9 '()
                                        (make-tree 11 '() '()))))

(define tree2 (make-tree 3 (make-tree 1 '() '())
                           (make-tree 7 (make-tree 5 '() '())
                                        (make-tree 9 '()
                                                     (make-tree 11 '() '())))))

(define tree3 (make-tree 5 (make-tree 3 (make-tree 1 '() '())
                                        '())
                           (make-tree 9 (make-tree 7 '() '())
                                        (make-tree 11 '() '()))))

(tree->list-1 tree1) ; (1 3 5 7 9 11)
(tree->list-2 tree1) ; (1 3 5 7 9 11)
(tree->list-1 tree2) ; (1 3 5 7 9 11)
(tree->list-2 tree2) ; (1 3 5 7 9 11)
(tree->list-1 tree3) ; (1 3 5 7 9 11)
(tree->list-2 tree3) ; (1 3 5 7 9 11)

; b.

#| The orders of growth in the number of steps differ. tree->list-1 is Θ(n²).
In each iteration we make two recursive calls with arguments of size n/2, and we
also traverse a list of size n/2 with append. So the total number of steps
approaches

  n/2 + 2(n/4) + 4(n/8) ... 2ⁿn/2ⁿ⁺¹ = Σ (i=1..n) i/2 = (n² + n)/4

which gives us Θ(n²) as we only care about the most significant power of n.

tree->list-2 is Θ(n), because at each iteration it cons's one additional tree
entry onto the result list until the entire tree has been consumed. |#

;; Exercise 2.64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

#| a. To understand partial-tree, think of elts as the concatenation of four
sublists:

  (a) elements in the left branch of the balanced tree;
  (b) the middle value (entry) of the balanced tree;
  (c) elements in the right branch of the balanced tree;
  (d) elements excluded from the balanced tree.

(a) and (c) each account for half the elements in the balanced tree other than
its entry, and their sizes are chosen to reflect this. partial-tree recursively
balances (a) and (c) and builds the balanced tree by connecting them with (b).
The balanced tree and (d) are returned as the result. |#

#| b. The order of growth in number of steps for a list of n elements is Θ(n).
Each iteration does the same steps so the number of steps is proportional to the
total number of recursive calls. Each iteration makes two recursive calls with n
cut in half. So the recursion depth i grows as (log₂ n) while the number of
recursive calls grows as 2ⁱ = 2^(log₂ n) = n, yielding Θ(n). |#

;; Exercise 2.65 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (union-set set1 set2)
  (define (iter list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          (else
            (let ((hd1 (car list1)) (hd2 (car list2)))
              (cond ((= hd1 hd2) (iter (cdr list1) list2))
                    ((< hd1 hd2) (cons hd1 (iter (cdr list1) list2)))
                    (else (cons hd2 (iter list1 (cdr list2)))))))))
  (list->tree (iter (tree->list-2 set1)
                    (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (define (iter list1 list2)
    (display (list 'iter list1 list2)) (newline)
    (if (or (null? list1) (null? list2))
        '()
        (let ((hd1 (car list1)) (hd2 (car list2)))
          (cond ((< hd1 hd2) (iter (cdr list1) list2))
                ((> hd1 hd2) (iter list1 (cdr list2)))
                (else (cons hd1 (iter (cdr list1) (cdr list2))))))))
  (list->tree (iter (tree->list-2 set1)
                    (tree->list-2 set2))))

;; Exercise 2.66 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let (entry (entry set-of-records))
        (cond ((< given-key (key entry))
               (lookup given-key (left-branch set-of-records)))
              ((> given-key (key entry))
               (lookup given-key (right-branch set-of-records)))
              (else entry)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-leaf symbol weight) (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)   ;symbol
                               (cadr pair)) ;frequency
                    (make-leaf-set (cdr pairs))))))

;; Exercise 2.67 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) ; (a d a b b c a)

;; Exercise 2.68 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (iter bits tree)
    (if (memq symbol (symbols tree))
        (cond ((leaf? tree) bits)
              ((memq symbol (symbols (left-branch tree)))
               (iter (cons 0 bits) (left-branch tree)))
              (else (iter (cons 1 bits) (right-branch tree))))))
  (reverse (iter '() tree)))

(equal? (encode (decode sample-message sample-tree)
                sample-tree)
        sample-message) ; #t

;; Exercise 2.69 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (define (insert tree set)
    (if (or (null? set)
            (<= (weight tree) (weight (car set))))
        (cons tree set)
        (cons (car set) (insert tree (cdr set)))))
  (cond ((null? leaf-set) (error "leaf-set is empty"))
        ((null? (cdr leaf-set)) (car leaf-set))
        (else (successive-merge (insert (make-code-tree (cadr leaf-set)
                                                        (car leaf-set))
                                        (cddr leaf-set))))))

;; Exercise 2.70 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pairs
  '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1)))

(define plaintext
  '(Get a job
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip yip yip yip yip yip
    Sha boom))

(define tree (generate-huffman-tree pairs))

(define message (encode plaintext tree))
; (1 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 1 1 1 1 0
;  1 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1
;  0 1 1 0 0 1)

(length message) ; 84

(* (length plaintext) (/ (log (length pairs)) (log 2))) ; 108

#| The Huffman encoding requires 84 bits, whereas 108 bits would be the
minimum required for a fixed-length encoding. |#

;; Exercise 2.71 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sketch-tree n)
  (define symbols '(a b c d e f g h i j))
  (define (display+ . xs) (map display xs))
  (define (iter syms indent)
    (display+ "{" (car syms))
    (map (lambda (sym) (display+ " " sym))
                       (cdr syms))
    (display+ "} " (- (expt 2 (length syms)) 1))
    (newline)
    (indent)
    (display+ "├─" (car syms) " " (expt 2 (- (length syms) 1)))
    (newline)
    (indent)
    (display+ "└─")
    (if (null? (cddr syms))
        (display+ (cadr syms) " 1")
        (iter (cdr syms) (lambda () (display "  ")
                                    (indent)))))
  (iter (sublist symbols 0 n)
        (lambda () #t)))

#| The procedure above generates a visualization of the Huffman tree for n
symbols with relative frequencies 1, 2, 4, ..., 2ⁿ⁻¹.

(sketch-tree 5)

  {a b c d e} 31
  ├─a 16
  └─{b c d e} 15
    ├─b 8
    └─{c d e} 7
      ├─c 4
      └─{d e} 3
        ├─d 2
        └─e 1

(sketch-tree 10)

  {a b c d e f g h i j} 1023
  ├─a 512
  └─{b c d e f g h i j} 511
    ├─b 256
    └─{c d e f g h i j} 255
      ├─c 128
      └─{d e f g h i j} 127
        ├─d 64
        └─{e f g h i j} 63
          ├─e 32
          └─{f g h i j} 31
            ├─f 16
            └─{g h i j} 15
              ├─g 8
              └─{h i j} 7
                ├─h 4
                └─{i j} 3
                  ├─i 2
                  └─j 1

In trees with this pattern of frequencies, it takes 1 bit to encode the most
frequent symbol and (n-1) bits to encode the least frequent symbol. |#

;; Exercise 2.72 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (encode-symbol symbol tree)
  (define (iter bits tree)
    (if (memq symbol (symbols tree))
        (cond ((leaf? tree) bits)
              ((memq symbol (symbols (left-branch tree)))
               (iter (cons 0 bits) (left-branch tree)))
              (else (iter (cons 1 bits) (right-branch tree))))))
  (reverse (iter '() tree)))

#| For encode-symbol from Ex. 2.68, when the relative frequencies of the n
symbols are as in Ex. 2.71, and limiting ourselves to trees generated by
the successive-merge implementation given in Ex. 2.69, the order of growth of
the number of steps to encode the most frequent symbol is Θ(1). This symbol is
always located in the left branch of the toplevel tree in and so its
single-digit code is always reached in a fixed number of steps.

The order of growth for encoding the the least frequent symbol is Θ(n²). On each
iteration encode-symbol performs an Θ(n) memq check, does a couple of fixed-cost
operations (checking the first two clauses of cond), and then calls itself
recursively on the right branch, which reduces n by one. The Θ(n) operation
dominates the others, and thus the total number of steps grows as
n + (n - 1) + (n - 2) ... 1 = Σ (i=1..n) i = (n² + n)/2, giving Θ(n²). |#
