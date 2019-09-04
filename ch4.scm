;;;;CODE FROM CHAPTER 4 OF STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;**DON'T TRY TO LOAD THIS FILE INTO SCHEME**
;;;; It contains lots of non-Scheme things, such as code to run in
;;;;  the lazy evaluator or the amb evaluator, queries to run in the
;;;;  query interpreter, etc.
;;;;
;;;; The code for the major subsystems in this chapter has been
;;;;  extracted and organized into loadable/runnable Scheme files.
;;;;  Those files contain runnable versions of
;;;;  -- the basic metacircular evaluator (sections 4.1 - 4.1.4)
;;;;  -- the analyzing version of the metacircular evaluator (section 4.1.7)
;;;;  -- the lazy evaluator (section 4.2)
;;;;  -- the amb (nondeterministic) evaluator (section 4.3)
;;;;  -- the query interpreter (section 4.4)
;;;;
;;;; See the start of each topic for more detail about what is loadable


;;;SECTION 4.1.1
;;; **SEE ALSO** ch4-mceval.scm (loadable/runnable evaluator)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)


;;EXERCISE 4.1
;; left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval (first-operand exps) env)))
        (cons first-value (list-of-values (rest-operands exps) env)))))

;; right to left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-values (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env) rest-values))))


;;;SECTION 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))


(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(cond ((> x 0) x)
      ((= x 0) (display 'zero) 0)
      (else (- x)))

(if (> x 0)
    x
    (if (= x 0)
        (begin (display 'zero)
               0)
        (- x)))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


;;EXERCISE 4.2
;; a. Moving the clause for procedure application before the clause for
;; assignments will cause assignments like (define x 3) to be treated as
;; procedure applications. The actual clause for assignments will never be
;; reached.

;; b.
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))


;;EXERCISE 4.3
;; Requires operation-table, get, and put from ch2support.scm

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((pair? exp)
         (let ((evaluator (or (get 'eval (operator exp))
                              (get 'eval '_application))))
           (evaluator exp env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(put 'eval 'quote (lambda (exp env) (text-of-quotation exp)))
(put 'eval 'set! eval-assignment)
(put 'eval 'define eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(put 'eval 'cond (lambda (exp env) (eval (cond->if exp) env)))

(put 'eval 'lambda (lambda (exp env)
                     (make-procedure (lambda-parameters exp)
                                     (lambda-body exp)
                                     env)))

(put 'eval '_application (lambda (exp env)
                           (apply (eval (operator exp) env)
                                  (list-of-values (operands exp) env))))

;; The data-directed implementation of eval differs from the data-directed
;; differentiation procedure of exercise 2.73 in that compound expressions can
;; have arbitrary operators (i.e. when representing a procedure call), and so it
;; must perform an additional "catchall" dispatch (get 'eval '_application) when
;; the operator is not recognized. The two procedures are similar in that they
;; both have a couple of special cases for variables and literal values, which
;; are handled outside of the type-based dispatch system.


;; EXERCISE 4.4
(define (and? exp) (tagged-list? exp 'and))
(define (and-operands exp) (cdr exp))
(define (make-and operands) (cons 'and operands))

(define (eval-and exp env)
  (let ((ops (and-operands exp)))
    (if (no-operands? ops)
        true
        (let ((first-value (eval (first-operand ops) env)))
          (if (true? first-value)
              (if (no-operands? (rest-operands ops))
                  first-value
                  (eval-and (make-and (rest-operands ops)) env))
              false)))))

(define (or? exp) (tagged-list? exp 'or))
(define (or-operands exp) (cdr exp))
(define (make-or operands) (cons 'or operands))

(define (eval-or exp env)
  (let ((ops (or-operands exp)))
    (if (no-operands? ops)
        false
        (let ((first-value (eval (first-operand ops) env)))
          (if (true? first-value)
              first-value
              (eval-or (make-or (rest-operands ops)) env))))))

;; add to eval
;((and? exp) (eval-and exp env))
;((or? exp) (eval-or exp env))


;; EXERCISE 4.5
;: (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;:       (else false))

(define (cond-arrow-clause? clause) (eq? (cadr clause) '=>))
(define (cond-arrow-test clause) (car clause))
(define (cond-arrow-recipient clause) (caddr clause))
(define (make-application op args) (cons op args))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND->IF"
                          clauses)))
              ((cond-arrow-clause? first)
                (make-application
                  (make-lambda
                    '(test-result)
                    (list
                      (make-if 'test-result
                               (make-application (cond-arrow-recipient first)
                                                 '(test-result))
                               (expand-clauses rest))))
                  (list (cond-arrow-test first))))
              (else
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))


;; EXERCISE 4.6
(define (let? exp) (tagged-list? exp 'let))
(define (let-assignments exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-variables exp)
  (define (iter assignments)
    (if (null? assignments)
        '()
        (cons (caar assignments) (iter (cdr assignments)))))
  (iter (let-assignments exp)))

(define (let-values exp)
  (define (iter assignments)
    (if (null? assignments)
        '()
        (cons (cadar assignments) (iter (cdr assignments)))))
  (iter (let-assignments exp)))

(define (make-let assignments body) (cons 'let (cons assignments body)))

(define (let->combination exp)
  (make-application
    (make-lambda (let-variables exp) (let-body exp))
    (let-values exp)))

;; add to eval
;((let? exp) (eval (let->combination exp) env)


;; EXERCISE 4.7
;; Requires exercise 6

;: (let* ((x 3)
;:        (y (+ x 2))
;:        (z (+ x y 5)))
;:   (* x z))

;; let* can be implemented as a nesting of let expressions where the outermost
;; let binds the first variable in the let*, the next-outermost binds the second
;; variable, etc.

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-assignments exp) (let-assignments exp))
(define (let*-body exp) (let-body exp))
(define (let*-variables exp) (let-variables exp))
(define (let*-values exp) (let-values exp))
(define (make-let* assignments body) (cons 'let* (cons assignments body)))

(define (let*->nested-lets exp)
  (define (iter assignments)
    (if (null? assignments)
        (sequence->exp (let*-body exp))
        (make-let (list (car assignments))
                  (list (iter (cdr assignments))))))
  (iter (let*-assignments exp)))

;; It is sufficient to add the following clause to eval, as the evaluation of
;; the nested lets will recursively call eval on the underlying non-derived
;; expressions.

;; add to eval
;((let*? exp) (eval (let*->nested-lets exp) env))


;; EXERCISE 4.8
;; Requires "more primitives" in ch4-mceval.scm
;; Requires exercise 5

;: (define (fib n)
;:   (let fib-iter ((a 1)
;:                  (b 0)
;:                  (count n))
;:     (if (= count 0)
;:         b
;:         (fib-iter (+ a b) a (- count 1)))))

(define (named-let? exp) (and (let? exp) (variable? (cadr exp))))
(define (named-let-name exp) (cadr exp))

(define (make-named-let name assignments body)
  (cons 'let (cons name (cons assignments body))))

(define (let-assignments exp) (if (named-let? exp) (caddr exp) (cadr exp)))
(define (let-body exp) (if (named-let? exp) (cdddr exp) (cddr exp)))

(define (make-definition variable value) (list 'define variable value))

(define (let->combination exp)
  (let ((inner-proc (make-lambda (let-variables exp) (let-body exp))))
    (if (named-let? exp)
        (make-application
          (make-lambda
            '()
            (list (make-definition (named-let-name exp) inner-proc)
                  (make-application (named-let-name exp) (let-values exp))))
          '())
        (make-application inner-proc (let-values exp)))))


;;EXERCISE 4.9
;; Requires "more primitives" from ch4-mceval.scm.
;; Requires exercises 5-6

;; (do action pred) evaluates action and pred in order, repeatedly until pred
;; evaluates to false. The return value is the final value of action.
;;
;;   (let ((count 0))
;;     (do (begin (set! count (+ count 1))
;;                count)
;;         (< count 5)))
;;   ;Value: 5

(define (do? exp) (tagged-list? exp 'do))
(define (do-action exp) (cadr exp))
(define (do-predicate exp) (caddr exp))

(define (do->combination exp)
  (make-application
    (make-lambda '() (list
      (make-definition 'loop
        (make-lambda '(action predicate) (list
          (make-let
            (list (list 'result (make-application 'action '())))
            (list (make-if (make-application 'predicate '())
                           (make-application 'loop '(action predicate))
                           'result))))))
      (make-application 'loop (list
        (make-lambda '() (list (do-action exp)))
        (make-lambda '() (list (do-predicate exp)))))))
    '()))

;; add to eval
;((do? exp) (eval (do->combination exp) env))

;; (for ((variable1 value1) (variable2 value2) ...)
;;      (condition1 condition2 ...)
;;      (increment1 increment2 ...)
;;      action1 action2 ...)
;;
;; creates an environment with the specified variables and initial values, then
;; evaluates repeatedly in this environment the conditions, then the actions,
;; then the increment expressions, each in order, until a condition evaluates to
;; false. The return value is the final value of the last action, or false if
;; this was never evaluated (i.e. if a condition was false on the first
;; iteration).
;;
;;   (for ((a 0) (b 1))
;;        ((< a 5) (< b 10))
;;        ((set! a (+ a 2)) (set! b (+ b 3)))
;;        (+ a b))
;;   ;Value: 11
;;
;;   (for ((a 0) (b 1))
;;        ((> a b))
;;        ((set! b (+ b 1)))
;;        b)
;;   ;Value: #f

(define (for? exp) (tagged-list? exp 'for))
(define (for-assignments exp) (cadr exp))
(define (for-predicates exp) (caddr exp))
(define (for-increments exp) (cadddr exp))
(define (for-body exp) (cddddr exp))

(define (for->combination exp)
  (let->combination
    (make-let
      (for-assignments exp)
      (list
        (make-let
          (list (list 'increment (make-lambda '() (for-increments exp)))
                (list 'body (make-lambda '() (for-body exp))))
          (list
            (make-while
              (make-and (for-predicates exp))
              (list
                (make-let
                  (list (list 'result (make-application 'body '())))
                  (list (make-application 'increment '())
                        'result))))))))))

;; add to eval
;((for? exp) (eval (for->combination exp) env))

;; (while pred action1 action2 ...) evaluates pred and the actions in order,
;; repeatedly until pred evaluates to false. The return value is the final
;; value of the last action, or false if this was never evaluated (i.e. if pred
;; was false on the first iteration).
;;
;;   (let ((count 0))
;;     (while (< count 5)
;;            (set! count (+ count 1))
;;            count))
;;   ;Value: 5
;;
;;   (while (= 2 3) 4)
;;   ;Value: #f

(define (while? exp) (tagged-list? exp 'while))
(define (while-predicate exp) (cadr exp))
(define (while-body exp) (cddr exp))
(define (make-while pred body) (cons 'while (cons pred body)))

(define (while->combination exp)
  (make-application
    (make-lambda '(pred body)
      (list (make-definition 'loop
              (make-lambda '(previous-result)
                (list (make-if (make-application 'pred '())
                               (make-application 'loop
                                 (list (make-application 'body '())))
                               'previous-result))))
            (make-application 'loop '(false))))
    (list (make-lambda '() (list (while-predicate exp)))
          (make-lambda '() (while-body exp)))))

;; add to eval
;((while? exp) (eval (while->combination exp) env))

;; (until pred action1 action2 ...) evaluates pred and the actions in order,
;; repeatedly until pred evaluates to something other than false. The return
;; value is the final value of the last action, or false if this was never
;; evaluated (i.e. if pred was non-false on the first iteration).
;;
;;   (let ((count 0))
;;     (until (= count 5)
;;            (set! count (+ count 1))
;;            count))
;;   ;Value: 5
;;
;;   (until (= 2 2) 4)
;;   ;Value: #f

(define (until? exp) (tagged-list? exp 'until))
(define (until-predicate exp) (cadr exp))
(define (until-body exp) (cddr exp))

(define (until->combination exp)
  (make-application
    (make-lambda '(pred body)
      (list (make-definition 'loop
              (make-lambda '(previous-result)
                (list (make-if (make-application 'pred '())
                               'previous-result
                               (make-application 'loop
                                 (list (make-application 'body '())))))))
            (make-application 'loop '(false))))
    (list (make-lambda '() (list (until-predicate exp)))
          (make-lambda '() (until-body exp)))))

;; add to eval
;((until? exp) (eval (until->combination exp) env))


;; EXERCISE 4.10
;; Requires exercise 6
;; The example summarizes the syntax changes; descriptions of each are given
;; below.
;;
;;   ((fib n) ::=
;;     (a ::= 0)
;;     (b ::= 1)
;;     (shift ::= (fun n ->
;;       (if (= n 0) then a else
;;         (let ((a+b ::= (+ a b)))
;;           (a := b)
;;           (b := a+b)
;;           (shift (- n 1))))))
;;     (shift n))

;; Assignments:
;;   (variable := value)

(define (assignment? exp)
  (and (pair? exp)
       (eq? (cadr exp) ':=)))

(define (assignment-variable exp) (car exp))
(define (assignment-value exp) (caddr exp))

;; Definitions:
;;  (variable ::= value)
;;  ((procedure param1 param2 ...) ::= exp1 exp2 ...)

(define (definition? exp)
  (and (pair? exp)
       (eq? (cadr exp) '::=)))

(define (definition-variable exp)
  (if (symbol? (car exp))
      (car exp)
      (caar exp)))

(define (definition-value exp)
  (if (symbol? (car exp))
      (caddr exp)
      (make-lambda (cdar exp) (cddr exp))))

;; Conditionals:
;;   (if predicate then consequent)
;;   (if predicate then consequent else alternative)
;;   (cond ...) is unchanged.

(define (if? exp)
  (and (tagged-list? exp 'if)
       (eq? (caddr exp) 'then)))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (cadddr exp))

(define (if-alternative exp)
  (let ((else-clause (cddddr exp)))
    (cond ((null? else-clause) 'false)
          ((not (eq? (car else-clause) 'else))
           (error "Malformed else clause" else-clause))
          ((null? (cdr else-clause))
           (error "Empty else clause"))
          (else (cadr else-clause)))))

(define (make-if predicate consequent alternative)
  (list 'if predicate 'then consequent 'else alternative))

;; Lambdas:
;;   (fun param1 param2 ... -> exp1 exp2 ...)

(define (lambda? exp)
  (if (and (eq? (car exp) 'fun)
           (memq '-> (cdr exp)))
      true
      false))

(define (lambda-parameters exp)
  (define (iter acc rest)
    (if (eq? (car rest) '->)
        (reverse acc)
        (iter (cons (car rest) acc) (cdr rest))))
  (iter '() (cdr exp)))

(define (lambda-body exp) (cdr (memq '-> exp)))

(define (make-lambda parameters body)
  (append (cons 'fun parameters) (cons '-> body)))

;; Let-bindings:
;;   (let ((variable1 ::= value1) (variable2 ::= value2) ...) exp1 exp2 ...)
;;   let* and named-let are not supported.

(define (let-assignments exp)
  (map (lambda (a) (list (car a) (caddr a)))
       (cadr exp)))

(define (let-body exp) (cddr exp))
(define (let-variables exp) (map car (let-assignments exp)))
(define (let-values exp) (map cadr (let-assignments exp)))

(define (make-let assignments body)
  (cons 'let
        (cons (map (function (a) (list (car a) '::= (cadr a)))
                   assignments)
              body)))


;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;; EXERCISE 4.11
(define (make-frame bindings) (cons '*frame* bindings))
(define (frame-bindings frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (list var val) (cdr frame))))

(define (extend-environment vars vals base-env)
  (cons (make-frame (map list vars vals))
        base-env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings) (env-loop (enclosing-environment env)))
            ((eq? var (caar bindings)) (cadar bindings))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-bindings frame))))
  (env-loop env)))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings) (env-loop (enclosing-environment env)))
            ((eq? var (caar bindings)) (set-car! (cdar bindings) val))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-bindings frame)))))
  (env-loop env))


;; EXERCISE 4.12
;; This and subsequent exercises use the textbook representation of frames, not
;; the representation as lists of pairs from exercise 11.

(define (scan-frame f frame)
  (define (iter vars vals)
    (if (null? vars)
        false
        (let ((result (f vars vals)))
          (if result
              result
              (iter (cdr vars) (cdr vals))))))
  (iter (frame-variables frame) (frame-values frame)))

(define (scan-env f env)
  (if (eq? the-empty-environment env)
      false
      (let ((result (scan-frame f (first-frame env))))
        (if result
            result
            (scan-env f (enclosing-environment env))))))

(define (set-variable-value! var val env)
  (or (scan-env (lambda (vars vals)
                  (if (eq? (car vars) var)
                      (begin (set-car! vals val) 'ok)
                      false))
                env)
      (error "Unbound variable -- SET!" var)))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (or (scan-frame
          (lambda (vars vals)
            (if (eq? (car vars) var)
                (begin (set-car! vals val) 'ok)
                false))
          frame)
        (begin (add-binding-to-frame! var val frame) 'ok))))

(define (lookup-variable-value var env)
  (let ((var-and-val (lookup-variable-and-raw-value var env)))
    (if var-and-val
        (cdr var-and-val)
        (error "Unbound variable" var))))

(define (lookup-variable-and-raw-value var env)
  (scan-env (lambda (vars vals)
              (if (eq? (car vars) var)
                  (cons var (car vals))
                  false))
    env))


;; EXERCISE 4.13
;; Requires exercise 12

;; (make-unbound! var env) removes any definition of var from the first frame
;; of env. It is not an error if var was not defined in the first frame. Any
;; definitions in enclosing frames are not affected.
;;
;; I limit make-unbound!'s effect to the first frame so that it resembles its
;; approximate inverse operation, define-variable!, and also because there's no
;; clear reason why one would need the other behavior. If we were to implement
;; an undefine special form (the opposite of define), then for parallelism with
;; define and for the preservation of modularity, we'd want it to be able to
;; operate only on the innermost frame.
;;
;; I do not require that var be bound as a precondition because that is easy
;; enough to check explicitly if needed, and the name of the procedure,
;; make-unbound!, highlights the postcondition that var is unbound, suggesting
;; its effect should be the same regardless whether var was bound initially.
;; (If the procedure were instead called unbind!, the other behavior might be
;; indicated.) In addition to these reasons, there is also a similarity with
;; define-variable! here, as that procedure operates regardless of whether the
;; name in question is bound.

(define (make-unbound! var env)
  (or (scan-frame
        (lambda (vars vals)
          (if (eq? (car vars) var)
              (begin (set-car! vars (cadr vars))
                     (set-cdr! vars (cddr vars))
                     (set-car! vals (cadr vals))
                     (set-cdr! vals (cddr vals))
                     'ok)
              false))
        (first-frame env))
      'ok))


;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))



(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

(driver-loop)

(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))

(append '(a b c) '(d e f))


;; EXERCISE 4.14
;; The system version of map can't serve as a primitive implementation "as-is"
;; because it involves procedure application. When the evaluator processes
;; (map f items), the arguments are evaluated first. The f argument, assuming a
;; sensible one has been passed, will evaluate to a procedure value: in our
;; current representation, either (procedure ...) or (primitive ...). Next,
;; apply-primitive-procedure will attempt to apply the system map procedure to
;; the evaluated arguments. This undoubtedly involves applying the f argument to
;; list items. But as the f argument is not a valid procedure in the underlying
;; Scheme, this attempted application leads to an error.


;;;SECTION 4.1.5

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

(eval '(* 5 5) user-initial-environment)

(eval (cons '* (list 5 5)) user-initial-environment)


;; EXERCISE 4.15
(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

;; If we run (try try), the two possible outcomes are the branches of the if
;; statement: either (A) the program runs forever, or (B) it returns the value
;; 'halted. If the outcome was (A), then (halts? try try) must have returned
;; true in order for that branch to have been selected. However, that would
;; imply that (try try) halts, which is clearly not the case since it runs
;; forever. Similarly if we got outcome (B), that would imply (halts? try try)
;; returned false, which contradicts the observed behavior that it did in fact
;; halt. With either possible outcome, halts? cannot accurately describe the
;; behavior of (try try) without a logical inconsistency. Thus, for p=try a=try,
;; it is impossible for (halts? p a) to correctly determine whether p halts on
;; a. This proves generally that it is impossible to write a halts? that works
;; for all p and a.


;;;SECTION 4.1.6

(define (f x)
  (define (even? n)
    (if (= n 0)
        true
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
        false
        (even? (- n 1))))
  ;; rest of body of f
  )

;: (lambda <vars>
;:   (define u <e1>)
;:   (define v <e2>)
;:   <e3>)
;:
;: (lambda <vars>
;:   (let ((u '*unassigned*)
;:         (v '*unassigned*))
;:     (set! u <e1>)
;:     (set! v <e1>)
;:     <e3>))


;; EXERCISE 4.16
;; Requires exercise 12

;; a.
(define (lookup-variable-value var env)
  (let ((var-and-val (lookup-variable-and-raw-value var env)))
    (if var-and-val
        (let ((val (cdr var-and-val)))
          (if (eq? val '*unassigned*)
              (error "Attempt to use uninitialized variable" var)
              val))
        (error "Unbound variable" var))))

;; b.
(define (scan-out-defines exps)
  (define (make-body vars vals others)
    (if (null? vars)
        others
        (list (make-let
                (map (lambda (var) (list var ''*unassigned*))
                     vars)
                (append (map make-assignment vars vals)
                        others)))))
  (rewrite-defines make-body exps))

(define (rewrite-defines f exps)
  (define (iter vars vals others exps)
    (cond ((null? exps) (f vars vals others))
          ((definition? (car exps))
           (iter (cons (definition-variable (car exps)) vars)
                 (cons (definition-value (car exps)) vals)
                 others
                 (cdr exps)))
          (else (iter vars vals (cons (car exps) others) (cdr exps)))))
  (iter '() '() '() (reverse exps)))

(define (make-assignment var val) (list 'set! var val))

;; c.
;; It is better to call scan-out-defines in make-procedure rather than
;; procedure-body so that the scanning out needs to happen only once, when a
;; procedure object is created, rather than each time the procedure body is
;; accessed (which could potentially happen many times).

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))


;; EXERCISE 4.17
;; Requires exercises 12 and 16
;;
;; Environment structure when interpreting definitions sequentially:
;;
;;   ┌──────────────────────────────┐
;;   │                              │◀── environment in which the
;;   └──────────────────────────────┘    outer lambda is called
;;                  ▲
;;   ┌──────────────┴───────────────┐
;;   │ <vars>                       │
;;   │ u: <result of evaluating e1> │◀── environment in which
;;   │ v: <result of evaluating e2> │    <e3> is evaluated
;;   └──────────────────────────────┘
;;
;; Environment structure when definitions are scanned out as described:
;;
;;   ┌──────────────────────────────┐
;;   │                              │◀── environment in which the
;;   └──────────────────────────────┘    outer lambda is called
;;                  ▲
;;   ┌──────────────┴───────────────┐
;;   │ <vars>                       │
;;   └──────────────────────────────┘
;;                  ▲
;;   ┌──────────────┴───────────────┐
;;   │ u: <result of evaluating e1> │◀── environment in which
;;   │ v: <result of evaluating e2> │    <e3> is evaluated
;;   └──────────────────────────────┘
;;
;; Discussion:
;;
;; The extra frame in the transformed program results from the introduction of
;; a let-expression, which desugars into a lambda application, necessitating the
;; creation of an additional frame. The difference in environment structure does
;; not affect program behavior because the program expressions are evaluated in
;; the same order (<e1>, <e2>, <e3> in this example) and the variable bindings
;; seen by each expression are equivalent *provided* that the value expressions
;; avoid making reference to any other variable define'd in the same block. (If
;; we don't follow this restriction, the two versions of the program may indeed
;; behave differently; for example, <e1> could attempt to look up u but actually
;; get some other u present in the outer environment, whereas in the transformed
;; version, this lookup would result in an error due to the special behavior for
;; '*unassigned* variables.)
;;
;; To achieve "simultaneous" scoping without constructing an additional frame:
(define (scan-out-defines exps)
  (define (make-body vars vals others)
    (append
      (map (lambda (var) (make-definition var ''*unassigned*)) vars)
      (map (lambda (var val) (make-assignment var val)) vars vals)
      others))
  (rewrite-defines make-body exps))


;; EXERCISE 4.18
;: (lambda <vars>
;:   (let ((u '*unassigned*)
;:         (v '*unassigned*))
;:     (let (a <e1>)
;:          (b <e2>))
;:       (set! u a)
;:       (set! v b))
;:   <e3>))
;:
;: (define (solve f y0 dt)
;:   (define y (integral (delay dy) y0 dt))
;:   (define dy (stream-map f y))
;:   y)

;; The solve procedure will not work if internal definitions are scanned out as
;; shown here. The definition of y poses no problem: although it refers to dy,
;; this is done in a delayed expression which is not forced by the call to
;; integral. However, the value expression of dy fails evaluation when it tries
;; to access the value of y, because -- despite y's value expression having been
;; evaluated -- the variable y is still set to '*unassigned* at this point.
;;
;; The solve procedure *will* work if internal definitions are scanned out as
;; shown in the text. As above, the definition of y poses no problem, and now,
;; the definition of dy is valid as well. The variable y has been fully defined
;; when dy's value expression is evaluated, so there is no longer an access
;; error. Furthermore, the reference to dy that is contained in the delayed
;; expression inside y does not cause a problem, as it is not forced when
;; evaluating (stream-map f y). Only the first item in the underlying stream
;; is mapped over, which in this case means the initial value y0. The delayed
;; expression is not forced until the second item of the stream is taken.


;; EXERCISE 4.19
;; Require exercise 12 and 16

;: (let ((a 1))
;:   (define (f x)
;:     (define b (+ a x))
;:     (define a 5)
;:     (+ a b))
;:   (f 10))

;;Behavior of above is
;; in MIT Scheme: --> ;Unassigned variable: a  [Alyssa]
;; in MC-Eval:--> 16 (sequential rule)     [Ben]
;; in MC-Eval with scanout: --> ;Unassigned variable a

;; Of the three approaches, I like Alyssa's best. I would eliminate Ben's
;; approach because it feels un-Lisp-like and hard to reason about. It makes the
;; environment of later define directives within a sequence depend on earlier
;; ones: this is a hierarchical relationship and should be expressed
;; hierarchically, i.e., with nested parentheses, rather than sequentially. We
;; already have nice constructs (let, let*) that express such relationships more
;; clearly.
;;
;; Eva's view does not suffer from the same shortcoming, but permits confusing
;; code. In the example above, it is challenging to remember that the first a
;; refers to the variable defined below (=5) rather than the outer variable (=1)
;; given the untuition that if we just defined a variable to a certain value,
;; the next reference to a variable of that name ought to produce that value.
;; A hybrid approach might be possible, so that in
;;
;;   (let ((a 1))
;;     (define b (+ a 3))
;;     (define a 5)
;;     (define c (+ a 4))
;;     ...
;;
;; the first a refers to the outer scope (=1) and the second to the inner (=5);
;; however, this reintroduces the same problem we saw with Ben's approach, i.e.,
;; confusingly expresses a hierarchical relationship in a sequential manner.
;;
;; We are left with Alyssa's approach, which does not introduce hierarchy like
;; Ben's, and does not permit confusing code like Eva's, as recursive references
;; of that sort will cause an error. While less flexible, this approach forces
;; the programmer to do something clearer when dealing with recursive
;; definitions or shadowing: e.g. use an additional let- or let*-expression, or
;; rename a variable.
;;
;; All this said, I would be in favor of removing define entirely from Scheme
;; and simply relying on let and let*, which are unambiguous and desugar in an
;; easy-to-understand way. To facilitate this I would add a shortcut for
;; procedure definitions, similar to what we currently have with define:
;;
;;   (let (((f x) (+ x 5))) ...)
;;
;; This change would also remove a redundancy from the language and encourage
;; a more functional, expression-oriented style of programming by encouraging
;; the programmer to think of frames as sets of bound values rather than as
;; mutable objects to be modified step-by-step using a construct (define) that
;; looks like a procedure call. The downside of this change is that mutually
;; recursive, simultaneous definitions aren't possible with either let or let*,
;; but for those it appears we have letrec (see exercise 20).

;; Implementation of Eva's behavior
;; N.B. Not thread-safe
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (cons '*unassigned*
                          (delay (eval (definition-value exp) env)))
                    env)
  'ok)

(define (lookup-variable-value var env)
  (let ((var-and-val (lookup-variable-and-raw-value var env)))
    (if var-and-val
        (let ((val (cdr var-and-val)))
          (cond ((eq? val '*assigning*)
                 (error "Invalid recursive reference" var))
                ((and (pair? val) (eq? (car val) '*unassigned*))
                 (let ((delayed-val (cdr val)))
                   (set-variable-value! var '*assigning* env)
                   (let ((new-val (force delayed-val)))
                     (set-variable-value! var new-val env)
                     new-val)))
                (else val)))
        (error "Unbound variable" var))))

(define (scan-out-defines exps)
  (define (make-body vars vals others)
    (append (map make-definition vars vals)
            vars
            others))
  (rewrite-defines make-body exps))


;; EXERCISE 4.20
;; Requires exercises 6, 12, and 16

;: (define (f x)
;:   (letrec ((even?
;:             (lambda (n)
;:               (if (= n 0)
;:                   true
;:                   (odd? (- n 1)))))
;:            (odd?
;:             (lambda (n)
;:               (if (= n 0)
;:                   false
;:                   (even? (- n 1))))))
;:     ;; rest of body of f
;:     ))
;:
;: (letrec ((fact
;:           (lambda (n)
;:             (if (= n 1)
;:                 1
;:                 (* n (fact (- n 1)))))))
;:   (fact 10))

;; a. Implementation of letrec
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-assignments exp) (let-assignments exp))
(define (letrec-body exp) (let-body exp))
(define (letrec-variables exp) (let-variables exp))
(define (letrec-values exp) (let-values exp))
(define (make-letrec assignments body) (cons 'letrec (cons assignments body)))

(define (letrec->let exp)
  (let ((vars (letrec-variables exp))
        (vals (letrec-values exp))
        (body (letrec-body exp)))
    (make-let (map (lambda (var) (list var ''*unassigned*))
                   vars)
              (append (map make-assignment vars vals)
                      body))))

;; b.
;; Evaluating <rest of body of f> as shown in exercise:
;;
;;   ┌─────────────────────────────────┐
;;   │                                 │◀── environment where
;;   └─────────────────────────────────┘    f is defined
;;                    ▲
;;   ┌────────────────┴────────────────┐
;;   │ x: ...                          │◀── environment where <rest of
;;   │ even?: ────────────────┐        │    body of f> is evaluated
;;   │ odd?: ─┐               │        │
;;   └────────┼───────────────┼────────┘
;;            ▼   ▲           ▼   ▲
;;          (•I•)─┘         (•I•)─┘
;;           │               │
;;           ▼               ▼
;;       params: n       params: n
;;       body:           body:
;;        (if (= n 0)     (if (= n 0)
;;            false           true
;;            ...             ...
;;
;; Evaluating <rest of body of f> with let replacing letrec:
;;
;;   ┌────────────────────────────────────────┐
;;   │                                        │◀── environment where
;;   └────────────────────────────────────────┘    f is defined
;;        ▲            ▲                ▲
;;   ┌────┴────┐       │                │
;;   │ x: ...  │◀──────┼────────────────┼──── environment where <rest of
;;   │ even?: ─┼───────┼────────────┐   │     body of f> is evaluated
;;   │ odd?: ──┼───┐   │            │   │
;;   └─────────┘   ▼   │            ▼   │
;;               (•I•)─┘          (•I•)─┘
;;                │                │
;;                ▼                ▼
;;            params: n        params:
;;            body:            body:
;;              (if (= n 0)     (if (= n 0)
;;                  false           true
;;                  ...             ...
;;
;; Discussion:
;;
;; With let in place of letrec, the definition of f no longer works. The
;; procedures that implement even? and odd? are defined outside the environment
;; where even? and odd? are visible. When one of these procedures is called,
;; its internal recursive reference to the other procedure will error as the
;; name of that procedure is not bound in the evaluation environment.


;; EXERCISE 4.21

;; a.

;; Factorial:
;;
;: ((lambda (n)
;:    ((lambda (fact)
;:       (fact fact n))
;:     (lambda (ft k)
;:       (if (= k 1)
;:           1
;:           (* k (ft ft (- k 1)))))))
;:  10)
;; ;Value: 3628800 ;; = 10!

;; Fibonacci:
;;
;; ((lambda (n)
;;    ((lambda (fib)
;;       (fib fib 0 1 n))
;;     (lambda (f a b count)
;;       (if (= count 0)
;;           a
;;           (f f b (+ a b) (- count 1))))))
;;  10)
;; ;Value: 55

;; b.
(define (f x)
  (define (even? n)
    (if (= n 0)
        true
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
        false
        (even? (- n 1))))
  (even? x))

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))


;;;SECTION 4.1.7

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;; *start* of analyzing evaluator
;;; **SEE ALSO** ch4-analyzingmceval.scm (loadable/runnable evaluator)

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;; *end* of analyzing evaluator


;; EXERCISE 4.22
;; Requires exercise 6

;; add to analyze
;((let? exp) (analyze (let->combination exp)))


;; EXERCISE 4.23
(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

;; For a sequence of a single expression, the original version of
;; analyze-sequence simply returns the execution procedure for the lone
;; expression of the sequence. Calling this procedure obviously just evaluates
;; that expression in the passed environment; there is no other work performed.
;; Alyssa's version returns an execution procedure that performs 5 additional
;; steps: cond + cdr + null? + car + apply.
;;
;; For a sequence of two expressions, the original version returns a procedure
;; that calls each expression's execution procedure in sequence. In other words,
;; the work performed is two procedure applications (plus whatever those
;; procedures do). Alyssa's version, on the other hand, does a lot more:
;; cond + cdr + null? + car + apply + cdr + apply + all the work from the
;; single-expression sequence, for a total of 12 steps, 10 more than the
;; original.
;;
;; In general we can see that the original version performs the bare minimum
;; work needed to call the execution procedures for each item in the sequence,
;; whereas the new version adds 6 additional steps per sequence item (they are
;; cond + cdr + null? + car + cdr + apply). In both versions, the amount of
;; extra work scales linearly with the number of expressions in the sequence;
;; it is simply greater by some constant factor in the new version vs. the
;; original.


;; EXERCISE 4.24
;; To set up the environment, first load ch4-mceval.scm, then load
;; ch4-analyzingmceval.scm starting from (define analyze ...). I.e., don't
;; load the new analyzing version of eval. Finally load the additional required
;; exercises listed above.
;;
;; Use (run-tests) to run all the tests and display results.
;;
;; Test cases are stored as scheme files in ch4-ex24-tests/ directory. Each
;; file defines a test-proc that takes an integer argument. The test framework
;; will run each test-proc with a range of values.
;;
;; Estimated % time spent on analysis for the procedures and inputs tested:
;;
;;   count-change: 5%-25% (decreases with # of iterations)
;;   fib:          5%-70% (decreases with # of iterations)
;;   sqrt:         20%
;;
;; The analyzing evaluator takes roughly 50-70% as much time as the original
;; evaluator across all the tests.

(define (run-tests)
  (let ((test-files
         (filter
           (lambda (path)
             (let ((ext (pathname-type path)))
               (and ext (string=? ext "scm"))))
           (directory-read "ch4-ex24-tests/"))))
    (if (null? test-files)
        (error "No test cases found")
        (for-each run-test test-files))))

(define (run-test test-file)
  (let*
    ((name (pathname-name test-file))
     (defs (read-exprs test-file))
     (inputs '(2 5 10 25))
     (reps 100))
    (for-each (lambda (input) (run-comparison name defs input reps))
              inputs)))

(define (run-comparison name defs input reps)
  (let* ((appl (list 'test-proc input))
         (expr1 (cons 'begin (append defs (list appl))))
         (expr2 (cons 'begin (append defs (list appl appl))))
         (t0 (time-eval original-eval expr1 reps))
         (t1 (time-eval analyzing-eval expr1 reps))
         (t2 (time-eval analyzing-eval expr2 reps))
         (a% (analysis-fraction t1 t2))
         (e% (execution-fraction t1 t2)))
    (display "Test case: ") (display name) (newline)
    (display "Input: ") (display input) (newline)
    (display "Original eval elapsed time (1 run): ") (display t0) (newline)
    (display "Analyzing eval elapsed time (1 run): ") (display t1) (newline)
    (display "Analyzing eval elapsed time (2 runs): ") (display t2) (newline)
    (display "Analyzing eval % spent analyzing: ") (display a%) (newline)
    (display "Analyzing eval % spent executing: ") (display e%) (newline)
    (display "Analyzing eval time as % of original: ") (display (/ t1 t0))
    (newline) (newline)))

(define (read-exprs file)
  (define (iter exprs)
    (let ((expr (read)))
      (if (eof-object? expr)
          (reverse exprs)
          (iter (cons expr exprs)))))
  (with-input-from-file file (lambda () (iter '()))))

(define (avg-time proc reps)
  (define (iter nrep)
    (if (= nrep 0)
        (runtime)
        (begin (proc)
               (iter (- nrep 1)))))
  (let* ((t0 (runtime))
         (t (iter reps)))
    (/ (- t t0) reps)))

(define (time-eval eval expr reps)
  (avg-time (lambda () (eval expr (setup-environment))) reps))


;; Given
;;
;;   t1 the runtime of the expression in the analyzing evaluator,
;;   t2 the runtime of repeating the expression 2x in the analyzing evaluator,
;;   a the fraction of time spent on analysis in the analyzing evaluator,
;;   e the fraction of time spend on execution in the analyzing evaluator
;;
;; we have
;;
;;   e = 1 - a
;;   t2 = (a + 2e)(t1)
;;      = (2 - a)(t1)
;;   t2 / t1 = 2 - a
;;   a = 2 - t2 / t1.
;;
;; Empirically, t2 / t1 sometimes turns out to be greater than 2 (!) so we
;; account for that by flooring the analysis fraction at 0%. (Sampling error
;; when using a small # of reps seems to be the cause.)
(define (analysis-fraction t1 t2) (max 0. (- 2. (/ t2 t1))))
(define (execution-fraction t1 t2) (- 1. (analysis-fraction t1 t2)))

(define original-eval eval)
(define (analyzing-eval exp env) ((analyze exp) env))


;;;SECTION 4.2.1

(define (try a b)
  (if (= a 0) 1 b))

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))


;; EXERCISE 4.25
(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

;; Evaluating (factorial 5) in ordinary, applicative-order Scheme won't
;; terminate, as evaluation of expression e, (* n (factorial (- n 1))), recurses
;; indefinitely. With normal-order evaluation, (factorial 5) works as expected.
;; At each iteration, e is passed unevaluated as the alternative branch to the
;; underlying if-expression. Now when the if-expression is evaluated, for n > 1,
;; the predicate (= n 1) holds false, causing e to be evaluated. When n = 1, the
;; predicate is now true, so the consequent 1 is evaluated and returned. The
;; alternative e is not evaluated, recursion stops, the procedure terminates.


;; EXERCISE 4.26
;; unless as a derived expression in applicative-order scheme:
(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-predicate exp) (cadr exp))
(define (unless-consequent exp) (caddr exp))

(define (unless-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-unless predicate consequent alternative)
  (list 'unless predicate consequent alternative))

(define (eval-unless exp)
  (if (false? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; add to eval
;((unless? exp) (eval-unless exp env))

;; utility of having unless as a procedure rather than as a special form:
;;
;; It could be useful to have unless (or if) as a procedure rather than a
;; special form when we want to use a list of booleans to select items from
;; other lists in a map expression. It is cleaner to pass the procedure by
;; name rather than having to wrap it in a lambda. Here is an example:
;;
;;   (let ((selectors (list true false false true))
;;         (left-vals (list 6 2 0 5))
;;         (right-vals (list 3 8 9 1)))
;;     (map unless selectors left-vals right-vals))
;;   ;Value: (3 2 0 1)
;;
;; With unless as a special form, the last line would have to be more verbose:
;;
;;   (map (lambda (s l r) (unless s l r)) selectors left-vals right-vals)
;;
;; Note that these examples assume normal-order evaluation.


;;;SECTION 4.2.2
;;; **SEE ALSO** ch4-leval.scm (loadable/runnable evaluator)

;; clause for EVAL
;;((application? exp)
;; (apply (actual-value (operator exp) env)
;;        (operands exp)
;;        env))
;;
;;* here is eval with that clause in it (*not* in book)
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)		;**
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define the-global-environment (setup-environment))

(driver-loop)

(define (try a b)
  (if (= a 0) 1 b))

(try 0 (/ 1 0))

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))


;; EXERCISE 4.27
(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))

;;; L-Eval input: count
;;; L-Eval value: 1
;; Why: Evaluating the definition of w requires evaluating the procedure
;; application (id (id 10)). (id 10) is turned into a thunk and passed to the
;; outer id, which proceeds to increment count and return the thunk argument,
;; which is then bound to the name w. The thunk is not yet evaluated at this
;; stage, so count remains at 1.

;;; L-Eval input: w
;;; L-Eval value: 10
;; Why: w is bound to a thunk containing the expression (id 10). Evaluating any
;; thunk in the REPL forces that thunk and returns the result. Here, forcing the
;; thunk causes the evaluation of (id 10), which increments count to 2 and
;; returns 10, the value that is displayed in the REPL.

;;; L-Eval input: count
;;; L-Eval value: 2
;; Why: see previous explanation.


;; EXERCISE 4.28
;; In this example, eval must evaluate the operator of a procedure application
;; using actual-value. In the definition of threes, map2 receives as its f
;; argument a thunk containing the expression (flip -). Within the body of map2
;; we have the procedure application (f (car as) (car bs)). If we tried to
;; evaluate its operator, f, using ordinary eval, we would get an error (Unknown
;; expression type) because eval cannot handle thunks. But since we evaluate the
;; operator using actual-value, the (flip -) thunk is forced and evaluates to a
;; procedure object, which can then be applied to arguments.

(define (flip f) (lambda (a b) (f b a)))

(define (map2 f as bs)
  (cond ((null? as) '())
        ((null? bs) '())
        (else (cons (f (car as) (car bs))
                    (map2 f (cdr as) (cdr bs))))))

(define threes (map2 (flip -) '(1 2 3) '(4 5 6)))


;; EXERCISE 4.29
;; a. A program that performs significantly worse under non-memoizing evaluator
(define (fact-iter x n)
  (if (= n 0)
      x
      (fact-iter (* x n) (- n 1))))

(define (fact n)
  (fact-iter 1 n))

(fact 100)

;; b. Interaction from text:
(define (square x)
  (* x x))

;;; L-Eval input: (square (id 10))
;;; L-Eval value (memoizing evaluator): 100
;;; L-Eval value (non-memoizing evaluator): 100

;;; L-Eval input: count
;;; L-Eval value (memoizing evaluator): 1
;;; L-Eval value (non-memoizing evaluator): 2


;; EXERCISE 4.30
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; a.
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;; In the above program, although the value of (proc (car items)) is not forced,
;; the expression is still evaluated via eval, per eval-sequence. This results
;; in evaluation, also via eval, of each of the body expressions of the lambda,
;; producing the desired behavior of the program. The distinction between
;; forcing and not forcing a given expression is not relevant here because
;; (proc (car items)) is an explicit procedure application, not a thunk.

;; b.
(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

;; With original eval-sequence:
;(p1 1)
;Value: (1 2)

;(p2 1)
;Value: 1

;; With modified eval-sequence:
;(p1 1)
;Value: (1 2)

;(p2 1)
;Value: (1 2)

;; c.
;; The items of the sequence are not thunks, so evaluating them using eval is
;; equivalent to evaluating them using actual-value.

;; d.
;; For the treatment of sequences in the lazy evaluator, I would favor the
;; original approach from the text. The whole point of a lazy evaluator, it
;; seems, is to avoid doing more work than necessary. Likewise, the whole point
;; of thunks is that there are some expressions that we might not need to
;; evaluate right away, so we need a way to defer their evaluation until later,
;; or possibly never. Thus, it makes sense not to force any of the expressions
;; in a sequence until absolutely necessary. For expressions occurring before
;; the end of the sequence, we'll never use their value, so we should never
;; force them. If we require a side effect from forcing any of those
;; expressions, we should restructure our code to execute that side effect
;; explicitly rather than relying on thunk forcing. For example, in part b of
;; this exercise, we could rewrite p2 as follows, and get the desired behavior:

(define (p2 x)
  (define (p e)
    (e)
    x)
  (p (lambda () (set! x (cons x '(2))))))

;(p2 1)
;Value: (1 2)


;; EXERCISE 4.31
(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-args (procedure-arg-kinds procedure)
                         arguments
                         env)
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-args kinds exps env)
  (if (no-operands? exps)
      '()
      (let ((first (cond ((eq? (car kinds) 'lazy)
                          (delay-it (first-operand exps) env false))
                         ((eq? (car kinds) 'lazy-memo)
                          (delay-it (first-operand exps) env true))
                         ((eq? (car kinds) 'eager)
                          (actual-value (first-operand exps) env))
                         (else (error "Unknown argument kind" (car kinds))))))
        (cons first
              (list-of-args (cdr kinds)
                            (rest-operands exps)
                            env)))))

(define (procedure-parameters p)
  (map (lambda (param) (if (pair? param) (car param) param))
       (cadr p)))

(define (procedure-arg-kinds p)
  (map (lambda (param) (if (pair? param) (cadr param) 'eager))
       (cadr p)))

(define (thunk-memoizing? x) (cadddr x))
(define (delay-it exp env memoizing?) (list 'thunk exp env memoizing?))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (if (thunk-memoizing? obj)
               (begin (set-car! obj 'evaluated-thunk)
                      (set-car! (cdr obj) result)
                      (set-cdr! (cdr obj) '())))
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))


;;;SECTION 4.2.3
;;;
;;; This code can be loaded as a whole into the lazy evaluator,
;;;  and the examples (commented out with ;:) can then be evaluated
;;;  individually.

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))


(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(define (add-lists list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) (car list2))
                    (add-lists (cdr list1) (cdr list2))))))

;: (define ones (cons 1 ones))

;: (define integers (cons 1 (add-lists ones integers)))

;: (list-ref integers 17)

(define (integral integrand initial-value dt)
  (define int
    (cons initial-value
          (add-lists (scale-list integrand dt)
                    int)))
  int)

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map f y))
  y)

;: (list-ref (solve (lambda (x) x) 1 .001) 1000)


;; EXERCISE 4.32
;; Requires "more primitives" from ch4-leval.scm

;; Example 1: We can use the fact that head of a "lazier" list is delayed to
;; more efficiently implement procedures that need only later elements from the
;; list. In the stream version of last-item, every item in the list must be
;; evaluated before returning the last one. In the "lazier" list version, only
;; the last item is evaluated.

;; Stream version
(define (last-item s)
  (if (empty-stream? (stream-cdr s))
      (stream-car s)
      (last-item (stream-cdr s))))

;; "Lazier" list version
(define (last-item xs)
  (if (null? (cdr xs))
      (car xs)
      (last-item (cdr xs))))

;; Example 2: This example, sequence reversal, illustrates how we can rearrange
;; all the items of a "lazier" list without evaluating any of them. Performing
;; the same change on a stream requires evaluation of the entire stream.

;; Stream version
(define (reverse s)
  (define (iter xs ys)
    (if (empty-stream? ys)
        xs
        (iter (cons-stream (stream-car ys) xs)
              (stream-cdr ys))))
  (iter the-empty-stream s))

;; "Lazier" list version
(define (reverse s)
  (define (iter xs ys)
    (if (null? ys)
        (map car xs)
        (iter (cons ys xs)
              (cdr ys))))
  (iter '() s))

;; Example 3: We can define sequences in terms of each other without having to
;; write an explicit delay or restructure the code. This is not always possible
;; with streams.

(define (calculate-interest balance)
  (cond ((> balance 50000.) 0.031)
        ((> balance 25000.) 0.028)
        ((> balance 10000.) 0.026)
        (else 0.014)))

;; Stream version -- Initial, doesn't work
(define (balance-over-time initial-value changes)
  (define balance (cons-stream initial-value (stream-map + balance changes)))
  balance)

(define (balance-with-interest initial-value)
  (define balance
    (balance-over-time initial-value
                       (stream-map (lambda (bal int) (* bal (+ 1. int)))
                                   balance
                                   interest-rate)))
  (define interest-rate (stream-map calculate-interest balance))
  balance)

;; Stream version -- Revised, works due to added delay
(define (balance-over-time initial-value delayed-changes) ;changed
  (define balance
    (cons-stream initial-value
      (stream-map + balance (force delayed-changes)))) ;changed
  balance)

(define (balance-with-interest initial-value)
  (define balance
    (balance-over-time initial-value
                       (delay ;changed
                         (stream-map (lambda (bal int) (* bal (+ 1. int)))
                                     balance
                                     interest-rate))))
  (define interest-rate (stream-map calculate-interest balance))
  balance)

;; "Lazier" list version, works as-is
;; Requires map2 from exercise 28
(define (balance-over-time initial-value changes)
  (define balance (cons initial-value (add-lists balance changes)))
  balance)

(define (balance-with-interest initial-value)
  (define balance
      (balance-over-time initial-value
                         (map2 (lambda (bal int) (* bal (+ 1. int)))
                               balance
                               interest-rate)))
  (define interest-rate (map calculate-interest balance))
  balance)


;; EXERCISE 4.33
;: (car '(a b c))

(define (quoted-pair? exp)
  (and (quoted? exp)
       (pair? (text-of-quotation exp))))

(define (eval-quoted-pair exp env)
  (let ((contents (text-of-quotation exp)))
    (eval (list 'cons
                (list 'quote (car contents))
                (list 'quote (cdr contents)))
          env)))

;; add to eval
;((quoted-list? exp) (eval-quoted-pair exp env))


;; EXERCISE 4.34
;; To use the code in this solution, do the following in order:
;;   • Start a fresh Scheme session
;;   • Paste the contents of ch4-leval.scm
;;   • Run the line at the end of ch4-mceval.scm to init the global environment
;;   • Paste the "Code to modify the driver loop" below
;;   • Run (driver-loop) to enter the driver loop
;;   • Paste the "Code to be evaluated within the driver loop" below
;;   • Paste the definitions under section 4.2.3 above, *except for* cons, car,
;;     and cdr

;; Code to modify the driver loop
(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((lazy-pair? object) (display (partial-eval-lazy-pair object 3)))
        (else (display object))))

(define (partial-eval-lazy-pair object max-items)
  (define (iter object count)
    (if (lazy-pair? object)
        (let* ((obj-car (lazy-pair-car object))
               (obj-car-evaled
                (if (lazy-pair? obj-car)
                    (partial-eval-lazy-pair obj-car max-items)
                    obj-car))
               (obj-cdr (lazy-pair-cdr object)))
          (cond ((null? obj-cdr) (list obj-car-evaled))
                ((= count max-items) (list obj-car "..."))
                ((lazy-pair? obj-cdr)
                 (cons obj-car-evaled (iter obj-cdr (+ count 1))))
                (else (list obj-car-evaled "." obj-cdr))))
        object))
  (cons 'lazy (iter object 1)))

(define (lazy-pair? x) (tagged-list? x '*lazy-pair*))
(define (lazy-pair-procedure x) (cadr x))

(define (lazy-pair-car x)
  (force-it (apply (lazy-pair-procedure x)
            (list '(lambda (p q) p))
            the-empty-environment)))

(define (lazy-pair-cdr x)
  (force-it (apply (lazy-pair-procedure x)
            (list '(lambda (p q) q))
            the-empty-environment)))

;; Code to be evaluated within the driver loop
(define primitive-cons cons)
(define primitive-car car)
(define primitive-cdr cdr)
(define primitive-list list)
(define (cons x y) (primitive-list '*lazy-pair* (lambda (m) (m x y))))
(define (car z) ((primitive-car (primitive-cdr z)) (lambda (p q) p)))
(define (cdr z) ((primitive-car (primitive-cdr z)) (lambda (p q) q)))


;;;SECTION 4.3
;;;
;;; The code from 4.3 (intro), 4.3.1, and 4.3.2 can be loaded into the
;;; amb evaluator, and the examples (commented out with ;:) can then
;;; be evaluated individually.
;;;   NB. To run the prime-number examples, you must also define prime?
;;; (e.g. using the definition from chapter 1)

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))

;: (prime-sum-pair '(1 3 5 8) '(20 35 110))


;;;SECTION 4.3.1

;: (list (amb 1 2 3) (amb 'a 'b))

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))


;: (prime-sum-pair '(1 3 5 8) '(20 35 110))

;: try-again

;: try-again

;: try-again

;: (prime-sum-pair '(19 27 30) '(11 36 58))


;; EXERCISE 4.35
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(define (an-integer-between a b)
  (cond ((= a b) a)
        ((> a b) (an-integer-between b a))
        (else (amb a (an-integer-between (+ a 1) b)))))


;; EXERCISE 4.36
;; If we replace an-integer-between with an-integer-starting-from in the
;; previous exercise, the procedure will run indefinitely without yielding an
;; answer. The first values of i and j will be selected (i=low and j=low), then
;; the program will try increasing values of k forever, never halting because
;; i=j and there exist no Pythagorean triples where i=j.

(define (a-pythagorean-triple-starting-from n)
  (define (a-triple-with-sum sum)
    (let ((i (an-integer-between n sum)))
      (let ((j (an-integer-between i (- sum i))))
        (let ((k (- sum i j)))
          (require (= (+ i j k) sum))
          (require (= (+ (* i i) (* j j)) (* k k)))
          (list i j k)))))
  (define (a-triple-with-sum-at-least sum)
    (amb (a-triple-with-sum sum)
         (a-triple-with-sum-at-least (+ sum 1))))
  (a-triple-with-sum-at-least n))


;; EXERCISE 4.37
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

;; Ben's version is more efficient. It cuts down the number of combinations that
;; must be explored. For i and j, the number of combinations is the same as in
;; the original version, as the expressions used to initialize these variables
;; are the same. For k, however, we now effectively explore one k value for
;; each combination of i and j, rather than all k values in the range from j to
;; high. This improvement should easily outweigh any added computation time
;; from other changes, e.g. the addition of the sqrt call.


;;;SECTION 4.3.2 -- Logic Puzzles

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))


;; EXERCISE 4.38
;; If we re-run (multiple-dwelling) after commenting out the line
;; (require (not (= (abs (- smith fletcher)) 1))), there are now 5 solutions.


;; EXERCISE 4.39
;; The order of restrictions does not affect the answer in this procedure. We
;; always begin with the complete set of all possible answers, regardless of the
;; order that restrictions are later applied. An valid answer, by definition,
;; passes all restrictions, so it is impossible for any restriction to filter
;; out a valid answer. Thus, all valid answers from the set of possible answers
;; are guaranteed to still be present at the end. Likewise, an invalid answer
;; by definition fails at least one restriction. For each invalid answer,
;; that same restriction will still be applied at some point, regardless of the
;; order the restrictions are applied overall. Therefore all invalid answers are
;; guaranteed to be removed from the final set. From this we can see that the
;; final set is unchanged. (The order in which multiple answers are presented
;; would not change, either, as the only affect of applying a restriction can be
;; to remove an answer from the working set. It cannot reorder the set.)
;;
;; The order of restrictions *can* affect the performance of the procedure.
;; Since a given evaluation of the procedure body terminates immediately
;; when a single restriction fails, we should expect better performance from
;; versions of the procedure that move the most-likely-to-fail restrictions
;; earlier in the procedure body, as their failure will avoid more evaluations
;; of additional restrictions this way. Below is a version of multiple-dwelling
;; that reorders the requirements slightly. If we time the two versions over
;; numerous invocations, the reordered version gives a very slight but
;; persistent speed advantage. (See ch4tests.scm for timing code and timings on
;; my machine.)

(define (multiple-dwelling-reordered)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    ;(require (distinct? (list baker cooper fletcher miller smith))) >──┐
    (require (> miller cooper)) ; ◀────────────────────────────────┐    │
    (require (not (= (abs (- smith fletcher)) 1))) ; ◀────────┐    │    │
    (require (not (= (abs (- fletcher cooper)) 1))) ; ◀───┐   │    │    │
    (require (not (= baker 5))) ;                         │   │    │    │
    (require (not (= cooper 1))) ;                        │   │    │    │
    (require (not (= fletcher 5))) ;                      │   │    │    │
    (require (not (= fletcher 1))) ;                      │   │    │    │
    ;(require (> miller cooper)) ; >──────────────────────┼───┼────┘    │
    ;(require (not (= (abs (- smith fletcher)) 1))) ; >───┼───┘         │
    ;(require (not (= (abs (- fletcher cooper)) 1))) ; >──┘             │
    (require (distinct? (list baker cooper fletcher miller smith))) ; ◀─┘
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))


;; EXERCISE 4.40
;; Before the distinct? requirement, there are 5^5 = 3125 sets of assignments of
;; people to floors; after, there are 5! = 120 sets.

(define (multiple-dwelling-fast)
  (let ((baker (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))
      (let ((fletcher (amb 1 2 3 4 5)))
        (require (not (= fletcher 5)))
        (require (not (= fletcher 1)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (let ((miller (amb 1 2 3 4 5)))
          (require (> miller cooper))
          (let ((smith (amb 1 2 3 4 5)))
            (require (not (= (abs (- smith fletcher)) 1)))
            (require (distinct? (list baker cooper fletcher miller smith)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))


;; EXERCISE 4.41
;; Requires nil from ch2support.scm
;; Requires accumulate, flatmap, permutations, and remove from section 2.2.3
(define (multiple-dwelling-native)
  (define (valid? baker cooper fletcher miller smith)
    (and (not (= baker 5))
         (not (= cooper 1))
         (not (= fletcher 5))
         (not (= fletcher 1))
         (> miller cooper)
         (not (= (abs (- smith fletcher)) 1))
         (not (= (abs (- fletcher cooper)) 1))))
  (define (show baker cooper fletcher miller smith)
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith)))
  (let ((answers
         (filter (lambda (p) (apply-in-underlying-scheme valid? p))
                 (permutations '(1 2 3 4 5)))))
    (cond ((null? answers) (error "No solution found"))
          ((not (null? (cdr answers))) (error "Multiple solutions found"))
          (else (apply-in-underlying-scheme show (car answers))))))


;; EXERCISE 4.42
(define (liars-puzzle)
  (define (xor a b)
    (require (if a (not b) b)))
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require (distinct? (list betty ethel joan kitty mary)))
    (xor (= kitty 2) (= betty 3))
    (xor (= ethel 1) (= joan 2))
    (xor (= joan 3) (= ethel 5))
    (xor (= kitty 2) (= mary 4))
    (xor (= mary 4) (= betty 1))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

;; Solution: ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))


;; EXERCISE 4.43
;; Requires map from section 2.2.1 and nil from ch2support.scm
;; Set mary-ann-moore? to false to omit the information that Mary Ann's last
;; name is Moore.
(define (yacht-puzzle mary-ann-moore?)
  (define (find f x ys)
    (cond ((null? ys) false)
          ((eq? (f (car ys)) x) (car ys))
          (else (find f x (cdr ys)))))
  (define (fdy f d y) (require (not (eq? d y))) (list f d y))
  (define (f fdy) (car fdy))
  (define (d fdy) (car (cdr fdy)))
  (define (y fdy) (car (cdr (cdr fdy))))
  (let ((ds '(mary-ann gabrielle lorna rosalind melissa)))
    (let ((barnacle (fdy 'barnacle 'melissa 'gabrielle)))
      (let ((hall (fdy 'hall (an-element-of ds) 'rosalind)))
        (let ((downing (fdy 'downing (an-element-of ds) 'melissa)))
          (let ((moore (fdy 'moore
                            (if mary-ann-moore? 'mary-ann (an-element-of ds))
                            'lorna)))
            (let ((parker (fdy 'parker (an-element-of ds) (an-element-of ds))))
              (let ((fdys (list moore barnacle hall downing parker)))
                (require (distinct? (map d fdys)))
                (require (eq? (y (find d 'gabrielle fdys)) (d parker)))
                (require (distinct? (map f fdys)))
                (require (distinct? (map y fdys)))
                (f (find d 'lorna fdys))))))))))

;; Solution: Downing
;; There are two solutions when we lack Mary Ann's last name: Downing, Parker


;; EXERCISE 4.44
(define (queens board-size)
  (define (iter col rows dias1 dias2)
    (if (> col board-size)
        rows
        (let ((row (an-integer-between 1 board-size)))
          (require (not (memq row rows)))
          (let ((dia1 (+ row col -1)))
            (require (not (memq dia1 dias1)))
            (let ((dia2 (+ board-size row (- col))))
              (require (not (memq dia2 dias2)))
              (iter (+ col 1)
                    (cons row rows)
                    (cons dia1 dias1)
                    (cons dia2 dias2)))))))
  (iter 1 '() '() '()))


;;;SECTION 4.3.2 -- Parsing natural language

;;; In this section, sample calls to parse are commented out with ;:
;;; and the output of parses is quoted with '
;;; Thus you can load this whole section into the amb evaluator --
;;;  (but beware of the exercise 4.47 code, and of redefinitions
;;;   of a procedure -- e.g. parse-noun-phrase)

(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats sleeps))

(define articles '(article the a))

;; output of parse
'(sentence (noun-phrase (article the) (noun cat))
           (verb eats))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-word verbs)))

(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))


;: (parse '(the cat eats))
;; output of parse
'(sentence (noun-phrase (article the) (noun cat)) (verb eats))

(define prepositions '(prep for to in by with))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

;: (parse '(the student with the cat sleeps in the class))

;; output of parse
'(sentence
 (noun-phrase
  (simple-noun-phrase (article the) (noun student))
  (prep-phrase (prep with)
               (simple-noun-phrase
                (article the) (noun cat))))
 (verb-phrase
  (verb sleeps)
  (prep-phrase (prep in)
               (simple-noun-phrase
                (article the) (noun class)))))

;: (parse '(the professor lectures to the student with the cat))

;; output of parse
'(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb lectures)
   (prep-phrase (prep to)
                (simple-noun-phrase
                 (article the) (noun student))))
  (prep-phrase (prep with)
               (simple-noun-phrase
                (article the) (noun cat)))))

;; output of parse
'(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb lectures)
  (prep-phrase (prep to)
               (noun-phrase
                (simple-noun-phrase
                 (article the) (noun student))
                (prep-phrase (prep with)
                             (simple-noun-phrase
                              (article the) (noun cat)))))))


;; EXERCISE 4.45
;: (parse '(the professor lectures to the student in the class with the cat))

;; The student is lectured to by the professor; this lecturing occurs in the
;; class; the lecture makes use of the cat.
'(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb-phrase
        (verb lectures)
        (prep-phrase (prep to)
                     (simple-noun-phrase (article the) (noun student))))
      (prep-phrase (prep in)
                   (simple-noun-phrase (article the) (noun class))))
    (prep-phrase (prep with)
                 (simple-noun-phrase (article the) (noun cat)))))

;; The student is lectured to by the professor; this lecturing occurs in not
;; just any class, but specifically the class where the cat is.
'(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb lectures)
      (prep-phrase (prep to)
                   (simple-noun-phrase (article the) (noun student))))
    (prep-phrase
      (prep in)
      (noun-phrase
        (simple-noun-phrase (article the) (noun class))
        (prep-phrase (prep with)
                     (simple-noun-phrase (article the) (noun cat)))))))

;; Not just any student, but specifically the student who is a member of the
;; class, is lectured to by the professor; the lecture incorporates the cat.
'(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb lectures)
      (prep-phrase
        (prep to)
        (noun-phrase
          (simple-noun-phrase (article the) (noun student))
          (prep-phrase (prep in)
                       (simple-noun-phrase (article the) (noun class))))))
    (prep-phrase (prep with)
                 (simple-noun-phrase (article the) (noun cat)))))

;; That specific student who is a member of the class, and who also has the cat,
;; receives a lecture from the professor.
'(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb lectures)
    (prep-phrase
      (prep to)
      (noun-phrase
        (noun-phrase
          (simple-noun-phrase (article the) (noun student))
          (prep-phrase (prep in)
                       (simple-noun-phrase (article the) (noun class))))
        (prep-phrase (prep with)
                     (simple-noun-phrase (article the) (noun cat)))))))

;; You know that specific student who is a member of not just any class, but the
;; class which has the cat? That person is being lectured to by the professor.
'(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb lectures)
    (prep-phrase
      (prep to)
      (noun-phrase
        (simple-noun-phrase (article the) (noun student))
        (prep-phrase
          (prep in)
          (noun-phrase
            (simple-noun-phrase (article the) (noun class))
            (prep-phrase (prep with)
                         (simple-noun-phrase (article the) (noun cat)))))))))


;; EXERCISE 4.46
;; Since parsers modify a global state (*unparsed*) that affects the results of
;; future parsing operations, the execution order of parsers is important. The
;; list representation of each grammatical rule orders subcomponents in the same
;; order they are expected to be encountered in the input stream. Accordingly,
;; the parsers for subcomponents are invoked in that order as arguments to the
;; list constructor. For example, a sentence consists of a noun phrase followed
;; by a verb phrase, and thus parse-sentence invokes parse-noun-phrase and
;; parse-verb-phrase as the second and third arguments to list. If argument
;; lists were evaluated in any other order than left-to-right, the parsers for
;; subcomponents would not execute in the order that the subcomponents occur in
;; input, causing parsing to fail on input that should be valid. For example,
;; when parsing a sentence with parse-sentence, parse-verb-phrase will be
;; invoked first, and will fail when it attempts to parse the noun phrase that
;; occurs first in the input.


;; EXERCISE 4.47
(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))

;; This revised version will correctly generate all possible parsings, but it
;; has a problem. Observe that the second branch of the amb begins by calling
;; parse-verb-phrase recursively. The order of arguments to amb ensures that
;; all correct answers will be found first, but once that happens, the only
;; remaining non-failing path is the one that takes the second branch of the amb
;; repeatedly ad infinitum. amb is required to explore this path (all paths must
;; be explored, per its definition) and this causes evaluation to hang forever.
;;
;; Swapping the order of the arguments to amb makes the problem more immediate:
;; the infinitely recursive path is now evaluated first, so every call to
;; parse-verb-phrase now simply hangs indefinitely without the possibility of
;; yielding any results at all.


;; EXERCISE 4.48
;; Extend noun and verb phrases to include adjectives and averbs
(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-noun)))

(define (parse-noun)
  (amb (parse-word nouns)
       (list 'noun-with-adjective-prefix
             (parse-adjective-phrase)
             (parse-noun))))

(define (parse-adjective-phrase)
  (define (maybe-extend adjective-phrase)
    (amb adjective-phrase
         (maybe-extend (list 'adjective-phrase-with-adverbial-suffix
                             adjective-phrase
                             (parse-adverbial-phrase)))))
  (maybe-extend (amb (parse-word adjectives)
                     (list 'adjective-phrase-with-adverbial-prefix
                           (parse-adverbial-phrase)
                           (parse-adjective-phrase)))))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase-with-adverbial-suffix
                             verb-phrase
                             (parse-adverbial-phrase)))))
  (maybe-extend (amb (parse-word verbs)
                     (list 'verb-phrase-with-adverbial-prefix
                           (parse-adverbial-phrase)
                           (parse-verb-phrase)))))

(define (parse-adverbial-phrase)
  (amb (parse-word adverbs)
       (parse-prepositional-phrase)))

(define adjectives '(adjective green smelly fast fun happy nerdy best worst))
(define adverbs '(adverb hastily fast breezily gallantly messily always now))

;;Support compound sentences
(define (parse-sentence)
  (define (maybe-extend sentence)
    (amb sentence
         (maybe-extend (list 'compound-sentence
                             sentence
                             (parse-word conjunctions)
                             (parse-sentence)))))
  (maybe-extend (parse-simple-sentence)))

(define (parse-simple-sentence)
  (list 'simple-sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

(define conjunctions '(conjunction and or but))


;; EXERCISE 4.49
;; Requires "more primitives" from ch4-ambeval.scm

(define (generate-sentences) (flatten-parse-tree (parse-sentence)))

(define (parse-word word-list) (an-element-of (cdr word-list)))

(define (flatten-parse-tree x)
  (define (iter x skip-head?)
    (cond ((pair? x)
           (if skip-head?
               (iter (cdr x) false)
               (append (iter (car x) true)
                       (iter (cdr x) false))))
          ((null? x) x)
          (else (list x))))
  (iter x true))

;; First few sentences generated by generate-sentences (using the original
;; parsing code from section 4.3.2, without any subsequent modifications from
;; exercises):
;;
;; (the student studies)
;; (the student studies for the student)
;; (the student studies for the student for the student)
;; (the student studies for the student for the student for the student)
;; ... and so on ...


;;;SECTION 4.3.3
;;; **SEE ALSO** ch4-ambeval.scm (loadable/runnable evaluator)

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

;; clause for ANALYZE
;;((amb? exp) (analyze-amb exp))
;;
;;* here is analyze with that clause in it (*not* in book)
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((amb? exp) (analyze-amb exp))  ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

;;;Simple expressions

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

;;;Conditionals and sequences

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail)
               (if (true? pred-value)
                   (cproc env succeed fail)
                   (aproc env succeed fail)))
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value fail)
           (b env succeed fail))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

;;;Definitions and assignments

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail)
               (define-variable! var val env)
               (succeed 'ok fail))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail)         ; *1*
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail)))))
             fail))))

;;;Procedure applications

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail)
               (get-args aprocs
                         env
                         (lambda (args fail)
                           (execute-application
                            proc args succeed fail))
                         fail))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    (lambda (arg fail)
                      (get-args (cdr aprocs)
                                env
                                (lambda (args fail)
                                  (succeed (cons arg args)
                                           fail))
                                fail))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;;;amb expressions

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

;;;Driver loop

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))


;; EXERCISE 4.50
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (let ((shuffled-cprocs (shuffle cprocs)))
        (define (try-next choices)
          (if (null? choices)
              (fail)
              ((car choices) env
                             succeed
                             (lambda () (try-next (cdr choices))))))
        (try-next shuffled-cprocs))))))

(define (shuffle xs)
  (define (iter xs ys zs rounds)
    (cond ((null? xs)
           (let ((xs2 (append ys zs)))
           (if (= rounds 0)
               xs2
               (iter xs2 '() '() (- rounds 1)))))
          ((= (random 2) 0)
           (iter (cdr xs)
                 (cons (car xs) ys)
                 zs
                 rounds))
          (else (iter (cdr xs)
                      ys
                      (cons (car xs) zs)
                      rounds))))
  (iter xs '() '() 7))

;; add to analyze
;((ramb? exp) (analyze-ramb exp))

;; One way to improve the sentence generation of exercise 49:
(define (an-element-of items)
  (require (not (null? items)))
  (ramb (car items) (an-element-of (cdr items))))

;; First few results from running generate-sentences in the driver loop:
;;
;; (the professor with a student eats for a cat in a professor to a class)
;;
;; (the professor with a student eats for a cat in a professor to a class
;;   for a student for the student for a class in the student for a student
;;   for the professor for a student)
;;
;; (the professor with a student eats for a cat in a professor to a class
;;   for a student for the student for a class in the student for a student
;;   for the professor for a student to a professor in a student for the cat
;;   to the professor to a class to a student to a professor for a student
;;   for the student by the student for the professor for a professor for a
;;   professor in a student for a professor)

;; Another way:
(define analyze-amb analyze-ramb)

;; First few results from running generate-sentences in the driver loop:
;;
;;   (a student eats)
;;   (a student eats for a class)
;;   (a student eats for a class to a student)
;;   (a student eats for a class to a student by a student)
;;   (a student eats for a class to a student by a student to a professor)


;; EXERCISE 4.51
;: (define count 0)
;:
;: (let ((x (an-element-of '(a b c)))
;:       (y (an-element-of '(a b c))))
;:   (permanent-set! count (+ count 1))
;:   (require (not (eq? x y)))
;:
;:   (list x y count))
;Value: (a b 2)
;: try-again
;Value: (a c 3)

(define (perm-assignment? exp) (tagged-list? exp 'permanent-set!))
(define (perm-assignment-variable exp) (cadr exp))
(define (perm-assignment-value exp) (caddr exp))

(define (analyze-perm-assignment exp)
  (let ((var (perm-assignment-variable exp))
        (vproc (analyze (perm-assignment-value exp))))
  (lambda (env succeed fail)
    (vproc env
           (lambda (val fail2)
             (set-variable-value! var val env)
             (succeed 'ok fail2))
           fail))))

;; add to analyze
;((perm-assignment? exp) (analyze-perm-assignment exp))

;; If permanent-set! is replaced with set! in the example, the values displayed
;; are as follows:
;;
;;   (list x y count)
;;   ;Value: (a b 1)
;;   try-again
;;   ;Value: (a c 1)


;; EXERCISE 4.52
;; Requires "more primitives" from ch4-ambeval.scm

(if-fail (let ((x (an-element-of '(1 3 5))))
           (require (even? x))
           x)
         'all-odd)
;Value: all-odd

(if-fail (let ((x (an-element-of '(1 3 5 8))))
           (require (even? x))
           x)
         'all-odd)
;Value: 8

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-value exp) (cadr exp))
(define (if-fail-alternative exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((vproc (analyze (if-fail-value exp)))
        (aproc (analyze (if-fail-alternative exp))))
    (lambda (env succeed fail)
      (vproc env succeed
             (lambda () (aproc env succeed fail))))))

;; add to analyze
;((if-fail? exp) (analyze-if-fail exp))


;; EXERCISE 4.53
(let ((pairs '()))
 (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
            (permanent-set! pairs (cons p pairs))
            (amb))
          pairs))
;Value: ((8 35) (3 110) (3 20))


;; EXERCISE 4.54
(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))

;; add to analyze
;((require? exp) (analyze-require exp))


;;  what about query assertions, rules, and queries?
;;  ***a few left -- ex 4.59, 4.61, 4.63, 4.64
;;  also append-to-form and assert!


;;;SECTION 4.4

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))


;;;SECTION 4.4.1

;;; data base [assertions]
;;; **see microshaft-data-base in the file ch4-query.scm

;;; Simple queries

(job ?x (computer programmer))

(address ?x ?y)

(supervisor ?x ?x)

(job ?x (computer ?type))

(job ?x (computer . ?type))

;;; Compound queries

(and (job ?person (computer programmer))
     (address ?person ?where))

(or (supervisor ?x (Bitdiddle Ben))
    (supervisor ?x (Hacker Alyssa P)))

(and (supervisor ?x (Bitdiddle Ben))
     (not (job ?x (computer programmer))))

(and (salary ?person ?amount)
     (lisp-value > ?amount 30000))

;;; data base [rules]
;;; **see microshaft-data-base in the file ch4-query.scm

;;; queries
(lives-near ?x (Bitdiddle Ben))

(and (job ?x (computer programmer))
     (lives-near ?x (Bitdiddle Ben)))


;; EXERCISE 4.55
;; a.
(supervisor ?person (Bitdiddle Ben))

;; b.
(job ?person (accounting . ?type))

;; c.
(address ?person (Slumerville . ?where))


;; EXERCISE 4.56
;; a.
(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?where))

;; b.
(and (salary ?person ?amount1)
     (salary (Bitdiddle Ben) ?amount2)
     (lisp-value < ?amount1 ?amount2))

;; c.
(and (supervisor ?person ?boss)
     (not (job ?boss (computer . ?type)))
     (job ?boss ?position))


;; EXERCISE 4.57
(rule (can-replace ?person1 ?person2)
  (and (job ?person1 ?job1)
       (or (job ?person2 ?job1)
           (and (job ?person2 ?job2)
                (can-do-job ?job1 ?job2)))
       (not (same ?person1 ?person2))))

;; a.
(can-replace ?person (Fect Cy D))

;; b.
(and (can-replace ?person1 ?person2)
     (salary ?person1 ?amount1)
     (salary ?person2 ?amount2)
     (lisp-value < ?amount1 ?amount2))


;; EXERCISE 4.58
(rule (big-shot ?person ?division)
  (and (job ?person (?division . ?type))
       (not (and (supervisor ?person ?super)
                 (job ?super (?division . ?type2))))))


;; EXERCISE 4.59
(meeting accounting (Monday 9am)))
(meeting administration (Monday 10am)))
(meeting computer (Wednesday 3pm)))
(meeting administration (Friday 1pm)))
(meeting whole-company (Wednesday 4pm)))

;; a.
(meeting ?dept (Friday ?time))

;; b.
(rule (meeting-time ?person ?day-and-time)
  (or (meeting whole-company ?day-and-time)
      (and (job ?person (?division . ?type))
           (meeting ?division ?day-and-time))))

;; c.
(meeting-time (Hacker Alyssa P) (Wednesday ?time))


;; EXERCISE 4.60
(lives-near ?person (Hacker Alyssa P))
(lives-near ?person-1 ?person-2)

;; The second search expression returns all instantiations of the invoked rule
;; for which the variable assignments satisfy the rule body. For some pair of
;; people a and b who live near each other, (lives-near a b) and
;; (lives-near b a) are both true according to the body of lives-near, and so
;; both instantiations are returned as search results.
;;
;; To filter out the redundant results from a commutative rule like lives-near,
;; we can add an additional condition to our query to allow only results where
;; the arguments follow a standard ordering. This ensures each combination of
;; arguments can occur only once. We can achieve the filtering by using a
;; lisp-value pattern to invoke an external sorting procedure.

;; In Lisp:
(define (in-order? xs ys)
  (let ((lx (length xs)) (ly (length ys)))
  (cond ((< lx ly) true)
        ((> lx ly) false)
        ((null? xs) true)
        ((eq? (car xs) (car ys))
         (in-order? (cdr xs) (cdr ys)))
        (else
         (symbol<? (car xs) (car ys))))))

;; In query evaluator:
(and (lives-near ?person-1 ?person-2)
     (lisp-value in-order? ?person-1 ?person-2))


;;; Logic as programs

(rule (append-to-form () ?y ?y))

(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

(append-to-form (a b) (c d) ?z)

(append-to-form (a b) ?y (a b c d))

(append-to-form ?x ?y (a b c d))


;; EXERCISE 4.61
(rule (?x next-to ?y in (?x ?y . ?u)))

(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z))

;;; Query input:
(?x next-to ?y in (1 (2 3) 4))
;;; Query response:
(1 next-to (2 3) in (1 (2 3) 4))
((2 3) next-to 4 in (1 (2 3) 4))

;;; Query input:
(?x next-to 1 in (2 1 3 1))
;;; Query response:
(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))


;; EXERCISE 4.62
(rule (last-pair (?x) ?x))
(rule (last-pair (?x . ?xs) ?y) (last-pair ?xs ?y))

;; A query like (last-pair ?x (3)) appears to produce an infinite recursion, as
;; evaluation aborts with the error that maximum recursion depth was exceeded.
;; The query system does not have enough information to produce a finite set of
;; results.


;; EXERCISE 4.63
(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)

(rule (grandson ?g ?s)
  (and (son ?f ?s)
       (son ?g ?f)))

(rule (son ?m ?s)
  (and (wife ?m ?w)
       (son ?w ?s)))


;;;SECTION 4.4.2

(job ?x (computer programmer))

(and (can-do-job ?x (computer programmer trainee))
     (job ?person ?x))

(and (supervisor ?x ?y)
     (not (job ?x (computer programmer))))


(lives-near ?x (Hacker Alyssa P))

;; rule for lives-near is in ch4-query.scm

(assert! (job (Bitdiddle Ben) (computer wizard)))

(assert! (rule (wheel ?person)
               (and (supervisor ?middle-manager ?person)
                    (supervisor ?x ?middle-manager))))


;;;SECTION 4.4.3

(and (job ?x (computer programmer))
     (supervisor ?x ?y))

(and (supervisor ?x ?y)
     (job ?x (computer programmer)))

(assert! (married Minnie Mickey))

(married Mickey ?who)

(assert! (rule (married ?x ?y)
               (married ?y ?x)))

(and (supervisor ?x ?y)
     (not (job ?x (computer programmer))))

(and (not (job ?x (computer programmer)))
     (supervisor ?x ?y))


;; EXERCISE 4.64
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))

(outranked-by (Bitdiddle Ben) ?who)

;; The query unifies with the outranked-by rule, causing the body of the rule to
;; be evaluated in an environment with ?staff-person bound to (Bitdiddle Ben)
;; and ?boss and ?who bound to one another, but not to a value. This evaluation
;; reaches a recursive invocation of outranked-by. At the point where this
;; invocation occurs, neither of its arguments has yet been bound to a value:
;; ?middle-manager is a variable with no prior occurrences, and ?boss is not
;; restricted by the first branch of the or-expression since the branches are
;; evaluated independently. As the evaluator evaluates the recursive query,
;; it reaches the *next* recursive invocation of outranked-by, where, for the
;; same reasons as before, neither of the arguments is bound to a value. We have
;; now evaluated outranked-by and reached a recursive invocation with
;; essentially identical arguments (i.e. two arguments not bound to anything),
;; so it is clear that recursion will proceed indefinitely (or at least, it will
;; proceed indefinitely if we attempt to iterate through the full contents of
;; the outermost result stream).
;;
;; This problem does not occur in the original version of outranked-by because
;; the arguments to the and-expression are swapped. The recursive search is
;; gated by (supervisor ?staff-person ?middle-manager), limiting recursive calls
;; to only those situations where a relevant supervisor record exists.


;; EXERCISE 4.65
(wheel ?who)

;; This query evaluates the body of wheel with ?person bound to ?who.
;; A rule produces a result stream based on all valid instantiations of its
;; body. Examining the body of wheel, we see that it holds for multiple
;; combinations of values of the variables ?middle-manager and ?x when ?person
;; is bound to (Warbucks Oliver). The result stream of the body will thus
;; include multiple frames where ?person is (Warbucks Oliver), and each of these
;; will produce a separate instantiation of (wheel ?who) with ?who bound to
;; (Warbucks Oliver).


;; EXERCISE 4.66
;; Some queries that include numeric data may produce multiple results that,
;; while they are not the same result overall, may include the same numeric data
;; as subexpressions. For example, this query to find the average salary among
;; all supervisors --
;;
;;   (average ?amount
;;     (and (supervisor ?report ?boss)
;;          (salary ?boss ?amount)))
;;
;; -- will include results with the same ?boss and same ?amount for each
;; employee who reports to a given supervisor, causing the average calculation
;; to erroneously include the supervisor's salary more than once in cases
;; where a supervisor has multiple direct reports. The result stream includes
;; the salary of Ben Bitdiddle two times:
;;
;;   (and (supervisor (fect cy d) (bitdiddle ben))
;;        (salary (bitdiddle ben) 60000))
;;   (and (supervisor (hacker alyssa p) (bitdiddle ben))
;;        (salary (bitdiddle ben) 60000))
;;
;; One way to fix this issue would be to create a new special form, unique.
;; (unique (<var-1> <var-2> ... <var-n>) <query>) filters the results of <query>
;; in such a way that, for each unique combination of values of <var-1> ...
;; <var-n> that occurs in the frames of the result stream, the first frame
;; containing that combination of values is retained, while subsequent such
;; frames are dropped. Here is how unique could be used to achieve the desired
;; behavior of the present example:
;;
;;   (average ?amount
;;     (unique (?boss)
;;             (and (supervisor ?report ?boss)
;;                  (salary ?boss ?amount))))


;; EXERCISE 4.67
;; The history should be a list of pairs of rules and frames, and the evaluator
;; should use it like a queue. Each time the evaluator is about to evaluate a
;; rule body in a frame, it should check the history to see whether there exists
;; any pair containing that rule alongside a frame with the same variables and
;; values as that frame. If such a pair exists, evaluation terminates with an
;; error. If such a pair does not exist, then a pair containing the rule and
;; frame should be added to the front of the history, and evaluation should
;; proceed as normal. When the last item in the result stream is consumed, the
;; first item of the history list should be dropped.


;; EXERCISE 4.68
;; This solution works for both (reverse ?x (1 2 3)) and (reverse (1 2 3) ?x).
;; I haven't yet figured out a solution that follows the book's suggestion to
;; use append-to-form. The solution below uses an auxiliary rule to mimic the
;; logic of the reverse procedure from exercise 2.18.

(rule (reverse ?a ?b)
  (reverse-iter () () ?a ?b ?b ?a))

(rule (reverse-iter ?x ?y (?a . ?b) (?c . ?d) ?x0 ?y0)
  (reverse-iter (?a . ?x) (?c . ?y) ?b ?d ?x0 ?y0))

(rule (reverse-iter ?x ?y () () ?x ?y))


;; EXERCISE 4.69
(rule ((great . ?rel) ?x ?y)
  (and (son ?x ?s)
       (or (and (same ?rel (grandson))
                (grandson ?s ?y))
           (and (?rel ?s ?y)
                (ends-with ?rel grandson)))))

(rule (ends-with (?x) ?x))
(rule (ends-with (?a ?b . ?c) ?x)
  (ends-with (?b . ?c) ?x))


;;;SECTION 4.4.4
;;; **SEE ALSO** ch4-query.scm (loadable/runnable query system)

;;;SECTION 4.4.4.1
;;;The Driver Loop and Instantiation

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           ;; [extra newline at end] (announce-output output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))


;;;SECTION 4.4.4.2
;;;The Evaluator

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

;;;Simple queries

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

;;;Compound queries

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(put 'and 'qeval conjoin)


(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

(put 'or 'qeval disjoin)

;;;Filters

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
           call
           frame
           (lambda (v f)
             (error "Unknown pat var -- LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore frame-stream) frame-stream)

(put 'always-true 'qeval always-true)

;;;SECTION 4.4.4.3
;;;Finding Assertions by Pattern Matching

(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

;;;SECTION 4.4.4.4
;;;Rules and Unification

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame)) ; {\em ; ***}
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)                     ; {\em ; ***}
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)    ; {\em ; ***}
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

;;;SECTION 4.4.4.5
;;;Maintaining the Data Base

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream assertion
                            current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

;; EXERCISE 4.70
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion THE-ASSERTIONS))
  'ok)

;; This version of add-assertion! won't have the intended effect because
;; cons-stream is a special form which delays evaluation of its second argument
;; (the stream tail) until it is explicitly requested via stream-cdr. So calling
;; stream-cdr on THE-ASSERTIONS will simply return THE-ASSERTIONS itself, and
;; the stream is just an infinite repetition of its head, assertion.
;;
;; Aliasing THE-ASSERTIONS using a let-binding solves the problem because the
;; values of variables in a let-binding are evaluated strictly. The alias
;; variable already points at the intended stream tail (the old stream) when
;; the tail of the new stream is evaluated, so the new stream behaves as
;; expected.


;;;SECTION 4.4.4.6
;;;Stream operations

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1) delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed (force delayed-s2)
                           (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))


(define (singleton-stream x)
  (cons-stream x the-empty-stream))


;;;SECTION 4.4.4.7
;;;Query syntax procedures

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

(define (negated-query exps) (car exps))

(define (predicate exps) (car exps))
(define (args exps) (cdr exps))


(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp)
  (tagged-list? exp '?))

(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
     (if (number? (cadr variable))
         (string-append (symbol->string (caddr variable))
                        "-"
                        (number->string (cadr variable)))
         (symbol->string (cadr variable))))))


;;;SECTION 4.4.4.8
;;;Frames and bindings
(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))


;; EXERCISE 4.71
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append (find-assertions query-pattern frame)
                    (apply-rules query-pattern frame)))
   frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
       (qeval (first-disjunct disjuncts) frame-stream)
       (disjoin (rest-disjuncts disjuncts) frame-stream))))


;; EXERCISE 4.73
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave
       (stream-car stream)
       (flatten-stream (stream-cdr stream)))))

;; EXERCISE 4.74
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))
(define (simple-flatten stream)
  (stream-map ??FILL-THIS-IN??
              (stream-filter ??FILL-THIS-IN?? stream)))

;; EXERCISE 4.75

(unique (job ?x (computer wizard)))

(unique (job ?x (computer programmer)))

(and (job ?x ?j) (unique (job ?anyone ?j)))

(put 'unique 'qeval uniquely-asserted)


;; EXERCISE 4.79

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)
