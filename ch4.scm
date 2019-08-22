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
;; Requires exercise 5
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
;; Requires exercises 5-6

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
;; Requires exercises 4-9 and 12

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
;; Requires exercises 4-9, 12, and 16
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
;; Require exercise 4-9, 12, and 16

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
;; Requires exercises 4-9

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
count
w
count

;; EXERCISE 4.29

(define (square x)
  (* x x))

(square (id 10))
count

;; EXERCISE 4.30

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;PART A
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

;;PART B

(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

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


;; EXERCISE 4.33
;: (car '(a b c))


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

;; EXERCISE 4.47

(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))


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

;; EXERCISE 4.51

(define count 0)

(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (permanent-set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))


;; EXERCISE 4.52

(if-fail (let ((x (an-element-of '(1 3 5))))
           (require (even? x))
           x)
         'all-odd)

(if-fail (let ((x (an-element-of '(1 3 5 8))))
           (require (even? x))
           x)
         'all-odd)


;; EXERCISE 4.53

(let ((pairs '()))
  (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
             (permanent-set! pairs (cons p pairs))
             (amb))
           pairs))


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


;; EXERCISE 4.59
(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))
(meeting whole-company (Wednesday 4pm))

;; EXERCISE 4.60

(lives-near ?person (Hacker Alyssa P))
(lives-near ?person-1 ?person-2)


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

(?x next-to ?y in (1 (2 3) 4))
(?x next-to 1 in (2 1 3 1))


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
