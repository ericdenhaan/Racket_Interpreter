;=======================================================================================================================
; CPSC 3740
; Project - startEval.rkt
; Written By: Eric Den Haan
;=======================================================================================================================

; Imports
#lang racket
(require test-engine/racket-tests)

;=======================================================================================================================
; Environment data structures and functions
;=======================================================================================================================

; Environment entry struct:
; #:mutable allows value to be changed
(define-struct envEntry ([value #:mutable]))

; Find a variable in a given environment
(define (envSearch env var)
  (envEntry-value (hash-ref env var)))

; Mutate a value in a given environment
; ! used because we are changing an existing environment entry, which is a side-effect
(define (updateEnvEntry! env var val)
  (set-envEntry-value! (hash-ref env var) val))

; Insert one or more variable-value bindings into a given environment
(define (envInsert env vars vals)
  (if(and(null? vars) (null? vals))
     env
     (envInsert (hash-set env (car vars) (make-envEntry (car vals))) (cdr vars) (cdr vals))))

; Update multiple variable-value bindings in a given environment
(define (updateEnvEntries! env vars vals)
  (cond
    [(and(null? vars) (null? vals)) env]
    [else
     (updateEnvEntry! env (car vars) (car vals))
     (updateEnvEntries! env (cdr vars) (cdr vals))]))

; Return an empty environment - use a hash table data structure
(define (emptyEnv) (hash))

;=======================================================================================================================
; Evaluation of primitive expressions
;=======================================================================================================================

; Evaluate constants
; If the constant is a symbol, look in the environment to see if it is defined
; Else just return the constant
(define (evalConst constant env)
  (cond
    [(symbol? constant) (envSearch env constant)]
    [else constant]))

; Evaluate quote
; Simply return the second part of the quote expression
(define (evalQuote expr env) (second expr))

; Check if the first element of the expression is a binary operator
(define (binOpMatch expr)
  (cond
    [(equal? (car expr) '+) #t]
    [(equal? (car expr) '-) #t]
    [(equal? (car expr) '*) #t]
    [(equal? (car expr) '/) #t]
    [(equal? (car expr) '=) #t]
    [(equal? (car expr) '<=) #t]
    [(equal? (car expr) '>=) #t]
    [(equal? (car expr) '<) #t]
    [(equal? (car expr) '>) #t]
    [else #f]))

; Evaluate binary operators
(define (evalBinOp expr env)
  (cond
    [(equal? (car expr) '+) (+ (evalEnv (second expr) env) (evalEnv (third expr) env))]
    [(equal? (car expr) '-) (- (evalEnv (second expr) env) (evalEnv (third expr) env))]
    [(equal? (car expr) '*) (* (evalEnv (second expr) env) (evalEnv (third expr) env))]
    [(equal? (car expr) '/) (/ (evalEnv (second expr) env) (evalEnv (third expr) env))]
    [(equal? (car expr) '=) (= (evalEnv (second expr) env) (evalEnv (third expr) env))]
    [(equal? (car expr) '<=) (<= (evalEnv (second expr) env) (evalEnv (third expr) env))]
    [(equal? (car expr) '>=) (>= (evalEnv (second expr) env) (evalEnv (third expr) env))]
    [(equal? (car expr) '<) (< (evalEnv (second expr) env) (evalEnv (third expr) env))]
    [(equal? (car expr) '>) (> (evalEnv (second expr) env) (evalEnv (third expr) env))]))

; myequal? function used for evalEqual?
(define (myequal? x y)
  ; If x and y are not pairs, simply compare using equal?
  (if (and (not (pair? x)) (not (pair? y)))
      (equal? x y)
      ; If x and y are pairs,
      ; recursively call myequal? on the first element of each list
      (if (and (pair? x) (pair? y))
          (if (myequal? (car x) (car y))
              ; If the recursive check of the first elements returns #t,
              ; recursively check the rest of the lists
              (myequal? (cdr x) (cdr y))
              #f)
          #f)))

; Evaluate equal?
(define (evalEqual expr env)
  (myequal? (evalEnv (second expr) env) (evalEnv (third expr) env)))

; List operations

; Check if list operations are recursively being called
(define (recursivePosition expr)
  (car (car (cdr expr))))

; car
(define (evalCar expr env)
  (define (firstElement) (car (car (cdr (car (cdr expr))))))
  (cond
    [(equal? (recursivePosition expr) 'car) (car (evalEnv (second expr) env))]
    [(equal? (recursivePosition expr) 'cdr) (car (evalEnv (second expr) env))]
    [(equal? (recursivePosition expr) 'cons) (car (evalEnv (second expr) env))]
    [else (firstElement)]))

; cdr
(define (evalCdr expr env)
  (define (lastElement) (cdr (last (car (cdr expr)))))
  (cond
    [(equal? (recursivePosition expr) 'car) (car (evalEnv (second expr) env))]
    [(equal? (recursivePosition expr) 'cdr) (cdr (evalEnv (second expr) env))]
    [(equal? (recursivePosition expr) 'cons) (car (evalEnv (second expr) env))]
    [else (lastElement)]))

; cons
(define (evalCons expr env)
  (cons (evalEnv (second expr) env) (evalEnv (third expr) env)))

; pair?
(define (evalPair expr env)
  (pair? (evalEnv (second expr) env)))

; Evaluate if statements
; Construct a new if statement based on the given expression and execute
(define (evalIf stmt env)
  (if (evalEnv (second stmt) env)
      (evalEnv (third stmt) env)
      (evalEnv (fourth stmt) env)))

;=======================================================================================================================
; Evaluation of lambda functions and function application
;=======================================================================================================================

; Return the parameters of a lambda expression
(define (lambdaParams fn) (second fn))

; Return the body of a lambda expression
(define (lambdaBody fn) (third fn))

; Create the the textual form of a lambda expression
(define (createLambdaExpr params body env)
  (list 'lambda params body env))

; Evaluate lambda expressions by creating and returning the correct lambda form
(define (evalLambda expr env)
  (createLambdaExpr (lambdaParams expr) (lambdaBody expr) env))

; Determine if a given expression is a function application
(define (functionApplication? expr)
  (pair? expr))

; Return the environment of a given function
(define (functionEnvironment fn) (fourth fn))

; Return the argument list for a function application
; Evaluate each argument using evalEnv, and insert the result into a list
(define (argList exprs env)
  (map (lambda (argument) (evalEnv argument env)) exprs))

; Evaluate a function application
; Call applyFunction passing the evaluated function as well as the evaluated argument list
(define (evalFunctionApplication expr env)
  (applyFunction
   (evalEnv (car expr) env)
   (argList (cdr expr) env)))

; Apply a function
; Insert the parameters of the lambda function into the environment
; Insert the arguments (which have been evaluated in argList) as the values for the parameters
; Call evalEnv using the body of the function and the environment returned by envInsert
(define (applyFunction fn args)
  (evalEnv
   (lambdaBody fn)
   (envInsert
    (functionEnvironment fn) (lambdaParams fn) args)))

;=======================================================================================================================
; Local binding (let and letrec)
;=======================================================================================================================

; Return the bindings from a let expression
(define (letBindings expr) (second expr))

; Return the body of a let expression
(define (letBody expr) (third expr))

; Return the variable names of bindings
(define (bindingVars bindings)
  (map (lambda (binding) (car binding)) bindings))

; Return the variable values of bindings
(define (bindingVals bindings env)
  (map (lambda (binding) (evalEnv (second binding) env)) bindings))

; Return a list of empty values for a set of bindings
(define (emptyBindingVals bindings)
  (map (lambda (binding) null) bindings))

; Initialize a set of bindings with names and empty values in an environment
(define (initializeBindings bindings env)
  (envInsert env (bindingVars bindings) (emptyBindingVals bindings)))

; Insert a set of bindings with names and values into an environment
(define (insertBindings bindings env)
  (envInsert env (bindingVars bindings) (bindingVals bindings env)))

; Update a set of bindings with names and values in an environment
(define (updateBindings! bindings env)
  (updateEnvEntries! env (bindingVars bindings) (bindingVals bindings env)))

; Evaluate let statements
; Create the equivalent lambda form of a letrec expression and evaluate it as a function application
; The environment used by evalEnv contains the inserted variables and values given as bindings
(define (evalLet expr env)
  (define (functionPart)
    (list (list 'lambda (bindingVars (letBindings expr)) (letBody expr))))
  (define (argPart)
    (bindingVals (letBindings expr) env))
  (evalEnv
   (append (functionPart) (argPart))
   (insertBindings (letBindings expr) env)))

; evalLetrec function
; Create the equivalent lambda form of a letrec expression and evaluate it as a function application
; The environment used by evalEnv contains the bindings of the letrec expression
; The bindings are first inserted with blank values
; They are then updated with the values given in the letrec expression
(define (evalLetrec expr env)
  (define (functionPart)
    (list (list 'lambda '() (letBody expr))))
  (evalEnv
   (functionPart)
   (updateBindings!
    (letBindings expr)
    (initializeBindings (letBindings expr) env))))

;=======================================================================================================================
; startEval
;=======================================================================================================================

; startEval function
; Call evalEnv and pass an empty environment
(define (startEval program)
  (evalEnv program (emptyEnv)))

; evalEnv function
; Parse the expression to determine its form and evaluate the expression appropriately
(define (evalEnv program env)
  (cond
    ; Empty list
    [(null? program) program]
    ; Constants, variables
    [(not(list? program)) (evalConst program env)]
    ; quote
    [(equal? (car program) 'quote) (evalQuote program env)]
    ; Binary operators
    [(binOpMatch program) (evalBinOp program env)]
    ; equal?
    [(equal? (car program) 'equal?) (evalEqual program env)]
    ; List operations
    [(equal? (car program) 'car) (evalCar program env)]
    [(equal? (car program) 'cdr) (evalCdr program env)]
    [(equal? (car program) 'cons) (evalCons program env)]
    [(equal? (car program) 'pair?) (evalPair program env)]
    ; if
    [(equal? (car program) 'if) (evalIf program env)]
    ; let expressions
    [(equal? (car program) 'let) (evalLet program env)]
    ; letrec expressions
    [(equal? (car program) 'letrec) (evalLetrec program env)]
    ; lambda expressions
    [(equal? (car program) 'lambda) (evalLambda program env)]
    ; Function application - note, this comes last to avoid confusion with other forms
    [(functionApplication? program) (evalFunctionApplication program env)]
    ; If none of the above cases are matched, report an error
    [else (writeln "Failure to evaluate - expression does not match defined language constructs")]))

;=======================================================================================================================
; Unit tests
;=======================================================================================================================

; Constants and variables
(check-expect (startEval
               '1)
              '1)

(check-expect (startEval
               "Hello")
              "Hello")

; quote
(check-expect (startEval
               '(quote a))
              (quote a))

(check-expect (startEval
               '(quote (123)))
              (quote (123)))

; Binary operators
(check-expect (startEval
               '(+ (+ 2 2) (+ 2 2)))
              (+ (+ 2 2) (+ 2 2)))

(check-expect (startEval
               '(- (- 2 1) (+ 2 1)))
              (- (- 2 1) (+ 2 1)))

(check-expect (startEval
               '(* (* 2 2) (+ 2 2)))
              (* (* 2 2) (+ 2 2)))

(check-expect (startEval
               '(/ (/ 4 2) (/ 4 2)))
              (/ (/ 4 2) (/ 4 2)))

(check-expect (startEval
               '(= 2 1))
              (= 2 1))

(check-expect (startEval
               '(<= 2 1))
              (<= 2 1))

(check-expect (startEval
               '(>= 2 1))
              (>= 2 1))

(check-expect (startEval
               '(> 2 1))
              (> 2 1))

(check-expect (startEval
               '(< 2 1))
              (< 2 1))

(check-expect (startEval
               '(equal? '(123) '(123)))
              (equal? '(123) '(123)))

(check-expect (startEval
               '(equal? (+ 1 1) (+ 1 1)))
              (equal? (+ 1 1) (+ 1 1)))

; List operations
(check-expect (startEval
               '(car (car (car '((((1 2) 3) 4))))))
              (car (car (car '((((1 2) 3) 4))))))

(check-expect (startEval
               '(car (cdr (cdr '(1 2 (3 4 5))))))
              (car (cdr (cdr '(1 2 (3 4 5))))))

(check-expect (startEval
               '(pair? (car (cons (cons 1 2) (cdr '(3 4))))))
              (pair? (car (cons (cons 1 2) (cdr '(3 4))))))

; if statements
(check-expect (startEval
               '(if (if (< 1 2) #t #f) (+ 1 2) (+ 3 4)))
              (if (if (< 1 2) #t #f) (+ 1 2) (+ 3 4)))

; lambda expressions and function application
(check-expect (startEval
               '((lambda (x) x) 1))
              ((lambda (x) x) 1))

(check-expect (startEval
               '((lambda (x) (quote x)) 1))
              ((lambda (x) (quote x)) 1))

(check-expect (startEval
               '(((lambda (x y) (lambda (z) (* z y))) 5 6) 10))
              (((lambda (x y) (lambda (z) (* z y))) 5 6) 10))


; Local bindings (let and letrec)
(check-expect (startEval
               '(let ([x 1] [y 2]) (+ x y)))
              (let ([x 1] [y 2]) (+ x y)))

(check-expect (startEval
               '(letrec ([x 1] [y 2] [z 3]) (+ x (+ y z))))
              (letrec ([x 1] [y 2] [z 3]) (+ x (+ y z))))

; Test from project specification document
(check-expect (startEval
               '(letrec ((fact
                          (lambda (x)
                            (if (= x 0) (quote 1)
                                (* x (fact (- x 1)))))))
                  (fact 10)))
              3628800)

; Run the tests
(test)