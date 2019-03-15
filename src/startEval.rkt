;=======================================================================================================================
; CPSC 3740
; Project - evalEnv.rkt
; Written By: Eric Den Haan
;=======================================================================================================================

; Imports
#lang racket
(require test-engine/racket-tests)

;=======================================================================================================================
; Environment data structures and functions
;=======================================================================================================================

; Environment entry struct:
(define-struct envEntry ([value #:mutable]))

; Find a variable in a given environment
(define (envSearch env var)
  (envEntry-value (hash-ref env var)))

; Mutate a value in a given environment
; ! used because we are changing an existing
; environment entry, which is a side-effect
(define (updateEnvEntry! env var val)
  (set-envEntry-value! (hash-ref env var) val))

; Insert one or more variable-value bindings
; into a given environment
(define (envInsert env vars vals)
  (if(and(null? vars) (null? vals))
     env
     (envInsert (hash-set env (car vars) (make-envEntry (car vals))) (cdr vars) (cdr vals))))

; Update multiple variable-value bindings
; into a given environment
(define (updateEnvEntries! env vars vals)
    (cond
      [(and(null? vars) (null? vals)) env]
      [else
       (updateEnvEntry! env (car vars) (car vals))
       (updateEnvEntries! env (cdr vars) (cdr vals))]))

; Return an empty environment:
(define (emptyEnv) (hash))

;=======================================================================================================================
; Evaluation
;=======================================================================================================================

; Evaluate constants
(define (evalConst constant env)
  (cond
    [(symbol? constant) (envSearch env constant)]
    [else constant]))

; Evaluate quote
(define (evalQuote expr env)
  (second expr))

; Test for binary operator
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
    
; Evaluate binary operator
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
(define (evalCar expr env)
  (define (recursivePosition) (car (car (cdr expr))))
  (define (firstElement) (car (car (cdr (car (cdr expr))))))
  (cond
    [(equal? (recursivePosition) 'car) (car (evalEnv (second expr) env))]
    [(equal? (recursivePosition) 'cdr) (car (evalEnv (second expr) env))]
    [(equal? (recursivePosition) 'cons) (car (evalEnv (second expr) env))]
    [else (firstElement)]))

(define (evalCdr expr env)
  (define (recursivePosition) (car (car (cdr expr))))
  (define (lastElement) (cdr (last (car (cdr expr)))))
  (cond
    [(equal? (recursivePosition) 'car) (car (evalEnv (second expr) env))]
    [(equal? (recursivePosition) 'cdr) (cdr (evalEnv (second expr) env))]
    [(equal? (recursivePosition) 'cons) (car (evalEnv (second expr) env))]
    [else (lastElement)]))

(define (evalCons expr env)
  (cons (evalEnv (second expr) env) (evalEnv (third expr) env)))

(define (evalPair expr env)
  (pair? (evalEnv (second expr) env)))

; Evaluate if statements
(define (evalIf stmt env)
  (if (evalEnv (second stmt) env)
      (evalEnv (third stmt) env)
      (evalEnv (fourth stmt) env)))

; Evaluate lambdas

; Some helper functions:

; Return the parameter section of a lambda expression
(define (lambdaParams fn) (second fn))

; Return the body of a lambda expression
(define (lambdaBody fn) (fourth fn))

; Create the the textual form of a lambda expression
(define (createLambdaExpr args body) (list 'lambda (list args) body))

; Determine if a given expression is a lambda expression
(define (lambdaExpr? expr)
  (and (pair? expr) (equal? (car expr) 'lambda) (list? (lambdaParams expr))))

; Determine whether beta reduction is possible
(define (betaReducable? expr)
  ; Return true if expr is a list, where the first item is a lambda expression
  ; and the second item is a value that can be applied to the first item
  (and (pair? expr) (lambdaExpr? expr) (pair? cdr expr)))

; Alpha conversion
(define (alphaConvert expr sourceArg destArg)
  (cond 
    [((equal? expr sourceArg) destArg)]
    ; If we have an ID collision between expr and sourceArg, return expr
    ; Else create a new lambda expression and substitute sourceArg for destArg
    [(lambdaExpr? expr) (if (equal? (lambdaParams expr) sourceArg)
                             expr
                             (createLambdaExpr (lambdaParams expr)
                                               (alphaConvert (lambdaBody expr) sourceArg destArg)))]
    ; If we have a list of expressions, call alpha conversion recursively on each list member
    [(pair? expr) (cons (alphaConvert (car expr) sourceArg destArg)
                        (alphaConvert (cdr expr) sourceArg destArg))]
    [else expr]))

; Beta reduction


; startEval function
; Call evalEnv adding an empty environment
(define (startEval program)
  (evalEnv program (emptyEnv)))

; evalEnv function
; Take the program and empty env passed from evalEnv and initiate parsing
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
    ; lambda
    ; [(equal? (car (car program)) 'lambda) (evalLambda program env)]

    ; If we do not match any of the forms above, 
    ))

;=======================================================================================================================
; Unit tests
;=======================================================================================================================

; Constants and variables
(check-expect (startEval '1) '1)
(check-expect (startEval "Hello") "Hello")

; quote
(check-expect (startEval '(quote a)) (quote a))
(check-expect (startEval '(quote 1)) (quote 1))
(check-expect (startEval '(quote (123))) (quote (123)))

; Binary operators
(check-expect (startEval '(+ 1 2)) (+ 1 2))
(check-expect (startEval '(+ 1 (+ 1 2))) (+ 1 (+ 1 2)))
(check-expect (startEval '(+ (+ 2 2) (+ 2 2))) (+ (+ 2 2) (+ 2 2)))
(check-expect (startEval '(- 2 1)) (- 2 1))
(check-expect (startEval '(- 2 (- 1 1))) (- 2 (- 1 1)))
(check-expect (startEval '(- (- 2 1) (+ 2 1))) (- (- 2 1) (+ 2 1)))
(check-expect (startEval '(* 2 1)) (* 2 1))
(check-expect (startEval '(* 2 (* 2 1))) (* 2 (* 2 1)))
(check-expect (startEval '(* (* 2 2) (+ 2 2))) (* (* 2 2) (+ 2 2)))
(check-expect (startEval '(/ 2 1)) (/ 2 1))
(check-expect (startEval '(/ 4 (/ 2 1))) (/ 4 (/ 2 1)))
(check-expect (startEval '(/ (/ 4 2) (/ 4 2))) (/ (/ 4 2) (/ 4 2)))
(check-expect (startEval '(= 2 1)) (= 2 1))
(check-expect (startEval '(<= 2 1)) (<= 2 1))
(check-expect (startEval '(>= 2 1)) (>= 2 1))
(check-expect (startEval '(> 2 1)) (> 2 1))
(check-expect (startEval '(< 2 1)) (< 2 1))
(check-expect (startEval '(equal? 1 2)) (equal? 1 2))
(check-expect (startEval '(equal? '1 '2)) (equal? '1 '2))
(check-expect (startEval '(equal? '(123) '(123))) (equal? '(123) '(123)))
(check-expect (startEval '(equal? (+ 1 1) (+ 1 1))) (equal? (+ 1 1) (+ 1 1)))

; List operations
(check-expect (startEval '(car '(1 2 3))) (car '(1 2 3)))
(check-expect (startEval '(car '((1 2) 2 3))) (car '((1 2) 2 3)))
(check-expect (startEval '(car '(((1 2) 1 2) 2 3))) (car '(((1 2) 1 2) 2 3)))
(check-expect (startEval '(car (car '((1 2) 3)))) (car (car '((1 2) 3))))
(check-expect (startEval '(car (car (car '((((1 2) 3) 4)))))) (car (car (car '((((1 2) 3) 4))))))
(check-expect (startEval '(cdr '(1 2 3))) (cdr '(1 2 3)))
(check-expect (startEval '(cdr '(2 3 (1 2)))) (cdr '(2 3 (1 2))))
(check-expect (startEval '(cdr '(2 3 ((1 2) 1 2)))) (cdr '(2 3 ((1 2) 1 2))))
(check-expect (startEval '(cdr (cdr '(1 2 (3 4 5))))) (cdr (cdr '(1 2 (3 4 5)))))
(check-expect (startEval '(car (cdr (cdr '(1 2 (3 4 5)))))) (car (cdr (cdr '(1 2 (3 4 5))))))
(check-expect (startEval '(cons 1 2)) (cons 1 2))
(check-expect (startEval '(cons '(1 2) '(1 2))) (cons '(1 2) '(1 2)))
(check-expect (startEval '(cons (cons 1 2) 3)) (cons (cons 1 2) 3))
(check-expect (startEval '(cons (cons 1 2) (car '(3 4)))) (cons (cons 1 2) (car '(3 4))))
(check-expect (startEval '(car (cons (cons 1 2) (cdr '(3 4))))) (car (cons (cons 1 2) (cdr '(3 4)))))
(check-expect (startEval '(pair? '(1 2))) (pair? '(1 2)))
(check-expect (startEval '(pair? (car (cons (cons 1 2) (cdr '(3 4)))))) (pair? (car (cons (cons 1 2) (cdr '(3 4))))))

; if statements
(check-expect (startEval
               '(if (< (+ 1 2) (- 4 3))
                    #t
                    #f))
              (if (< (+ 1 2) (- 4 3))
                  #t
                  #f))

(check-expect (startEval
               '(if (equal? (cons 1 2) (cons 2 1))
                    #t
                    #f))
              (if (equal? (cons 1 2) (cons 2 1))
                    #t
                    #f))

(check-expect (startEval
               '(if (if (< 1 2)
                        #t
                        #f)
                    (+ 1 2)
                    (+ 3 4)))
              (if (if (< 1 2)
                        #t
                        #f)
                    (+ 1 2)
                    (+ 3 4)))

; lambda
(check-expect (startEval `((lambda (x) x) 1)) ((lambda (x) x) 1))

; Run the tests
(test)