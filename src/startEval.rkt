; CPSC 3740
; Project - startEval.rkt
; Written By: Eric Den Haan

; Imports
#lang racket
(require test-engine/racket-tests)

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

; Evaluate constants
(define (evalConst constant)
  constant)

; Evaluate quote
(define (evalQuote expr)
  (startEval (second expr)))

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
(define (evalBinOp expr)
  (cond
    [(equal? (car expr) '+) (+ (startEval (second expr)) (startEval (third expr)))]
    [(equal? (car expr) '-) (- (startEval (second expr)) (startEval (third expr)))]
    [(equal? (car expr) '*) (* (startEval (second expr)) (startEval (third expr)))]
    [(equal? (car expr) '/) (/ (startEval (second expr)) (startEval (third expr)))]
    [(equal? (car expr) '=) (= (startEval (second expr)) (startEval (third expr)))]
    [(equal? (car expr) '<=) (<= (startEval (second expr)) (startEval (third expr)))]
    [(equal? (car expr) '>=) (>= (startEval (second expr)) (startEval (third expr)))]
    [(equal? (car expr) '<) (< (startEval (second expr)) (startEval (third expr)))]
    [(equal? (car expr) '>) (> (startEval (second expr)) (startEval (third expr)))]))

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
(define (evalEqual expr)
  (myequal? (startEval(second expr)) (startEval(third expr))))

; startEval function
; All parsing of expressions starts here
(define (startEval program)
  ;(write program)
  (cond
    ; Empty List
    [(null? program) program]
    ; Constants, variables
    [(not(list? program)) (evalConst program)]
    ; quote
    [(equal? (car program) 'quote) (evalQuote program)]
    ; Binary operators
    [(binOpMatch program) (evalBinOp program)]
    ; equal?
    [(equal? (car program) 'equal?) (evalEqual program)]))

; Unit tests

; Constants and variables
(check-expect (startEval '1) 1)
(check-expect (startEval 'a) 'a)

; quote
(check-expect (startEval '(quote 1)) 1)
;(check-expect (startEval '(quote (123))) (quote (123)))

; Binary operators
(check-expect (startEval '(+ 1 2)) 3)
(check-expect (startEval '(+ 1 (+ 1 2))) 4)
(check-expect (startEval '(+ (+ 2 2) (+ 2 2))) 8)
(check-expect (startEval '(- 2 1)) 1)
(check-expect (startEval '(- 2 (- 1 1))) 2)
(check-expect (startEval '(- (- 2 1) (- 2 1))) 0)
(check-expect (startEval '(* 2 1)) 2)
(check-expect (startEval '(* 2 (* 2 1))) 4)
(check-expect (startEval '(* (* 2 2) (* 2 2))) 16)
(check-expect (startEval '(/ 2 1)) 2)
(check-expect (startEval '(/ 4 (/ 2 1))) 2)
(check-expect (startEval '(/ (/ 4 2) (/ 4 2))) 1)
(check-expect (startEval '(= 2 1)) #f)
(check-expect (startEval '(<= 2 1)) #f)
(check-expect (startEval '(>= 2 1)) #t)
(check-expect (startEval '(> 2 1)) #t)
(check-expect (startEval '(< 2 1)) #f)
(check-expect (startEval '(equal? 1 2)) #f)
(check-expect (startEval '(equal? '1 '2)) #f)
(check-expect (startEval '(equal? '(123) '(123))) #t)
(check-expect (startEval '(equal? (+ 1 1) (+ 1 1))) #t)

; Run the tests
(test)