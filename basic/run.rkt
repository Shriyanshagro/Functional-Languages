#lang racket
(require eopl/eopl)
(provide (all-defined-out))

(define-datatype Ast Ast?
  [num (n number?)]
  [primApp (op IspOp?) (rands (list-of Ast?))])

;; operator table keeps all supported operations with metadata
;; table has name, procedure,and arity for each supported operator
;; table can be enhanced by adding signature of arguments
(define opTable (list (list '+ 'plus + 'n)
                      (list '- 'minus - 'n)
                      (list '* 'mul * 'n)
                      (list '/ 'div / 'n)
                      (list '^ 'pow expt 'n)))

;; lookupOp: symbol? -> operator | null
;; looking for match for operator symbol in a table for supported operations
;; if match is found, metadata (p:procedure ar:arity) is returned
(define lookupOp
   (lambda (sym)
     (let ((vals (filter (lambda(u)(equal? (car u) sym)) opTable)))
             (if (not (null? vals))
                 (rest (car vals))
                 null))))


;; lookuppOp: symbol? -> operator | null
;; looking for match for operator symbol in a table for supported operations
;; if match is found, metadata (p:procedure ar:arity) is returned
(define lookuppOp
   (lambda (sym)
     (let ((vals (filter (lambda(u)(equal? (car (cdr u)) sym)) opTable)))
             (if (not (null? vals))
                 (rest (car vals))
                 null))))

;; IspOp?: symbol? -> boolean
;; looks up in the table for match, returns boolean
(define IspOp?
   (lambda (sym)
     (not (null? (lookuppOp sym)))))

;; IsOp?: symbol? -> boolean
;; looks up in the table for match, returns boolean
(define IsOp?
   (lambda (sym)
     (not (null? (lookupOp sym)))))

;; parse: list? --> Ast?
;; throws error if input is not in right format
(define parse
  (lambda (ls)
    (cond
      [(null? ls) (error "Input in bad format:(null)")]
      [(list? ls) ;process the list by traversing it
         (let ((head (first ls))(tail (rest ls)))
          (cond
                [(number? head) (num head)]
                [(IsOp? head) (primApp (car (lookupOp head)) (map parse tail))]
                [else (error "Bad operator type")]))]
       (else (parse (list ls))) ;single item can be converted into list to enter the main code

  )))

(define ans? number?)
;;eval : Ast? --> ans?
;; throws error if arity is mismatched.
(define eval
  (lambda (ast)
    (cases Ast ast
      [num (n)  n]
      [primApp (op rands) (letrec ((opInfo (lookuppOp op))     ;find the operator metadata from the operator table
                                   (proc (second opInfo))     ;get the operator procedure
                                   (arity (third opInfo))     ;get the arity
                                   (args (map eval rands)))   ;evaluate operands to get actual arguments

                            ;; proceed only if either arity is defined as unlimited ('n) or
                            ;; # of actual args match arity

                              (if (or (equal? arity 'n)
                                      (and (number? arity)
                                           (equal? arity (length args))))
                                  (apply proc args)
                                  (error "eval: primApp: " "wrong number of arguments")))]
      )))
