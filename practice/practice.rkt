#lang racket
(require eopl)
(provide (all-defined-out))
(require racket/trace)

(define wrap(lambda(ls)
	(cond
	[(null? ls) '()]
        [else (cons (list(car ls)) (wrap(cdr ls)))]
		)))

(define count-occurrences(lambda(x ls)
                           (cond
                             [(null? ls) 0]
                             [(pair? (car ls)) (+ (count-occurrences x (cdr ls)) (count-occurrences x (car ls)))]
                             [[equal? x (car ls)] (+ 1 (count-occurrences x (cdr ls)))]
                             [else (count-occurrences x (cdr ls)) ])
                           ))

(define merge(lambda(ls1 ls2)
                      (cond
                        [[null? ls1] ls2]
                        [[null? ls2] ls1]
                        [[<= (car ls1) (car ls2)] (cons (car ls1) (merge (cdr ls1) ls2))]
                        [[< (car ls2) (car ls1)] (cons (car ls2) (merge (cdr ls2) ls1))]
                        )
                      ))

(define product-map(lambda(el1 ls2)
                     (cond
                       [(null? ls2) `()]
                       [(cons (list el1 (car ls2)) (product-map el1 (cdr ls2)))]
                     )
                   ))
(define product(lambda(ls1 ls2)
                 (cond
                   [(null? ls2) '()]
                   [(null? ls1) '()]
                   [[cons (product-map (car ls1) ls2) (product (cdr ls1) ls2)]]
                   
                   )
                 ))

; val, left, right
;inorder - left, root, right
(trace-define traverse(lambda(ls)
                  (cond
                    [(null? ls) '()]
                    [(not (pair? (cdr ls))) traverse(car ls)]
                    [else (flatten(list (list (node (cdr ls)) (car ls)) (right (cdr ls))))]
                    )
                  ))

(trace-define node(lambda(ls)
              (cond
                [(null? ls) '()]
                [(cons? ls) (traverse (car ls))]
                [else car ls]
;                [(cons (car ls) (node(cdr ls)))]
                )
              ))

(trace-define right(lambda(ls)
              (cond
                [(null? ls) '()]
                [(cons? ls) (traverse (cdr ls))]
                [else car ls]
;                [(cons (car ls) (node(cdr ls)))]
                )
              ))

(define (curry f n)
 (let loop ((n n) (args '()))
   (if (zero? n)
       (apply f (reverse args))
       (lambda (x)
         (loop (- n 1) (cons x args))))))

(define findsub(lambda(el1 el2 ls2 ls1)
                 (cond
                   [(null? ls1) #t]
                   [(null? ls2) #f]
                   [(equal? el1 el2) (findsub (car ls1) (car ls2) (cdr ls2) (cdr ls1))]
                   [else (findsub el1 (car ls2) (cdr ls2) ls1)]
                 )
                 ))
(define is-subseq(lambda(ls1 ls2)
                   (cond
                     [(null? ls1) true]
                     [(null? ls2) false]
                     [ (equal? (findsub (car ls1) (car ls2) (cdr ls2)  (cdr ls1)) #t) #t]
                     [else #f]
                     )

                   ))

(define-datatype tree tree?
  [null]		;;; Null
  [nodes (val number?)	;;; Value of the node
        (left tree?)	;;; Left subtree
        (right tree?)])	;;; Right subtree

(define tree-reduce (lambda (x f root)
    (cases tree root
      [null () x]
      [nodes (k left right) (f k (tree-reduce x f left) (tree-reduce x f right))])))
