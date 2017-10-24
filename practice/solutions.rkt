#lang racket
(require eopl)
(provide (all-defined-out))

(define wrap (lambda (ls)
    (cond
     [(null? ls) '()]
     (else (cons (list (car ls)) (wrap (cdr ls)))))))

(define count-occurrences (lambda(x ls) (count-occurrences-cnt x 0 ls)))
(define count-occurrences-cnt (lambda (k cnt ls)
    (cond
      [(null? ls) cnt]
      [(and (not (pair? (car ls))) (equal? (car ls) k)) (count-occurrences-cnt k (+ cnt 1) (cdr ls))]
      [(and (not (pair? (car ls))) (not (equal? (car ls) k))) (count-occurrences-cnt k cnt (cdr ls))]
      (else (+ cnt (+ (count-occurrences-cnt k 0 (car ls)) (count-occurrences-cnt k 0 (cdr ls))))))))

(define merge (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [(null? ls2) ls1]
      [(>= (car ls1) (car ls2)) (cons (car ls2) (merge ls1 (cdr ls2)))]
      (else (cons (car ls1) (merge (cdr ls1) ls2))))))

(define product-sub (lambda (ls c)
    (cond
      [(null? ls) '()]
      (else (cons (list (car ls) c) (product-sub (cdr ls) c))))))

(define product (lambda (ls1 ls2)
    (cond
      [(null? ls2) '()]
      (else (traverse '(2 (1 () ()) (3 () ())))(append (product-sub ls1 (car ls2)) (product ls1 (cdr ls2)))))))

(define traverse (lambda (root)
    (cond
      [(null? root) '()]
      (else (flatten (list (traverse (car (cdr root))) (car root) (traverse (car (cdr (cdr root))))))))))

(define-datatype tree tree?
  [null]		;;; Null
  [node (val number?)	;;; Value of the node
        (left tree?)	;;; Left subtree
        (right tree?)])	;;; Right subtree


(define findpath (lambda (val root)
    (cases tree root
      [null () '()]
      [node (k left right)
       (cond
         [(equal? val k) '()]
         [(> val k) (cons 'right (findpath val right))]
         [(< val k) (cons 'left (findpath val left))])])))

(define (curry f n)
 (let loop ((n n) (args '()))
   (if (zero? n)
       (apply f (reverse args))
       (lambda (x)
         (loop (- n 1) (cons x args))))))

(define member? (lambda (a ls)
  (cond
   [(null? ls) #f]
   [(equal? a (car ls)) #t]
   (else (member? a (cdr ls))))))

(define sublist (lambda (a ls)
  (cond
   [(null? ls) '()]
   [(equal? a (car ls)) (cdr ls)]
   (else (sublist a (cdr ls))))))

(define is-subseq (lambda (ls1 ls2)
   (cond
    [(null? ls1) #t]
    [(member? (car ls1) ls2) (is-subseq (cdr ls1) (sublist (car ls1) ls2))]
    (else #f))))

(define tree-reduce (lambda (x f root)
    (cases tree root
      [null () x]
      [node (k left right) (f k (tree-reduce x f left) (tree-reduce x f right))])))
