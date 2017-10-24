#lang racket
(require "ast.rkt")
(require "parser.rkt")
(require "env.rkt")
(require "eval-ast.rkt")

    (require rackunit)
    (define env1 (extended-env (list (mk-tuple 'x 2)(mk-tuple 'y 3)(mk-tuple 'z 4) (mk-tuple 'b 0)) (empty-env)))
    (define env2 (extended-env (list (mk-tuple 'a 20)(mk-tuple 'b 30)(mk-tuple 'c 40)) env1))
    (define env3 (extended-env (list (mk-tuple 's 200)(mk-tuple 'u 300)(mk-tuple 'v 400)) env2))
    (define G (empty-env))
    (define e1 (extended-env (list (mk-tuple 'x 10) (mk-tuple 'y 20)) G))
    (define e2 (extended-env (list (mk-tuple 'a 30) (mk-tuple 'b 40) (mk-tuple 'c 50)) e1))
    ;;closure
    (check-equal? (closure '(x y) (primApp '+ (list (id 'x) (id 'y))) (extended-env '((x 10) (y 20)) (empty-env)))
	       (eval-ast (parse '(assume ((foo (function (x y) (+ x y)))) foo)) e1) "eval: function bind")

    ;; application
    (check-equal? 5 (eval-ast (parse '(assume ((foo (function (x y) (+ x y)))) (foo 2 3))) e1) "eval: function application1")
    (check-equal? 27 (eval-ast (parse '(assume ((x 20)(y (function(a)(+ a 5))))(y (+ x 2)))) (empty-env)) "eval: function application2")
    (check-equal? 0 (eval-ast (parse '(assume ((x 20)(y (function()(+ x 5))))(y))) (extended-env '((x -5)) (empty-env)))
		               "eval: function application3")

    ;;ifte
    (check-equal? 22 (eval-ast (parse '(assume ((x 20)
		                                (y (function (a) (ifte (IsZero? (- a 1)) (+ a 10) a))))
		                               (y (+ x 2))))
		               (empty-env)) "eval: function application3-ifte-true")

    (check-equal? 11 (eval-ast (parse '(assume ((x 20)
		                                (y (function (a) (ifte (IsZero? (- a 1)) (+ a 10) a))))
		                               (y 1)))
		               (empty-env)) "eval: function application3-ifte-false")

    ;;ifte:error:test_not_boolean
    (check-exn exn? (lambda () (eval-ast (parse '(assume ((x 20)
		                                (y (ifte  (- a 1) (+ a 10) a)))
		                               (+ x y)))
		              (extended-env (list (mk-tuple 'a 10)) (empty-env)))) "eval: assume-ifte-test-not-boolean")
    ;;unbound identifier 
    (check-exn exn? (lambda()(eval-ast (parse 'foo) (empty-env))) "eval-ast:unbound identifier")

    ;; recursion - assume - unbound identifier
    (check-exn exn? (lambda() (eval-ast (parse '(assume ((fac (function (n) (ifte (IsZero? n)
		                                        1
		                                        (*  n (fac (- n 1)))))))
		              (fac 6))) e1) "recursion-test2-unbound-identifier"))


    ; to fix unbound error, pass fac as formal parameter to itself
    (check-equal? 720 (eval-ast (parse '(assume ((fac (function (f n) (ifte (IsZero? n)
		                                        1
		                                        (*  n (f f (- n 1)))))))
		              (fac fac 6))) G) "recursion-test3-nonrecursion-method")
    ;; recursion - assume*
    (check-equal? 720 (eval-ast (parse '(assume* ((fac (function (n) (ifte (IsZero? n)
		                                        1
		                                        (*  n (fac (- n 1)))))))
		              (fac 6))) e1) "recursion-test1")

    ;; recursion - trace-assume
    (eval-ast (parse '(trace-assume ((fac (function (n) (ifte (IsZero? n)
		                                        1
		                                        (*  n (fac (- n 1)))))))
		              (fac 6))) e1)
    ; output =>
    ; > (fac 6)
    ; > (fac 5)
    ; > (fac 4)
    ; > (fac 3)
    ; > (fac 2)
    ; > (fac 1)
    ; > (fac 0)
    ; < 1
    ; < 1
    ; < 2
    ; < 6
    ; < 24
    ; < 120
    ; < 720
    ; 720

    (check-equal? 20 (eval-ast (parse '(trace-assume ((a 10))
		                       (+ x a))) e1) "simple-assume-test1")

    (eval-ast (parse '(trace-assume ((foo (function (n) (* n 2))))
		              (foo 50))) e1)

    ; output =>
    ; > (foo 50)
    ; < 100
    ; 100