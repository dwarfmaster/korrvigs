#lang racket

(require data/maybe)
(require datalog)
(require "parser.rkt")

(provide 
  run-datalog 
  jsonify-datalog-value
  find-query-arity)

(define (run-datalog theory dtl)
  (define query `(datalog theory . ,dtl))
  (define result
    (let ([ns (make-base-empty-namespace)])
      (namespace-attach-module (current-namespace) 'datalog ns)
      (parameterize ([current-namespace ns])
        (namespace-require 'racket/base)
        (namespace-require 'datalog)
        (namespace-set-variable-value! 'theory theory #t)
        (eval query))))
  result)

(define (jsonify-datalog-value val)
  (cond
    [(symbol? val) 
     (symbol->string val)]
    [(and (number? val) (not (integer? val)) (exact? val)) 
     (exact->inexact val)]
    [(entry? val)
     (hash 'uuid (symbol->string (entry-uuid val))
           'sub (maybe 'null identity (entry-sub val))
           'query (maybe 'null identity (entry-query val)))]
    [else 
      val]))

(define (find-query-arity datalog)
  (if (empty? datalog)
    0
    (let ([entry (car datalog)])
      (if (eqv? (car entry) '!)
        (let ([pred (if (eqv? (caadr entry) ':-) (cadadr entry) (cadr entry))])
          (if (eqv? (car pred) 'query)
            (- (length pred) 1)
            (find-query-arity (cdr datalog))))
        (find-query-arity (cdr datalog))))))
