#!/usr/bin/env racket
#lang racket

(require json)
(require datalog)
(require "server.rkt")

(define socket-path (make-parameter "~/.local/state/korrvigs/test.sock"))
(define types-path (make-parameter "~/.config/korrvigs/types.sexp"))

(define (bootstrap-datalog)
  (define theory (make-theory))
  (datalog theory
           (! (test a b))
           (! (test b c))
           (! (:- (query X Z) (test X Y) (test Y Z)))
           (? (query X Y))))

(provide main)
(define (main)
  (command-line
    #:program "korrvigs-server"
    #:once-each
    [("-s" "--socket") sock
                       "Specify the unix socket <sock> to use"
                       (socket-path sock)]
    [("-t" "--types") tps
                      "Specify the file <tps> with the types to load"
                      (types-path tps)])
  (define fp (open-input-file (types-path)))
  (define types (apply hash (read fp)))
  (bootstrap-datalog)
  (close-input-port fp)
  (serve (socket-path) types)
  (define (loop)
    (sleep 10)
    (loop))
  (loop))

(main)
