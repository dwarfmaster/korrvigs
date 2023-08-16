#!/usr/bin/env racket
#lang racket

(require json)
(require datalog)
(require data/maybe)
(require "server.rkt")

(define socket-path (make-parameter "~/.local/state/korrvigs/test.sock"))
(define types-path (make-parameter "~/.config/korrvigs/types.sexp"))
(define rules (make-parameter nothing))

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
                      (types-path tps)]
    [("-r" "--rules") rls
                      "Specify the file <rls> containing prolog rules to include"
                      (rules (just rls))])
  (define fp (open-input-file (types-path)))
  (define types (apply hash (read fp)))
  (close-input-port fp)
  (serve (socket-path) types (rules))
  (define (loop)
    (sleep 10)
    (loop))
  (loop))

(main)
