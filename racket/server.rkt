#lang racket

(require ffi/unsafe/custodian)
(require racket/unix-socket)
(require data/maybe)
(require data/either)
(require datalog)
(require megaparsack megaparsack/text)
(require json)

(require "parser.rkt")
(require "datalog-utils.rkt")
(require "files.rkt")

(struct no-unix-sockets exn:fail:user ())

(provide serve)
(define (serve path types rules)
  (unless unix-socket-available?
    (raise (no-unix-sockets 
             "Unix Sockets not available" 
             (current-continuation-marks))))
  (define main-custodian (make-custodian))
  (parameterize ([current-custodian main-custodian])
    (define listener (unix-socket-listen path))
    (register-custodian-shutdown
      'delete-unix-socket
      (lambda (v) (delete-file path))
      main-custodian
      #:at-exit? #t)
    (define theory (watch-and-update-wiki types rules))
    (define (loop)
      (accept-and-handle listener theory)
      (loop))
    (thread loop))
  (lambda () (custodian-shutdown-all main-custodian)))

(define (accept-and-handle listener theory)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values [in out] (unix-socket-accept listener))
    (thread
      (lambda ()
        (handle theory in out)
        (custodian-shutdown-all cust)))))

(define (handle theory in out)
  (define datalog (port->string in #:close? #f))
  (define parsed (parse-string (datalog/p nothing nothing) datalog))
  (either
    (lambda (err)
      (display (format "Couldn't parse datalog;\n~a" (parse-error->string err)) out))
    (lambda (dtl)
      (let* 
        ([dtl (map (lambda (atom) `(! ,atom)) dtl)]
         [result (run-query theory dtl)])
        (for ([mapping result])
             (let
               ([json 
                  (for/list ([[_ val] mapping])
                    (jsonify-datalog-value val))])
               (write-json json out)
               (display "\n" out)))))
    parsed)
  (flush-output out))

(define (run-query theory datalog)
  (define arity (find-query-arity datalog))
  (define args
    (map 
      (lambda (i) (string->symbol (format "X~a" i))) 
      (stream->list (in-range arity))))
  (define datalog-with-query (append datalog `((? (query . ,args)))))
  (define dtl (hash-copy (unbox theory)))
  (run-datalog dtl datalog-with-query))
