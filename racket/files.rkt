#lang racket

(provide watch-and-update-wiki)

(require uuid)
(require megaparsack megaparsack/text)
(require datalog)
(require data/either)
(require data/maybe)

(require "parser.rkt")
(require "datalog-utils.rkt")

(define wiki-root (string->path "/home/luc/downloads/wiki"))

(define (read-wiki-tree types)
  (define theory (make-theory))
  (define roots (directory-list wiki-root))
  (define evs
    (for/list ([root roots])
      (define entry-root (build-path wiki-root root))
      (define entries (directory-list entry-root))
      (define evs
        (for/list ([entry entries])
          (if (uuid-string? (path->string entry))
            (let ([path (build-path entry-root entry)]
                  [uuid (uuid-string->symbol (path->string entry))])
              (read-entry-files types path uuid theory))
            (display (format "~s is not a valid UUID\n" entry)))))
      (append (flatten evs) (list (filesystem-change-evt entry-root)))))
  (define all-evs (append (flatten evs) (list (filesystem-change-evt wiki-root))))
  (values all-evs theory))

(define (read-entry-files types path uuid theory)
  (define files (directory-list path))
  (flatten
    (for/list ([file files])
      (read-entry-file types path uuid file theory))))

(define (read-entry-file types path uuid file theory)
  (if (or (bytes=? (path-get-extension file) #".meta")
          (char=? #\. (string-ref (path->string file) 0)))
    '()
    (let* ([mtdt (path-replace-extension file ".meta")]
           [mtdt-path (build-path path mtdt)]
           [base (path->string file)]
           [full-path (build-path path file)]
           [ent (entry uuid (just (path->string file)) nothing)])
      (unless (file-exists? mtdt-path) (display-to-file "" mtdt-path))
      (define dtl-parsed
        (parse-string (datalog/p (just types) (just ent))
                      (port->string (open-input-file mtdt-path) #:close? #t)))
      (either 
        (lambda (err)
          (display 
            (format "Parse error in ~a:~n ~a~n" mtdt-path
                    (parse-error->string err)))
          (flush-output))
        (lambda (dtl)
          (let ([query (map (lambda (atom) `(! ,atom)) dtl)])
            (run-datalog theory query)))
        dtl-parsed)
      (define ev (filesystem-change-evt mtdt-path))
      (list ev))))

(define (watch-and-update-wiki types)
  (define theory (box (make-theory)))
  (define (loop)
    (define-values (evs ntheory) (read-wiki-tree types))
    (flush-output)
    (set-box! theory ntheory)
    (apply sync evs)
    (loop))
  (thread loop)
  theory)
