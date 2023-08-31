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
  (define raw-files (directory-list path))
  (define def (string->path "default.meta"))
  (define files (if (member def raw-files)
                  raw-files
                  (append raw-files (list def))))
  (flatten
    (for/list ([file files])
      (read-entry-file types path uuid file theory))))

(define (read-entry-file types path uuid file theory)
  (if (and (not (string=? (path->string file) "default.meta"))
           (or (bytes=? (path-get-extension file) #".meta")
               (char=? #\. (string-ref (path->string file) 0))))
    '()
    (let* ([mtdt (path-replace-extension file ".meta")]
           [mtdt-path (build-path path mtdt)]
           [base (path->string file)]
           [full-path (build-path path file)]
           [sub (if (string=? base "default.meta") nothing (just base))]
           [ent (entry uuid sub nothing)])
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

(define (watch-and-update-wiki types rules)
  (define theory (box (make-theory)))
  (define rules-query 
    (maybe 
      '() 
      (lambda (path) 
        (define parsed
          (parse-string (datalog/p (just types) nothing)
                        (port->string (open-input-file path) #:close? #t)))
        (either
          (lambda (err)
            (display (format "Parse error in ~a:~n~a~n" path (parse-error->string err)))
            (flush-output)
            '())
          (lambda (dtl) (map (lambda (atom) `(! ,atom)) dtl))
          parsed))
      rules))
  (define (loop)
    (define-values (evs ntheory) (read-wiki-tree types))
    (run-datalog ntheory rules-query)
    (flush-output)
    (set-box! theory ntheory)
    (apply sync evs)
    (loop))
  (thread loop)
  theory)

