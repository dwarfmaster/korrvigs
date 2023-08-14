#lang racket

(provide datalog/p entry-parts/p)
(provide (struct-out entry))

(require megaparsack megaparsack/text)
(require uuid)
(require data/functor data/applicative data/monad)
(require data/maybe)
(require datalog)

(struct entry (uuid sub query))

(define (maybe/p parser) 
  (or/p
    (map just parser)
    (pure (nothing))))
(define (>> m1 m2) (chain (lambda (discard) m2) m1))
(define seq/m
  (letrec
    ([loop (lambda seq 
             (case (length seq)
               [(0) (pure '())]
               [(1) (car seq)]
               [else (>> (car seq) (apply loop (cdr seq)))]))])
    loop))

(define (between/p first last bt)
  (do first
      [vl <- bt]
      last
      (pure vl)))

(define spaces/p (many/p space/p))

(define (args/p parser #:sep [sep/p (>> (try/p (>> spaces/p (char/p #\,))) spaces/p)])
  (letrec
    ([do-parser/p
       (or/p
         (do
           [vl <- parser]
           [lst <- do-sep/p]
           (pure (cons vl lst)))
         (pure null))]
     [do-sep/p 
       (or/p
         (>> sep/p do-parser/p)
         (pure null))])
    do-parser/p))

(define uuid/p
  (map uuid-string->symbol
       (guard/p
         (map list->string (repeat/p 36 any-char/p))
         uuid-string?
         "Valid UUID")))

(define label/p
  (map list->string (many+/p (char-not-in/p "'/#\\"))))

(define entry-parts/p
  (do
    [uuid <- uuid/p]
    [sub <- (maybe/p (>> (char/p #\/) label/p))]
    [query <- (maybe/p (>> (char/p #\#) label/p))]
    (pure (entry uuid sub query))))

(define (self/p self-id)
  (define self/p (seq/m (char/p #\s) (char/p #\e) (char/p #\l) (char/p #\f)))
  (maybe
    (guard/p (try/p self/p) (lambda (_) #f))
    (lambda (self-id) 
      (try/p 
        (do
          self/p
          [query <- (maybe/p (>> (char/p #\#) 
                                 (between/p (char/p #\') (char/p #\') label/p)))]
          (pure (maybe
                  self-id
                  (lambda (query) (entry 
                                    (entry-uuid self-id) 
                                    (entry-sub self-id) 
                                    (just query)))
                  query)))))
    self-id))

(define entry/p
  (between/p
    (char/p #\') (char/p #\')
    entry-parts/p))

(define string/p
  (let ([escape/p 
          (lambda (char v) (>> (try/p (>> (char/p #\\) (char/p char))) (pure v)))])
    (map list->string
         (between/p (char/p #\") (char/p #\")
                    (many/p (or/p
                              (escape/p #\" #\")
                              (escape/p #\\ #\\)
                              (escape/p #\a (integer->char #x07))
                              (escape/p #\b (integer->char #x08))
                              (escape/p #\e (integer->char #x1b))
                              (escape/p #\f (integer->char #x0c))
                              (escape/p #\n (integer->char #x0a))
                              (escape/p #\r (integer->char #x0d))
                              (escape/p #\t (integer->char #x09))
                              (escape/p #\v (integer->char #x0b))
                              (char-not/p #\")))))))

(define number/p
  (let
    ([num/p (do [sign <- (maybe/p (char/p #\-))]
              [num <- (many+/p digit/p)]
              (pure (* (if (just? sign) -1 1) (string->number (list->string num)))))]
     [readE (lambda (str) (if (empty? str) 0 (string->number (list->string str))))])
    (chain
      (lambda (nb)
        (let ([num (car nb)]
              [mdot (cadr nb)]
              [expo (caddr nb)])
          (maybe
            (pure (* num (expt 10 expo)))
            (lambda (dot)
              (let* ([e0 (length dot)]
                     [e (- expo e0)]
                     [base (+ (* num (expt 10 e0)) (readE dot))])
                (pure (* base (expt 10 e)))))
            mdot)))
      (guard/p
        (list/p
          num/p
          (maybe/p (>> (char/p #\.) (many/p digit/p)))
          (or/p (>> (char/p #\e) num/p) (pure 0)))
        (lambda (nb)
          (or (just? (cadr nb)) (>= (caddr nb) 0)))
        "An integer cannot have a negative exponent"))))

(define var/p
  (let ([var-letter/p
          (guard/p
            any-char/p
            (lambda (c)
              (or (char-alphabetic? c)
                  (char-numeric? c)
                  (eq? c #\_))))])
    (do 
      [st <- (try/p (guard/p any-char/p char-upper-case? "Upper case character"))]
      [tl <- (many/p (try/p var-letter/p))]
      (pure (string->symbol (list->string (cons st tl)))))))

(define (atom/p self-id)
  (or/p
    (self/p self-id)
    entry/p
    string/p
    number/p
    var/p))

(define ident/p
  (let* ([symbols "_-"]
         [base? (lambda (c) (or 
                              (and
                                (char-alphabetic? c) 
                                (char-lower-case? c))
                              (string-contains? symbols (string c))))]
         [in? (lambda (c) (or
                            (char-alphabetic? c)
                            (char-numeric? c)
                            (string-contains? symbols (string c))))])
    (do 
      [base <- (guard/p any-char/p base? 
                        (format "An alphabetic character or one of ~s" symbols))]
      [body <- (many/p 
                 (try/p
                   (guard/p any-char/p in?
                            (format "An alphanumeric character of one of ~s" symbols))))]
      (pure (string->symbol (list->string (cons base body)))))))

(define (typecheck types pred)
  (maybe
    #t
    (lambda (types)
      (define (type-atom type atom)
        (or
          (and (symbol=? type 'string) (string? atom))
          (and (symbol=? type 'number) (number? atom))
          (and (symbol=? type 'entry) (entry? atom))
          (symbol? atom))) ; We don't typecheck variables
      (define name (car pred))
      (if (hash-has-key? types name)
        (let* ([args (cdr pred)]
               [types (hash-ref types name)])
          (and
            (eqv? (length args) (length types))
            (andmap type-atom types args)))
        #f))
    types))

(define (pred/p types self-id)
  (let ([oparen (integer->char 40)]
        [cparen (integer->char 41)])
    (do
      [name <- (try/p ident/p)]
      [args <- (maybe/p
                 (do
                   (try/p (>> spaces/p (char/p oparen)))
                   spaces/p
                   [args <- (args/p (atom/p self-id))]
                   spaces/p
                   (char/p cparen)
                   (pure args)))]
      (guard/p 
        (pure (cons name (from-just null args)))
        (lambda (pred) (typecheck types pred))
        "A well-typed predicate"))))

(define (constr/p self-id)
  (do
    [arg1 <- (atom/p self-id)]
    spaces/p
    [op <- (or/p (>> (char/p #\=) (pure '=)) 
                 (seq/m (char/p #\!) (char/p #\=) (pure '!=)))]
    spaces/p
    [arg2 <- (atom/p self-id)]
    (pure (list op arg1 arg2))))

(define (rule/p types self-id)
  (let* ()
    (do
      [hd <- (pred/p types self-id)]
      spaces/p
      [args <- (maybe/p
                 (do
                   (try/p (>> (char/p #\:) (char/p #\-)))
                   spaces/p
                   [preds <- (args/p (or/p (pred/p types self-id) (constr/p self-id)))]
                   spaces/p
                   (pure preds)))]
      (char/p #\.)
      (pure (maybe hd (lambda (args) `(:- ,hd . ,args)) args)))))

(define (datalog/p types self-id)
  (do
    spaces/p
    [rules <- (args/p (rule/p types self-id) #:sep spaces/p)]
    eof/p
    (pure rules)))
