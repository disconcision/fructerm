#lang racket

(require rackunit)
(require racket/hash)

#|

fructerm

a syntax-rewriting-oriented pattern-matching system
in the style of racket/match, but where patterns can
be constructued a run-time.



currently implemented:

- pattern variables/data

- fixed-length list pattern

- ellipses-patterns

- quote (needs more tests)

- quasiquote/unquote (NON-NESTED)

- simple containment patterns


to-come:

- quasiquote/unquote NESTED
- unquote/splicing?
  not necessary, but for the hell of it?

- non-binding wildcard

- complex containment patterns

- sets & hashes

|#


; rewriting internals
(provide runtime-match ; literals → pattern-templates → stx → stx
         destructure   ; stx → env
         restructure)  ; env → stx → stx

; ratch is a match-like syntactic form
(provide ratch)


#| desugar-pattern : stx → stx

 desugar-pattern makes pattern combinators
 more explicit; in particular, it rewrites
 bare sexprs into pconses, and, if they contain
 ellipses literals, into pconses & p...es.

 i'll also take the opportunity to introduce
 some of the combinators.|# 
(define (desugar-pattern stx [quote-level 0])
  (define D (curryr desugar-pattern quote-level))
  (match stx

    ; don't parse quoted content
    [`(quote ,_) stx]
    [`(quasiquote ,(and x (not (? list?))))
     `(quote ,x)]
    [`(quasiquote ,(? list? xs))
     #:when (equal? 0 quote-level)
     (D (map (curryr desugar-pattern (add1 quote-level)) xs))]
    [(and (? symbol?)
          #;(not 'quote)
          #;(not 'quasiquote)
          #;(note 'unquote)
          (not 'p⋱)
          (not 'p⋱until)
          (not 'p...)
          (not 'pcons))
     (match quote-level
       [0 stx]
       [_ `(quote ,stx)])]
    [(list 'unquote x)
     ; #:when (quote-level . = . 1)
     (when (zero? quote-level) (error "bad unquote"))
     (desugar-pattern x (sub1 quote-level))]
    
    ; containment patterns
    [`(⋱ ,pat)
     (D `(p⋱ _ ,pat))]
    [`(⋱ (until ,?-pat) ,pat)
     (D `(p⋱until _ ,?-pat ,pat))]
    [`(,id ⋱ ,pat)
     (D `(p⋱ ,id ,pat))]
    [`(,id ⋱ (until ,?-pat) ,pat)
     (D `(p⋱until ,id ,?-pat ,pat))]
    
    #| lists and ellipses patterns:

   these forms can be most readily understood
   by examining their reconstructions; into cons
   and append respectively. explictly, (p... a b)
   greedily matches pattern a to the initial
   segments of a list|#
    
    [`(,(and #;(not 'quote)
             (not 'p⋱)
             (not 'p⋱until)
             (not 'p...)
             (not 'pcons)) . ,_)
     (D (foldr
         (λ (x acc)
           (match acc
             [`(,_ ,(== '...) ,y ,ys ...)
              `(p... ,x ,y)]
             [_ `(pcons ,x ,acc)]))
         '() stx))]

    ; budget recursion scheme
    [(? list?) (map D stx)]
    [_ stx]))

; wip
(define (apply-expr f stx)
  (match stx
  
    #;[`(quasiquote ,x) 0]
    #;[(list 'unquote x) 0]

    [`(quote ,p)
     `(quote ,(f p))]
    [`(pcons ,p ,ps)
     `(pcons ,(f p) ,(f ps))]
    [`(p... ,p ,ps)
     `(p... ,(f p) ,(f ps))]
    [`(p⋱ ,(? symbol? id) ,arg)
     `(p⋱ ,id ,(f arg))]
    
    [(? symbol?) stx]
    [(or (? number?) (? empty?)) stx]
    [(? list?) (map f stx)])
  )

; wip
(define ((quote-literals literals) stx)
  (define literal?
    (curry hash-has-key? literals))
  (match stx
    [(? literal?) `(quote ,stx)]
    [`(quote ,p) stx]
    [_ (apply-expr (quote-literals literals) stx)]))


; wip
(define ((process-qq quote-level) stx)
  (println "process-qq")
  (println quote-level)
  (println stx)
  (when (quote-level . < . 0) (error "bad unquote"))
  (match stx
    [(? number?) stx]
    [`(quote ,x) `(quote ,x)]
    [`(quasiquote ,x)
     ((process-qq (add1 quote-level)) x)]
    [(and (? symbol?))
     #:when (not (zero? quote-level))
     `(quote ,stx)]
    [(list 'unquote x)
     #:when (equal? 1 quote-level)
     x]
    [(list 'unquote x)
     #:when (quote-level . > . 1)
     (match ((process-qq (sub1 quote-level)) x)
       [`(quote ,w) (list 'unquote ((process-qq (sub1 quote-level)) w))]
       [_ ((process-qq (sub1 quote-level)) x)])
     ]
    [(? list?)
     #:when (not (zero? quote-level))
     (println "lsit case")
     (println stx)
     (println (second stx))
     
     (map (process-qq quote-level) stx)]))

#;(define (explicit⋱ stx)
    (match stx
      [`(,id ⋱ ,pat)
       `(p⋱ ,id ,pat)]))


(check-equal? ((quote-literals #hash((a . _))) 'a)
              '(quote a))
(check-equal? ((quote-literals #hash((a . _))) '(1 a))
              '(1 (quote a)))
(check-equal? ((quote-literals #hash((b . _))) '(1 a))
              '(1 a))
(check-equal? ((quote-literals #hash((b . _))) '(1 (pcons a b)))
              '(1 (pcons a (quote b))))

#| desugar-template : stx → stx
 similar to above. not sure if this function will
 continue to exist. unsure to this point about
 how much i want to enforce pattern/template
 symmetry. right now it's almost identical. |#
(define (desugar-template stx [quote-level 0])
  (define D (curryr desugar-pattern quote-level))
  (match stx

    [`(quote ,_) stx]
    [`(quasiquote ,(and x (not (? list?))))
     `(quote ,x)]
    [`(quasiquote ,(? list? xs))
     (D (map (curryr desugar-pattern (add1 quote-level)) xs))]
    [(and (? symbol?)
          #;(not 'quote)
          #;(not 'quasiquote)
          #;(note 'unquote)
          (not 'p⋱)
          (not 'p⋱until)
          (not 'p...)
          (not 'pcons))
     (match quote-level
       [0 stx]
       [_ `(quote ,stx)])]
    [(list 'unquote x)
     (when (zero? quote-level) (error "bad unquote"))
     (desugar-pattern x (sub1 quote-level))]
    
    [`(,id ⋱ ,pat)
     (D `(p⋱ ,id ,pat))]
    [`(,(and (not 'p⋱)
             (not 'p...)
             (not 'pcons)) . ,_)
     (D (foldr
         (λ (x acc)
           (match acc
             [`(,_ ,(== '...) ,y ,ys ...)
              `(p... ,x ,y)]
             [_ `(pcons ,x ,acc)]))
         '() stx))]
    [(? list?) (map D stx)]
    [_ stx]))


; desugar examples

(check-equal? (desugar-pattern `(a ⋱ 1))
              '(p⋱ a 1))

(check-equal? (desugar-pattern `(a ⋱ (until 2) 1))
              '(p⋱until a 2 1))

(check-equal? (desugar-pattern `(a ⋱ (1 b ...)))
              '(p⋱ a (pcons 1 (p... b ()))))

(check-equal? (desugar-pattern '(1 2 3 d ... 5 f ... 7 8))
              '(pcons 1 (pcons 2 (pcons 3 (p... d (pcons 5 (p... f (pcons 7 (pcons 8 ())))))))))



; helpers for destructuring

#| bind : maybe-env → (env → maybe-env) → maybe-env
   passthrough match failures |#
(define (bind x f)
  (if (equal? 'no-match x)
      'no-match
      (f x)))

#| append-hashes : hash → hash → hash
   for incrementally binding lists along folds|#
(define (append-hashes h1 h2)
  (hash-union
   h1
   ; must be a better way to do this:
   (make-hash (hash-map h2 (λ (k v) (cons k (list v)))))
   #:combine (λ (a b) (append a b))))


#| destructure : stx → env
   info forthcoming. see tests |#
(define (destructure types c-env arg pat)
  (define D (curry destructure types c-env))
  (define (accumulate-matches pat x acc)
    (bind acc
          (λ (_) (bind (destructure types #hash() x pat)
                       (curry append-hashes acc)))))
  (define constructor-id?
    (curry hash-has-key? types))
  (define literal?
    (disjoin number? constructor-id? empty?))
  ; note the empty set case
  (define pattern-variable?
    (conjoin symbol? (negate literal?)))
  
  (match* (pat arg)
    [((? literal?) (== pat))
     c-env]
    [((? pattern-variable?) _)
     (hash-set c-env pat arg)]
    [(`(quote ,p) _)
     #:when (equal? p arg)
     c-env]
    [(`(p⋱ ,cntx-name ,(app (curry D arg) (? hash? new-env)))
      _) ; should this be (? list?) ?
     (hash-union new-env (hash-set c-env cntx-name identity))]
    [(`(p⋱ ,cntx-name ,find-pat)
      `(,xs ...))
     ; split arg list at first match
     (define-values (initial-segment terminal-segment)
       (splitf-at xs
                  (λ (x)
                    (not (hash? (D x `(p⋱ ,cntx-name ,find-pat)))))))
     ; draw the rest of the owl
     (match* (initial-segment terminal-segment)
       [((== xs) _) 'no-match]
       [(`(,is ...) `(,hit ,ts ...))
        (define new-env (D hit `(p⋱ ,cntx-name ,find-pat)))
        (hash-set new-env cntx-name
                  (compose (λ (x) `(,@is ,x ,@ts))
                           (hash-ref new-env cntx-name)))])]
    
    [(`(pcons ,first-pat ,rest-pat)
      `(,first-arg ,rest-arg ...))
     (bind (D (first arg) first-pat)
           (λ (h) (bind (D rest-arg rest-pat)
                        (curry hash-union h))))]
    
    [(`(p... ,p ,ps)
      (? list?))
     (define/match (greedy arg-init arg-tail)
       [('() _)
        (bind (D arg-tail ps)
              (bind (D `() p)
                    (curry hash-union)))]
       [(`(,as ... ,b) `(,cs ...))
        (match (D cs ps)
          ['no-match (greedy as `(,b ,@cs))]
          [new-env
           (match (foldl (curry accumulate-matches p)
                         #hash() `(,@as ,b))
             ['no-match (greedy as `(,b ,@cs))]
             [old-env (hash-union new-env old-env)])])])
     (greedy arg '())]
    [(_ _) 'no-match]))


#| destructure : literals → env → stx
   info forthcoming. see tests |#
(define (restructure types env stx)
  (define R (curry restructure types env))
  (define (constructor-id? id)
    (hash-has-key? types id))
  (define literal?
    (disjoin number? constructor-id? empty?))
  (define variable?
    (conjoin symbol? (negate literal?)))
  (match stx
    [(? literal? d) d]
    [(? variable? id) (hash-ref env id)]
    [`(quote ,d) d]
    [`(pcons ,p ,ps)
     (cons (R p) (R ps))]
    [`(p... ,p ,ps)
     (append (R p) (R ps))]
    [`(p⋱ ,(? symbol? id) ,arg)
     ((hash-ref env id) (R arg))]))


#| runtime-match : literals → pattern/templates → stx → stx |#
(define (runtime-match types pat-tems source)
  (define new-pat-tems (map runtime-match-rewriter pat-tems))
  (match new-pat-tems
    [`() 'no-match]
    [`((,(app desugar-pattern pattern)
        ,(app desugar-template template))
       ,other-clauses ...)
     (define env (destructure types #hash() source pattern))
     (if (equal? 'no-match env)
         (runtime-match types other-clauses source)
         (restructure types env template))]))


#| runtime-match-rewriter : pattern/templates → pattern/templates
   an ad-hoc rewriting phase; possible seed for macro system |#
(define (runtime-match-rewriter pat-tem)
  (match pat-tem
    [(or `(,a ⋱→ ,b) `(⋱→ ,a ,b))
     (let ([ctx (gensym)])
       `((,ctx ⋱ ,a) (,ctx ⋱ ,b)))]
    [_ pat-tem]))


#| ratch : looks like match but tastes like fructerm |#
(define-syntax-rule (ratch source clauses ...)
  (runtime-match #hash() '(clauses ...) source))


; ratch example usage
(check-equal?
 (ratch '(let ([a 1] [b 2]) 0)
   [(form ([id init] ...) body)
    (id ... init ...)])
 (match '(let ([a 1] [b 2]) 0)
   [`(,form ([,id ,init] ...) ,body)
    `(,@id ,@init)]))



#| EXAMPLES & TESTS |#

(module+ test

  
  ; destructure examples and tests

  (define (test-destr source pattern)
    (destructure #hash() #hash()
                 source (desugar-pattern pattern)))

  
  ; destructure literal/variable tests

  (check-equal? (test-destr 1 1)
                #hash())

  (check-equal? (test-destr 1 'a)
                #hash((a . 1)))

  (check-equal? (test-destr 'a 'a)
                #hash((a . a)))

  
  ; destructure quote tests

  (check-equal? (test-destr 'a '(quote a))
                #hash())

  (check-equal? (test-destr '(quote a) 'a)
                #hash((a . (quote a))))

  (check-equal? (test-destr 'a '(quote b))
                'no-match)

  (check-equal? (test-destr '(a 1) '((quote a) 1))
                #hash())

  (check-equal? (test-destr '(a 1) '((quote a) b))
                #hash((b . 1)))

  (check-equal? (test-destr '(a 1) '(quote (a 1)))
                #hash())


  ; destructure quasiquote/unquote tests

  (check-equal?
   (ratch '(if 1 2 3)
     [`(if ,a ,b ,c)
      `(cond [,a ,b]
             [else ,c])])
   (match '(if 1 2 3)
     [`(if ,a ,b ,c)
      `(cond [,a ,b]
             [else ,c])]))

  ;
  ; FIX BUG
  #;
  (check-equal?
   (ratch  '(if 1 2 3)
     [`(if ,a ,b ,c)
      `(cond [`,a ,b]
             [else ,c])])
   (match  '(if 1 2 3)
     [`(if ,a ,b ,c)
      `(cond [`,a ,b]
             [else ,c])]))
  

  
  ; destructure tests for pcons pattern

  (check-equal? (test-destr '() '(pcons () anything))
                'no-match)

  (check-equal? (test-destr '(1) '(pcons 1 ()))
                #hash())

  (check-equal? (test-destr  '(1) '(pcons a ()))
                #hash((a . 1)))

  (check-equal? (test-destr '(1 2) '(pcons 1 (2)))
                #hash())

  (check-equal? (test-destr '(1 2) '(pcons 1 (pcons 2 ())))
                #hash())

  (check-equal? (test-destr '(1 2 3) '(pcons a (2 3)))
                #hash((a . 1)))

  (check-equal? (test-destr '(1 2 3) '(pcons 1 (pcons 2 (pcons 3 ()))))
                #hash())


  ; destructure  tests for p... pattern

  (check-equal? (test-destr '() '(p... a ()))
                #hash((a . ())))

  (check-equal? (test-destr '(1) '(p... a ()))
                #hash((a . (1))))

  (check-equal? (test-destr '(1 2) '(p... a (pcons 2 ())))
                #hash((a . (1))))

  ; destructure tests for p... pattern with complex subpattern

  (check-equal? (test-destr
                 '((1 1) (1 2) (4 5) 3 4)
                 '(p... (a b) (3 4)))
                #hash((a . (1 1 4)) (b . (1 2 5))))


  ; destructure tests for ... multi-pattern

  ; FIX THIS BUG!!!!!!
  #; (check-equal? (test-destr
                    '()
                    '(1 ...))
                   #hash())

  (check-equal? (test-destr
                 '(1)
                 '(2 ...))
                'no-match)

  (check-equal? (test-destr
                 '(a)
                 '(1 ...))
                'no-match)

  (check-equal? (test-destr
                 '(1)
                 '(1 ...))
                #hash())

  (check-equal? (test-destr
                 '()
                 '(f ...))
                #hash((f . ())))

  (check-equal? (test-destr
                 '(1)
                 '(f ...))
                #hash((f . (1))))

  (check-equal? (test-destr
                 '(1)
                 '(1 f ...))
                #hash((f . ())))

  (check-equal? (test-destr
                 '(1 2 3 4 5)
                 '(1 2 d ...))
                '#hash((d . (3 4 5))))

  (check-equal? (test-destr
                 '(1 2 3 4 5)
                 '(1 2 3 d ... 5))
                '#hash((d . (4))))

  (check-equal? (test-destr
                 '(1 2 3 4 5)
                 '(a ... 2 b ... 5))
                #hash((a . (1)) (b . (3 4))))

  (check-equal? (test-destr
                 '(1 2 3 4 5)
                 '(1 a ... 2 b ... 5))
                #hash((a . ()) (b . (3 4))))

  (check-equal? (test-destr
                 '(1 2 3 4 5)
                 '(1 2 3 d ... 5 f ...))
                #hash((f . ()) (d . (4))))

  (check-equal? (test-destr
                 '(1 2 3 4 5 6)
                 '(1 2 3 d ... 5 f ... 6))
                #hash((f . ()) (d . (4))))

  (check-equal? (test-destr
                 '(1 2 3 4 5 6 7)
                 '(1 2 3 d ... 5 f ... 7))
                #hash((f . (6)) (d . (4))))

  (check-equal? (test-destr
                 '(1 2 3 4 4 5 6 6 6 7 8)
                 '(1 2 3 d ... 5 f ... 7 8))
                #hash((f . (6 6 6)) (d . (4 4))))


  ; destructure tests for ... multi-pattern with complex subpattern

  (check-equal? (test-destr
                 '(1 (4 1 (6 2)) (4 3 (6 4)))
                 '(1 (4 a (6 b)) ...))
                #hash((a . (1 3)) (b . (2 4))))


  ; destructure tests for nested ... multi-pattern

  (check-equal? (test-destr
                 '(1 (1 2 3) (4 5 6))
                 '(1 (a ...) ...))
                #hash((a . ((1 2 3) (4 5 6)))))

  (check-equal? (test-destr
                 '(1 (1 2 3) (1 5 6))
                 '(1 (1 a ...) ...))
                #hash((a . ((2 3) (5 6)))))

  (check-equal? (test-destr
                 '(1 (1 0 0 0 1 0 0) (1 0 1 0 0 0))
                 '(1 (1 a ... 1 b ...) ...))
                #hash((a . ((0 0 0) (0)))
                      (b . ((0 0) (0 0 0)))))


  ; destructure tests for simple containment pattern

  (define contain-test
    (λ (source pattern)
      (destructure #hash() #hash()
                   source (desugar-pattern pattern))))

  (check-equal? ((hash-ref (contain-test
                            '(1)
                            '(a ⋱ 1))
                           'a)
                 1)
                '(1))

  (check-equal? ((hash-ref (contain-test
                            '(1 0)
                            '(a ⋱ 1))
                           'a)
                 1)
                '(1 0))

  (check-equal? ((hash-ref (contain-test
                            '(0 1)
                            '(a ⋱ 1))
                           'a)
                 1)
                '(0 1))

  (check-equal? ((hash-ref (contain-test
                            '(0 1 0)
                            '(a ⋱ 1))
                           'a)
                 1)
                '(0 1 0))

  (check-equal? ((hash-ref (contain-test
                            '(0 (0 1) 0)
                            '(a ⋱ 1))
                           'a)
                 1)
                '(0 (0 1) 0))

  ; TODO: should this be valid? or only match lists???
  (check-equal? ((hash-ref (contain-test
                            1
                            '(a ⋱ 1))
                           'a)
                 2)
                2)

  (check-equal? (hash-ref (contain-test
                           1
                           '(a ⋱ b))
                          'b)
                1)

  (check-equal? ((hash-ref (contain-test
                            '(0 2)
                            '(a ⋱ (0 b)))
                           'a)
                 '(0 3))
                '(0 3))

  (check-equal? ((hash-ref (contain-test
                            '(0 (0 2) 0)
                            '(a ⋱ (0 b)))
                           'a)
                 '(0 3))
                '(0 (0 3) 0))



  ; integration tests: basic restructuring

  (check-equal? (runtime-match #hash() '((a 2)) '1)
                2)

  (check-equal? (runtime-match #hash() '(((a b) b)) '(1 2))
                2)

  (check-equal? (runtime-match #hash() '(((a ...) (a ...))) '(1 2))
                '(1 2))


  ; integration tests: ellipses restructuring
  
  (check-equal? (runtime-match #hash() '(((a b ...) (b ... (a)))) '(1 2 3))
                '(2 3 (1)))

  (check-equal? (runtime-match #hash() '(((a b ...) (b ... (a)))) '(1))
                '((1)))

  
  ; integration tests: basic containment de/restructuring
  
  (check-equal? (runtime-match #hash()
                               '(((a ⋱ 1)
                                  (a ⋱ 2)))
                               '(1))
                '(2))

  (check-equal? (runtime-match #hash()
                               '(((a ⋱ 1)
                                  (a ⋱ 2)))
                               '(0 1))
                '(0 2))

  (check-equal? (runtime-match #hash()
                               '(((a ⋱ 1)
                                  (a ⋱ 2)))
                               '(0 (0 0 1) 0))
                '(0 (0 0 2) 0))

  (check-equal? (runtime-match #hash()
                               '(((a ⋱ (0 b))
                                  (a ⋱ (1 b))))
                               '(0 (0 2) 0))
                '(0 (1 2) 0))

  ; todo: more examples!
  )