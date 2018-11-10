#lang racket

(require memoize)
(provide ⋱ ⋱1 ⋱+)

#|

   CONTAINMENT PATTERNS

   this library implements several match-expanders which
   can be required seperately and used with racket/match

   ⋱ , ⋱+ , ⋱1  are match-expanders which implement
   containment patterns. these descend into an s-expression
   to capture matches and their multi-holed context.

   an n-ary context is captured as a normal n-ary procedure

|#


(module+ test
  (require rackunit)

  #|

    (⋱ <pattern>)

    this macher preorder-traverses an s-expression,
    matching on the first instance of <pattern>

  |#


  ; ⋱ finds left-to-write
  (check-true (match `(0 1 2 3)
                [(⋱ 1) #t]))

  ; ⋱ fails
  (check-true (match `(0 0 2 3)
                [(not (⋱ 1)) #t]))
  
  ; ⋱ decends
  (check-true (match `(0 (0 1) 2 3)
                [(⋱ 1) #t]))
  
  ; ⋱ does nothing
  (check-equal? (match `(0 1 2 3)
                  [(⋱ a) a])
                `(0 1 2 3))
  
  ; ⋱ finds arbitrary patterns
  (check-equal? (match `(0 (1 zap) 2 3)
                  [(⋱ `(1 ,a)) a])
                `zap)

  ; ⋱ works top-down
  (check-equal? (match `(zap (0 (zap 3)))
                  [(⋱ `(zap ,a)) a])
                `(0 (zap 3)))

  
  #|

    pattern : (⋱1  <context-id> <pattern>)

    finds unique occurence of <pattern>

  |#

  
  ; ⋱1 insists on a UNIQUE match
  (check-true (match `(0 1 1)
                [(not (⋱1 1)) #t]))
  

  
  #|

    pattern : (⋱ <context-id> <pattern>)

    this generalization captures a one-holed context
    such that application as a function (<context-id> arg)
    replaces the first <pattern> with the value of arg

  |#

  ; contexts have holes
  (check-equal? (match `(0 1 2 3)
                  [(⋱ c 2)
                   (⋱ c 'zap)])
                `(0 1 zap 3))

  ; context is identity
  (check-equal? (match `(0 1 2 3)
                  [(⋱ c a)
                   (⋱ c 'whatever)])
                'whatever)

  (check-equal? (match `(0 1 2 3)
                  [(⋱ c a)
                   a])
                `(0 1 2 3))
 
  ; contexts descend 
  (check-equal? (match '(0 0 ((▹ 7) 0 0))
                  [(⋱ c `(▹ ,a))
                   (c `(▹▹ ,a))])
                '(0 0 ((▹▹ 7) 0 0)))

  #|

    template: (⋱ <context-id> arg)

    note that (⋱ <context-id> arg) can be
    optionally used in template for symmetry

  |#

  (check-equal? (match '(0 0 ((▹ 7) 0 0))
                  [(⋱ c `(▹ ,a))
                   (⋱ c `(▹▹ ,a))])
                '(0 0 ((▹▹ 7) 0 0)))

  #|

    (⋱+ <context-id> <pattern>)


    this generalization captures a multi-holed context
    such that application as a function (<context-id> args ...)
    replaces each <pattern> matches with its respective arg

  |#

  (check-equal? (match '(0 1 (0 1 (1 0)) 0 1)
                  [(⋱+ c 1)
                   (⋱+ c '(3 4 5 6))])
                '(0 3 (0 4 (5 0)) 0 6))

  #|

    we can use this with a nested match to rewrite
    the captured sub-patterns as a list:

  |#

  (check-equal? (match '(0 1 (0 1 (1 (▹ 0))) 0 1)
                  [(⋱+ c (and a (or `(▹ ,_) (? number?))))
                   (⋱+ c (match a [`(,x ... (▹ ,y) ,z ,w ...)
                                   `(,@x ,y (▹ ,z) ,@w)]))])
                '(0 1 (0 1 (1 0)) (▹ 0) 1))

  (check-equal? (match '(0 (s 1) (2 (s (▹ 3)) (4 5)) (s 6) 7)
                  [(⋱+ c (and a `(s ,_)))
                   (⋱+ c (match a [`(,x ... (s (▹ ,y)) (s ,z) ,w ...)
                                   `(,@x (s ,y) (s (▹ ,z)) ,@w)]))])
                '(0 (s 1) (2 (s 3) (4 5)) (s (▹ 6)) 7))

  #|

    (⋱+ <context-id> (capture-when <when-pattern>) <list-pattern>)

    this generalization captures all matches on when-pattern
    and the matches the list of results against <list-pattern>

  |#

  ; moving a cursor to the next atom
  (check-equal? (match '(0 1 (0 1 (1 (▹ 0))) 0 1)
                  [(⋱+ c (capture-when (or `(▹ ,_) (? number?)))
                       `(,x ... (▹ ,y) ,z ,w ...))
                   (⋱+ c 
                       `(,@x ,y (▹ ,z) ,@w))])
                '(0 1 (0 1 (1 0)) (▹ 0) 1))


  ; toy scope-aware subtitution
  (check-equal? (match `(let ([w 1])
                          z
                          (let ([z 2])
                            z)
                          (let ([y 3])
                            z))
                  [(⋱+ c (until `(let ([z ,_]) ,_ ...))
                      'z)
                   (⋱+ c (make-list 2 'new-name))])
                `(let ([w 1])
                   new-name
                   (let ([z 2])
                     z)
                   (let ([y 3])
                     new-name)))
 
  )



#|
   CONTAINMENT PATTERNS IMPLEMENTATION
|#


(define-match-expander ⋱
  ; containment pattern (returns first match)
  (λ (stx)
    (syntax-case stx ()
      [(⋱ <internal-pat>)
       #'(⋱ _ <internal-pat>)]
      [(⋱ context-id <internal-pat>)
       #'(app
          (curry first-containment (match-lambda? <internal-pat>))
          `(,context-id (,<internal-pat>)))]))
  (λ (stx)
    (syntax-case stx ()
      [(⋱ context-id internal-template)
       #'(context-id internal-template)])))


(define-match-expander ⋱1
  ; containment pattern (returns first match)
  (λ (stx)
    (syntax-case stx ()
      [(⋱1 <internal-pat>)
       #'(⋱1 _ <internal-pat>)]
      [(⋱1 context-id <internal-pat>)
       #'(app
          (curry multi-containment (match-lambda? <internal-pat>))
          `(,context-id (,<internal-pat>)))]))
  (λ (stx)
    (syntax-case stx ()
      [(⋱1 context-id internal-template)
       #'(context-id internal-template)])))


(define-match-expander ⋱+
  ; containment pattern matcher
  ; returns multiple matches; template is list to splice in
  (λ (stx)
    (syntax-case stx (capture-when)
      [(⋱+ context-id (capture-when <cond-pat>) <internal-pat>)
       #'(app
          (curry multi-containment (match-lambda? <cond-pat>))
          `(,context-id ,<internal-pat>))]
      [(⋱+ context-id (until <cond-pat>) <internal-pat>)
       #'(app
          (λ (x) (multi-containment (match-lambda? <internal-pat>) x
                                    (match-lambda? <cond-pat>)))
          `(,context-id (,<internal-pat> (... ...))))]
      [(⋱+ context-id <internal-pat>)
       #'(app
          (curry multi-containment (match-lambda? <internal-pat>))
          `(,context-id (,<internal-pat> (... ...))))]))
  (λ (stx)
    (syntax-case stx ()
      [(⋱+ context-id internal-template)
       #'(apply context-id internal-template)])))



(define-syntax-rule (match-lambda? <pat>)
  ; converts a pattern into a predicate
  (match-lambda [<pat> #t] [_ #f]))



(define (multi-split ls lengths)
  ; splits list ls into segments of lengths lengths
  (unless (equal? (length ls)
                  (apply + lengths))
    (error "length of list doesn't partition"))
  (define-values (actual extra)
    (for/fold ([acc '()]
               [ls ls])
              ([l lengths])
      (define-values (this others)
        (split-at ls l))
      (values (cons this acc) others)))
  (reverse actual))


(define/memo (multi-containment match? xs (until? (λ (x) #f)))
  ; this returns a list of two elements
  ; the first element is the multi-holed context as a fn
  ; the second is a list of the contents of those holes
  (cond
    [(match? xs)
     (list (λ (x) x) `(,xs))]
    [(or (not (list? xs)) (until? xs))
     (list (λ () xs) `())]
    [else
     (define subpairs
       (for/list ([x xs])
         (multi-containment match? x until?)))
     (define subcontexts
       (map first subpairs))
     (define submatches
       (apply append (map second subpairs)))
     (define subcontext-arities
       (map procedure-arity subcontexts))
     (define (context-candidate . args)
       (for/list ([subfn subcontexts]
                  [arg-list (multi-split args
                                         subcontext-arities)])
         (apply subfn arg-list)))
     (define new-context
       (procedure-reduce-arity context-candidate
                               (apply + subcontext-arities)))
     (list new-context
           submatches)]))


(define/memo (first-containment match? xs (until? (λ (x) #f)))
  ; this returns a list of two elements
  ; the first element is a one-holed context as a fn
  ; the second is a one-element list of the content of that hole
  ; this currently is just a gloss for mult-containment
  ; it could be implemented more efficiently separately
  (match-define (list context matches)
    (multi-containment match? xs until?))
  (if (empty? matches)
      (list context matches)
      (list (λ (x) (apply context x (rest matches))) (list (first matches)))))


(module+ test
  (require rackunit)

  (check-equal? ((first (multi-containment (curry equal? 1) '(0 0 1 0 1))) 3 4)
                '(0 0 3 0 4))
  
  (check-equal? ((first (multi-containment (curry equal? 1) '(0 1 (0 1 (1 0)) 0 1) )) 3 4 5 6)
                '(0 3 (0 4 (5 0)) 0 6))

  (check-equal? ((first (multi-containment (curry equal? 1)
                                           '(0 1 (0 1 (1 0)) 0 1)
                                           (curry equal? '(1 0)))) 3 4 5)
                '(0 3 (0 4 (1 0)) 0 5))

  (check-equal? ((first (first-containment (curry equal? 1) '(0 0 1 0 1))) 3)
                '(0 0 3 0 1))

  (check-equal? ((first (first-containment (curry equal? 1) '(0 0 0 0 0))))
                '(0 0 0 0 0))
)

