#lang racket


#| 2018.06.15

 preserved for posterity. this is the remains of
 my initial attempt at fructerm; mostly just a
 quasiquotation implementation. the commented-out
 tests were never implemented.

 goal: run-time specifiable term rewriting rules

 pattern forms from racket/match which i use:
 - list
 - literal, var
 - ...
 - quasiquote, unquote
 - splicing-unquote
 - app, ?
 - and, or, not

|#


; a sketch integration:

#; (define ((fructerm pattern-templates) target)
     (match-let ([`(,pattern ,template) (first pattern-templates)])
       (define environment (destructure pattern target))
       (if environment
           (restructure template environment)
           ((fructerm (rest pattern-templates)) target))))

#; (define (restructure template environment)
     identity)

#; (check-equal? ((fructerm '(`a 4) ) 'a)
                 4)
#; (check-equal? (fructerm `(+ 1 2) [,a ⋱↦ (▹ ,a)])
                 `(▹ (+ 1 2)))
#; (check-equal? ((fructerm '???) [((▹ ,(? form-name? a)) ,x ...) ⋱↦ (c▹ (c▹▹ ,empty-symbol) (,a ,@x))])
                 0)


; helpers for destructure:

(define-syntax-rule (λ? pattern)
  (match-lambda [pattern #true] [_ #false]))

(define literal?
  (disjoin number? boolean?))

(define (flatten-unless-binding ls)
  (match ls
    ["error" '("error")]
    [`(,(and a (not (? list?)) (not "error")) ,(and b (not (? list?)) (not "error"))) `((,a ,b))]
    [(? list?) (apply append (map flatten-unless-binding ls))]))

(define (collate results)
  (let ([flat-results (flatten-unless-binding results)])
    (if (empty? (filter (λ? "error") flat-results))
        flat-results
        "error")))


; destructure

(define (destructure pattern target [quote-level 0])
  (displayln "destructuring:")
  (displayln pattern)
  (displayln target)
  (displayln quote-level)
  (match* (pattern target)

    ; (1 ... 2) -> ((ooo 1) 2)
    [((list (list 'ooo pat) other) _)
     (displayln "confusing")
     (displayln "resume work here")]
              
    [((? literal?) (== pattern))
     (displayln "plain literal match case")
     `()]

    [((? symbol?) (== pattern))
     #:when (> quote-level 0)
     (displayln "symbol literal success case")
     '()]
              
    [((? symbol?) (not (? symbol?)))
     ;does second clause make sense? can't bind a bare symbol
     #:when (equal? quote-level 0)
     (displayln "symbol identifier bind case")
     `((,pattern ,target))]
             
    [((list 'quasiquote a) _)
     (displayln "quasiquote case")
     (destructure a target (add1 quote-level))]
           
    [((list 'unquote a) _)
     #:when (> quote-level 0)
     (displayln "unquote case")
     (destructure a target (sub1 quote-level))]

    [((list 'quote a) _)
     (displayln "quote case")
     (destructure a target +inf.0)]
    ; need to make this symmetric in target like below
    ; all the time? sometimes?

    #;[((list 'quote (? list? ps)) (? list? ts))
       (displayln "quote list case")
       (collate (map (curryr destructure +inf.0) ps ts))]
              
    [((? list?) (? list?))
     #:when (and (equal? (length pattern) (length target))
                 (> quote-level 0))
     (displayln "list case")
     (collate (map (curryr destructure quote-level) pattern target))]

    #; [((? literal?) (not (== pattern)))
        (displayln "plain literal no-match case")
        "error"]
           
    [(_ _)
     (displayln "fallthrough no match")
     "error"]))



(module+ test
  (require rackunit)


  (check-equal? (flatten-unless-binding '(a 2))
                '((a 2)))
  (check-equal? (flatten-unless-binding '((a 2)))
                '((a 2)))
  (check-equal? (flatten-unless-binding '((a 2) (b 3)))
                '((a 2) (b 3)))
  (check-equal? (flatten-unless-binding '((a 2) ((b 3))))
                '((a 2) (b 3)))
  (check-equal? (flatten-unless-binding '((a 2) ((b 3) (c 4) ((d 5) "error"))))
                '((a 2) (b 3) (c 4) (d 5) "error"))

  
  
  (check-equal? (destructure 1 1)
                '())
  (check-equal? (destructure '1 '1)
                '())
  (check-equal? (destructure 'a 'a)
                "error")
  ; does above make sense? logic: can't bind a bare symbol
  ; instead we should do:
  (check-equal? (destructure 'a ''a)
                '((a 'a)))
  (check-equal? (destructure 'a 4)
                '((a 4)))
  (check-equal? (destructure ''a 4)
                "error")
  (check-equal? (destructure '`(1) '(1))
                '())
  (check-equal? (destructure '`(,1) '(1))
                '())
  (check-equal? (destructure '`(,a) '(1))
                '((a 1)))
  (check-equal? (destructure '`(a) '(1))
                "error")
  (check-equal? (destructure '`,a '(1))
                `((a (1))))
  (check-equal? (destructure 'a '(1))
                `((a (1))))
  (check-equal? (destructure '`(,a ,b) '(1 2))
                '((a 1) (b 2)))
  (check-equal? (destructure '(a b) '(1 2))
                "error")
  (check-equal? (destructure '`(1 ,b) '(1 2))
                '((b 2)))
  (check-equal? (destructure '`(a ,b) '(a 2))
                '((b 2)))
  (check-equal? (destructure '`(a ,b) '(1 2))
                "error")
  (check-equal? (destructure '`(1 2) '(1 2))
                '())
  (check-equal? (destructure '`(,a ,b ,c) '(1 2 3))
                '((a 1) (b 2) (c 3)))
  (check-equal? (destructure '`(,a (,b)) '(1 (2)))
                '((a 1) (b 2)))
  (check-equal? (destructure '`(,a ,b) '(1 2 3))
                "error")
  (check-equal? (destructure '`(,a ,b ,c) '(1 2))
                "error")
  (check-equal? (destructure '(,a ,b) '(1 2))
                "error")
  (check-equal? (destructure '(,a ,b) ''(1 2))
                "error")
  (check-equal? (destructure ''(a b) ''(1 2))
                "error")
  (check-equal? (destructure ''(a b) '(a b))
                '())
  ; logic of below: target is just (1 1); not valid
  (check-equal? (destructure '(1 1) '(1 1))
                "error")
  (check-equal? (destructure ''(1 1) '(1 1))
                '())
  (check-equal? (destructure '`(1 `(2 ,a ,,b)) '(1 (2 a 7)))
                '((b 7)))
  
  ; why is below an error? does qq not nest the way i think?
  #; (match '(1 (2 a 7))
       [`(1 `(2 ,a ,,b)) b])
  (check-equal? (destructure '`(1 `(2 ,a ,,,b)) '(1 (2 a 7)))
                "error")
  ; yes. maybe refactor for only one level of qq in pattern?

  )


#;(module+ test
    
    (check-equal? (destructure '... '(1 2 3))
                  "error") ; is bare ellipses just an error?
    (check-equal? (destructure '`(...) '(1 2 3))
                  "error")
    (check-equal? (destructure '`(... ...) '(1 2 3))
                  "error")
    (check-equal? (destructure '`(a ... ...) '(1 2 3))
                  "dunno????")

  
    (check-equal? (destructure '`(... 3) '(1 2 3))
                  "error")
    (check-equal? (destructure '`(1 ... ...) '())
                  "dunno bout this case")
    (check-equal? (destructure '`(1 ...) '())
                  '())
    (check-equal? (destructure '`(1 ...) '(1))
                  '())
    (check-equal? (destructure '`(1 ...) '(1 1 1))
                  '())
    (check-equal? (destructure '`(1 ... 2) '(1 1))
                  '())
    (check-equal? (destructure '`(1 ... 2 ...) '(1 1 2 2 2))
                  '())

  
    (check-equal? (destructure '`(... ,a) '(1 2 3))
                  "error")
    (check-equal? (destructure '`(a ...) '`(a ...))
                  `())
    (check-equal? (destructure '`(,a ...) '1)
                  "error")
    (check-equal? (destructure '`(,a ...) '())
                  '(())) ; is this right??
    (check-equal? (destructure '`(,a ...) '(1))
                  '((a (1))))
    (check-equal? (destructure '`(,a ...) '(1 2 3))
                  '(a (1 2 3)))
    (check-equal? (destructure '`(1 ,a ...) '(1 2 3))
                  '(a (2 3)))
    (check-equal? (destructure '`(1 ,a ... 3) '(1 2 3))
                  '(a (2)))
    (check-equal? (destructure '`(1 ,a ... 3) '(1 3))
                  '())
    (check-equal? (destructure '`(,a ... 3) '(1 2 3))
                  '(a (1 2)))

  
    (check-equal? (destructure '`(,a ,b ...) '(1 2 3))
                  '((a (1)) (b (2 3))))
    (check-equal? (destructure '`(,a ,b ...) '(1))
                  '((a (1)) (b '())))
    (check-equal? (destructure '`(,a ... ,b) '(1 2 3))
                  '((a (1 2)) (b (3))))
    (check-equal? (destructure '`(,a ... ,b ...) '(1 2 3))
                  "error")
    (check-equal? (destructure '`(,a ... ,b ...) '())
                  "dunno")
    (check-equal? (destructure '`(,a ... 3 ,b ...) '(1 2 3))
                  '((a '(1 2)) (b '())))

  
    (check-equal? (destructure '`(() ...) '(1 2 3))
                  "dunno bout this")
    (check-equal? (destructure '`((1) ...) '(1 2 3))
                  "dunno bout this")
    (check-equal? (destructure '`((,a) ...) '((1) (2) (3)))
                  '((a (1 2 3))))
    (check-equal? (destructure '`((,a 1) ...) '((5 1) (4 1)))
                  '((a (5 4))))
    (check-equal? (destructure '`((,a b) ...) '((5 1) (4 1)))
                  '((a (5 4)) (b 1 1)))

    (check-equal? (destructure '`((...) ...) '((1)))
                  "error")
    (check-equal? (destructure '`((1 ...) ...) '((1 1 1 1) (1)))
                  '()) ; or nested empty sets?
    (check-equal? (destructure '`((,a ...) ...) '((5 3 1) (4 2)))
                  '((a ((5 3 1) (4 2)))))
    (check-equal? (destructure '`((,a ...) ...) '((5 3 1) ()))
                  '((a ((5 3 1) ()))))
    (check-equal? (destructure '`((,a ... 1 ,b ... ) ...) '((5 3 1) (1)))
                  '((a ((5 3) ())) (b (() ()))))
    )


#;(module+ test
    
    (check-equal? (restructure '1 '())
                  1)
    (check-equal? (restructure ''(1 1) '())
                  '(1 1))
    (check-equal? (restructure 'a '((a 1)))
                  1)
    (check-equal? (restructure 'a '())
                  "error")
    (check-equal? (restructure 'a '((b 1)))
                  "error")
    (check-equal? (restructure '(a) '((a 1)))
                  "error")
    (check-equal? (restructure ''(a) '((a 1)))
                  '(a))
    (check-equal? (restructure '`(a) '((a 1)))
                  '(a))
    (check-equal? (restructure '(,a) '((a 1)))
                  "error")
    (check-equal? (restructure '`(,a) '((a 1)))
                  '(1))
    (check-equal? (restructure '`(,a a) '((a 1)))
                  '(1 a))
    (check-equal? (restructure '`(,a ,a) '((a 1)))
                  '(1 1))
    (check-equal? (restructure '`(,a ,b) '((a 1)))
                  "error")
    (check-equal? (restructure '`(,a ,b) '((b 2) (a 1)))
                  '(1 2))
    (check-equal? (restructure '`(,a (,b ,c (,b))) '((a 1) (b 2) (c 3)))
                  '(1 (2 3 (1))))
    (check-equal? (restructure '`(,a `(,b ,,c `(,,,b ,c))) '((a 1) (b 2) (c 3)))
                  '(1 (2 3 (1))))
    )
