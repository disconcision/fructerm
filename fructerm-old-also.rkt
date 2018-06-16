#lang racket


; attributes
; annotations, asepects, and affordances



; goal; run-time specifiable term rewriting rules

; pattern forms from racket pattern matching that i use;
; list
; literal, var
; ...
; quasiquote, unquote
; splicing-unquote
; app, ?
; and, or, not

(define-values (\\
                //
                /@) (values ((curry list) 'quasiquote)
                            ((curry list) 'unquote)
                            ((curry list) 'unquote-splicing)))

(define-syntax-rule (λ? pattern) (match-lambda [pattern #true] [_ #false]))

(define ((fructerm pattern-templates) target)
  (match-let ([`(,pattern ,template) (first pattern-templates)]
              )
    (define environment (destructure pattern target))
    (if environment
        (restructure template environment)
        ((fructerm (rest pattern-templates)) target))
    ))

(define literal?
  (disjoin number?
           boolean?
           ))

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

(define atom? (compose not pair?))
(define proper-list? (conjoin list? (compose not empty?)))
; pre-applies fn into source tree
(define (map-rec fn source) 
  (match (fn source)
    [(? list? ls) (map (curry map-rec fn) ls)]
    [(? atom? a) a]))
; desugars _ ... into (ooo _)
(define/match (undotdotdot source)
  [((list a ... b '... c ...)) `(,@(undotdotdot a) (ooo ,b) ,@c)]
  [(_) source])
; resugars (ooo _) into _ ...
(define/match (redotdotdot source)
  [(`(,a ... (ooo ,b) ,c ...)) `(,@(redotdotdot a) ,b ... ,@c)]
  [(_) source])


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


(define (restructure template environment)
  0)


; patterns i want to add;
; ...-style splicing unquote
; containment-singleton
; containment-list
; containment-tree
; locality anchor
#; (main-loop
    
    ; amodal functions
    ['f1 (!do (λ (_) backup-stage))] ; restores stage to initial state
    ['f2 (pretty-print (project-symbol stage-gui))] ; print s-expr representing stage
    ['f3 (pretty-print stage-gui)] ; print raw stage data

    (SELECT     
     ['home                  [,a
                              ⋱↦ (▹ ,a)] [(▹ ,a) ⋱↦ ,a]]                         
     [#\return               ([((▹ ,(? form-name? a)) ,x ...)
                               ⋱↦ (c▹ (c▹▹ ,empty-symbol) (,a ,@x))]
                              [(▹ ,a)
                               ⋱↦ (c▹ (c▹▹ ,empty-symbol) ,a)]) 
                             (mode: TRANSFORM)]
                           
     ['right                 (▹-next-? atom?)]
     ['left                  (▹-prev-? atom?)]
     ['up                    [(,a ... (▹ ,b ...) ,c ...)
                              ⋱↦ (▹ (,@a ,@b ,@c))]]
     ['down                  [(▹ (,a ,b ...))
                              ⋱↦ ((▹ ,a) ,@b)]]
                           
     [#\space                simple-paint]
     ['escape                [(⋈ ,a ,b)
                              ⋱↦ ,b]]
                           
     [(reg "[A-Za-z_]")      [(▹ ,a)
                              ⋱↦ (s▹ ,(string key-code) ,((▹▹tag-hits (string key-code)) a))]
                             (mode: SEARCH)])
   
    (SEARCH    
     [(or 'escape #\return)  (compose [(s▹ ,buf ,sel)
                                       ⋱↦ (▹ ,sel)]
                                      [(▹▹ ,a)
                                       ⋱↦ ,a])
                             (mode: SELECT)]
                           
     ['right                 ▹-cycle-▹▹]
                           
     [(or 'left #\backspace) [(s▹ ,buf ,sel)
                              ⋱↦ ,(let ([bu (remove-last-char-str buf)])
                                    `(s▹ ,bu ,((▹▹tag-hits bu) sel)))]]
                           
     [(reg "[A-Za-z0-9_]")   [(s▹ ,buf ,sel)
                              ⋱↦ ,(let ([new ((append-char-to-str key-code) buf)])
                                    `(s▹ ,new ,((▹▹tag-hits new) sel)))]])

    (TRANSFORM
     ['escape                (compose [(c▹ ,buf ,sel)
                                       ⋱↦ (▹ ,sel)]
                                      [(⋈ ,num ,sel)
                                       ⋱↦ ,sel])
                             (mode: select)]
                           
     [#\return               [(c▹ ,buf ,sel)
                              ⋱↦ (▹ ,(([(c▹▹ ,x) ⋱↦ ,x]) (eval-painted-buffer buf sel)))]
                             (mode:select)]
                           
     [(or 'right #\space)    ([(,as ...  (c▹▹ ,(? empty-symbol?)))
                               ⋱↦ (,@as (c▹▹ ,empty-symbol))]
                              [(,as ...  (c▹▹ ,b))
                               ⋱↦ (,@as ,b  (c▹▹ ,empty-symbol))]
                              [(,(and as (not (== 'c▹))) ... (c▹▹ ,b) ,c ,cs ...)
                               ⋱↦ (,@as ,b  (c▹▹ ,c) ,@cs)]
                              [(c▹▹ ,(? empty-symbol?))
                               ⋱↦ (c▹▹ ,empty-symbol)]
                              [(c▹▹ ,a)
                               ⋱↦ (,a  (c▹▹ ,empty-symbol))])]
                           
     ['down                  [(c▹▹ ,a)
                              ⋱↦ ((c▹▹ ,a))]]
                           
     ['up                    ([(,as ... (,bs ... (c▹▹ ,(? empty-symbol?))))
                               ⋱↦ (,@as (,@bs) (c▹▹ ,empty-symbol))]
                              [(,as ... (,bs ... (c▹▹ ,c)))
                               ⋱↦ (,@as (,@bs ,c) (c▹▹ ,empty-symbol))])]
                           
     [(or 'left #\backspace) ([((c▹▹ ,(? empty-symbol?)) ,as ...)
                               ⋱↦ (c▹▹ ,empty-symbol)]
                              [(c▹▹ ,(? symbol? s))
                               ⋱↦  (c▹▹ ,(remove-last-char s))]
                              [(c▹▹ ,(? atom? s))
                               ⋱↦  (c▹▹ ,empty-symbol)]
                              [(c▹ (c▹▹ ,(? empty-symbol?)) ,xs ...)
                               ⋱↦ (c▹ (c▹▹ ,empty-symbol) ,@xs)]
                              [(,xs ... ,(? atom? x) (c▹▹ ,(? empty-symbol?)) ,ys ...)
                               ⋱↦  (,@xs (c▹▹ ,x) ,@ys)]
                              [(,xs ... (,as ...) (c▹▹ ,(? empty-symbol? s)) ,ys ...)
                               ⋱↦  (,@xs (,@as (c▹▹ ,empty-symbol)) ,@ys)])]
                           
     [(reg "[0-9]")          (named-paint-c▹▹ (string->number (string key-code)))]
     ['control               [(c▹ ,buf ,sel)
                              ⋱↦ (c▹ ,buf ,(toggle-paint sel))]]
     [#\tab                  replace-with-first-autocomplete-match]
     [(reg "[A-Za-z_]")      [(c▹▹ ,(? symbol? s))
                              ⋱↦ (c▹▹ ,((append-char-to key-code) s))]])

    (PROJECT))

(module+ test
  (require rackunit)
  #;(check-equal? ((fructerm '(`a 4) ) 'a)
                  4)
  #;(check-equal? (fructerm `(+ 1 2) [,a ⋱↦ (▹ ,a)])
                  `(▹ (+ 1 2)))
  #;(check-equal? ((fructerm '???) [((▹ ,(? form-name? a)) ,x ...) ⋱↦ (c▹ (c▹▹ ,empty-symbol) (,a ,@x))])
                  
                  0)



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
  

  ; yes: maybe refactor for only one level of qq in pattern?




  #;(restructure:
     (check-equal? (destructure ''(1 1) '(1 1))
                   '())


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
                   '(1 (2 3 (1)))))
   
  )

