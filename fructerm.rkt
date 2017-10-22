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
           (match-lambda [(quasiquote ,(? symbol?)) #true] [_ #false])
           #;(match-lambda [(quote (? literal?)) #true] [_ #false])))

(define (destructure pattern target [quote-level 0])
  (cond [(equal? quote-level 0)
         (cond [(and (list? pattern) (list? target)) (map destructure pattern target)]

               [(and (literal? pattern) (equal? pattern target)) 0]
               ;fallthrough
               [(symbol? pattern) 0 #;(bind pattern environment)]
               
               [else (display "no match or error")]
               )]
        [else
         (cond)]))

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
#;(module+ test
    (require rackunit)
    (check-equal? ((fructerm '(`a 4) ) 'a)
                  4)
    (check-equal? (fructerm `(+ 1 2) [,a ⋱↦ (▹ ,a)])
                  `(▹ (+ 1 2)))
    (check-equal? ((fructerm '???) [((▹ ,(? form-name? a)) ,x ...) ⋱↦ (c▹ (c▹▹ ,empty-symbol) (,a ,@x))])
                  
                  0)
    (check-equal?
     
     0)
    (check-equal?
     
     0)
    (check-equal?
     
     0)
    (check-equal?
     
     0)
    (check-equal?
     
     0)
    (check-equal?
     
     0)
    (check-equal?
     
     0)
    
    
    )

; annotation patterns

#; ([attribute : pattern] rest-of list contents)

#| this matches against a list beginning with a hash containing key 'attribute' whose value matches pattern |#

; but if given a list that doesn't start with list containing a colon as the second element e.g. (a b c),
; this is re-written as (,any-hash a b c)

; attributes are unique and are keywordish. if we try to match
#; (attribute rest-of list contents ...)
#; (attribute arg ... : rest-of list contents ...)
; this gets rewritten to
#; ([attribute : _] rest-of list contents ...)
#; ([attribute : arg ...] rest-of list contents ....)

; e.g.

#; (▹ ,a)
#; ([▹ :] ,sel)

#; (s▹ ,buf : ,sel)
#; ([s▹ : ,buf] ,sel)

; maybe
#; (s: ,buf : ,sel)
#; ([s : ,buf] ,sel)

; alternatively we could 'project' an affordance before matching on it:
; (term-rewrite (app (project-attribute ▹) `(▹ ,a)) ((inject-attribute ▹) `(▹ ,a))


; metafunctionality

; function to automatically left-right reverse a rewrite rule