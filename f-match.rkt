#lang racket


(provide f/match phash phash-plus p⋱ p⋱+)

(require (for-syntax racket/match
                     racket/list
                     racket/function)
         racket/hash
         "fructerm-common.rkt")


(define-syntax (f/match stx)
  (syntax-case stx (/)
    [(f/match source pairs ...)
     (let ([new-pairs (map rewrite-pairs (syntax->datum #'(pairs ...)))])
       (with-syntax ([(newest-pairs ...) (datum->syntax stx new-pairs)])
         #'(match source newest-pairs ...)))]))

(define-for-syntax (rewrite-p/ stx)
  (match stx
    [`(,context ⋱ ,pat)
     `(p⋱ ,context ,(rewrite-p/ pat))]
    [`(,context ⋱+ ,pat ...)
     `(p⋱+ ,context ,@(map rewrite-p/ pat))]
    ; changed to support multi-arg templates...
    ; should probably seperate pattern/template rewriting at some point
    [(list anns ... and-pat '... / a)
     (list 'quasiquote (list 'p/ (list 'unquote `(phash-plus ,@anns ,and-pat)) (list 'unquote (rewrite-p/ a))))]
    [`(,anns ... / ,a)
     (list 'quasiquote (list 'p/ (list 'unquote `(phash ,@anns)) (list 'unquote (rewrite-p/ a))))]    
    [(? list?) (map rewrite-p/ stx)]
    [_ stx]))

(define-for-syntax (rewrite-pairs stx)
  (match stx
    [`(,pat ,xs ... ,tem)
     `(,(rewrite-p/ pat) ,@xs ,(rewrite-p/ tem))]
    [_ stx]))




(define-match-expander phash
  (λ (stx)
    (syntax-case stx ()
      [(phash anns ...)
       (let ([new-anns
              (for/list ([ann (syntax->datum #'(anns ...))])
                (if (symbol? ann)
                    `(,(list 'quote ann) ,ann)
                    `(,(first ann) . ,(rewrite-p/ (rest ann)))#;ann))]) ; kind of a hack (also see below) for selection-list
         (with-syntax ([(newest-anns ...) (datum->syntax stx new-anns)])
           #'(hash-table newest-anns ...)))]))
  (λ (stx)
    (syntax-case stx ()
      [(phash anns ...)
       (let ([new-anns
              (apply append
                     (for/list ([ann (syntax->datum #'(anns ...))])
                       (if (symbol? ann)
                           `(,(list 'quote ann) ,ann)
                           `(,(first ann) . ,(rewrite-p/ (rest ann))))))]) ; hack see above
         (with-syntax ([(newest-anns ...) (datum->syntax stx new-anns)])
           #'(hash newest-anns ...)))])))


(define-match-expander phash-plus
  (λ (stx)
    (syntax-case stx ()
      [(phash-plus anns ... rest-pat)
       (let ([new-anns
              (for/list ([ann (syntax->datum #'(anns ...))])
                (if (symbol? ann)
                    `(,(list 'quote ann) ,ann)
                    ann))])
         (with-syntax ([(newest-anns ...) (datum->syntax stx new-anns)])
           #'(hash-table newest-anns ... rest-pat (... ...))))]))
  (λ (stx)
    (syntax-case stx ()
      [(phash-plus anns ... rest-pat)
       (let ([new-anns
              (apply append
                     (for/list ([ann (syntax->datum #'(anns ...))])
                       (if (symbol? ann)
                           `(,(list 'quote ann) ,ann)
                           ann)))])
         (with-syntax ([(newest-anns ...) (datum->syntax stx new-anns)])
           #'(hash-union (for/fold ([acc #hash()])
                                   ([r rest-pat])
                           (match r
                             [`(,k ,v) (hash-set acc k v)]))
                         (hash newest-anns ...)
                         #:combine/key (λ (k v1 v2) (if (equal? v1 v2) v1 v2 #;(error (string-append "hash error" (~v v1) " " (~v v2))))))))])))

(module+ test
  (require rackunit)
  (check-equal? (match #hash((a . 70))
                  [(phash a) a])
                70)
  (check-equal? (match #hash((a . 70) (b . 80))
                  [(phash-plus b rd) rd])
                '((a 70)))

  (check-equal? (f/match '(p/ #hash( (> . >) (sort . expr)) (p/ #hash((b . 8)) 2))
                  [( > a ... / (b / 2))
                   a])
                '((sort expr)))

  (check-equal? (f/match '(p/ #hash( (> . >) (sort . expr)) (p/ #hash((b . 8)) 666))
                  [( > a ... / (b / 666))
                   (a ... / (b / 667))])
                '(p/
                  #hash((sort . expr))
                  (p/ #hash((b . 8)) 667)))

  (check-equal? (f/match '(p/ #hash( (> . 2) (sort . expr)) (p/ #hash((b . 8)) 666))
                  [( (> w) a ... / (b / 666))
                   ( (> w) a ... / (b / 667))])
                '(p/
                  #hash((> . 2) (sort . expr))
                  (p/ #hash((b . 8)) 667)))

  (check-equal? (f/match '(p/ #hash( (> . >) (sort . expr)) (p/ #hash((b . 8)) 2))
                  [( > (sort 'expr) / (b / 2))
                   b])
                8)

  (check-equal? (f/match '(p/ #hash( (> . >) (sort . expr)) (p/ #hash((b . 8)) 2))
                  [(> (sort 'expr) / (b / 2))
                   (> (sort 'pat) / (b / 2))])
                '(p/
                  #hash((sort . pat) (> . >))
                  (p/ #hash((b . 8)) 2)))

  ; verify this test case is actually right.....
  (check-equal? (f/match '(p/
                           #hash((in-scope . ())
                                 (selection-list
                                  .
                                  ((p/ #hash((sort . expr) (♦ . ♦)) 0)
                                   (p/
                                    #hash((sort . expr))
                                    (app (p/ #hash((sort . expr)) ⊙) (p/ #hash((sort . expr)) ⊙)))
                                   (p/
                                    #hash((sort . expr))
                                    (λ ((p/ #hash((sort . pat)) ⊙)) (p/ #hash((sort . expr)) ⊙)))
                                   (p/ #hash((sort . expr)) (var (p/ #hash((sort . char)) ⊙)))))
                                 (sort . expr)
                                 (▹ . ▹))
                           ⊙)
                  [(▹ ('selection-list `(,xs ... ,(♦ anns ... / whatever) ,( anns2 ... / w2) ,ws ...))
                      / s)
                   (▹ ('selection-list `(,@xs ,(anns ... / whatever) ,(♦ anns2 ... / w2) ,@ws))
                      / s)]
                  #;[(▹ ('selection-list `(,xs ... (p/ ,(hash-table (♦  ♦) y-rest ...) ,y) (p/ ,(hash-table z-rest ...) ,z) ,ws ...))
                        / s)
                     0])
                '(p/
                  #hash((selection-list
                         .
                         ((p/ #hash((sort . expr)) 0)
                          (p/
                           #hash((sort . expr) (♦ . ♦))
                           (app (p/ #hash((sort . expr)) ⊙) (p/ #hash((sort . expr)) ⊙)))
                          (p/
                           #hash((sort . expr))
                           (λ ((p/ #hash((sort . pat)) ⊙)) (p/ #hash((sort . expr)) ⊙)))
                          (p/ #hash((sort . expr)) (var (p/ #hash((sort . char)) ⊙)))))
                        (▹ . ▹))
                  ⊙))


  ; single containment pattern tests

  (check-equal? (f/match '(0 0 ((1 7) 0 0))
                  [(c ⋱ `(1 ,a))
                   (c ⋱ `(2 ,a))])
                '(0 0 ((2 7) 0 0)))

  (check-equal? (f/match '(p/ #hash((in-scope . ()) (sort . expr) (▹ . ▹)) 0)
                  [(c ⋱ (▹ As ... / 0))
                   (c ⋱ (▹ As ... / '⊙))])
                '(p/ #hash((in-scope . ()) (sort . expr) (▹ . ▹)) ⊙))

  ; multiple containment pattern tests

  (check-equal? (f/match '(0 1 (0 1 (1 0)) 0 1)
                  [(c ⋱+ 1) (c ⋱+ '(3 4 5 6))])
                '(0 3 (0 4 (5 0)) 0 6))

  (check-equal? (f/match '(0 1 (0 1 (1 (▹ 0))) 0 1)
                  [(c ⋱+ (and a (or `(▹ ,_) (? number?))))
                   (c ⋱+ (match a [`(,x ... (▹ ,y) ,z ,w ...)
                                   `(,@x ,y (▹ ,z) ,@w)]))])
                '(0 1 (0 1 (1 0)) (▹ 0) 1))

  (check-equal? (f/match '(0 (s 1) (2 (s (▹ 3)) (4 5)) (s 6) 7)
                  [(c ⋱+ (and a `(s ,_)))
                   (c ⋱+ (match a [`(,x ... (s (▹ ,y)) (s ,z) ,w ...)
                                   `(,@x (s ,y) (s (▹ ,z)) ,@w)]))])
                '(0 (s 1) (2 (s 3) (4 5)) (s (▹ 6)) 7))

  
  
  )






(define-match-expander p⋱+
  ; containment pattern matcher
  ; returns multiple matches; template is list to splice in
  (λ (stx)
    (syntax-case stx ()
      [(p⋱+ context-id <internal-pat>)
       #'(app
          (curry multi-containment (match-lambda? <internal-pat>))
          `(,context-id (,<internal-pat> (... ...))))]))
  (λ (stx)
    (syntax-case stx ()
      [(p⋱+ context-id internal-template)
       #'(apply context-id internal-template)])))


(define-match-expander p⋱
  ; containment pattern (returns first match)
  (λ (stx)
    (syntax-case stx ()
      [(p⋱ context-id <internal-pat>)
       #'(app
          (curry multi-containment (match-lambda? <internal-pat>))
          `(,context-id (,<internal-pat>)))]))
  (λ (stx)
    (syntax-case stx ()
      [(p⋱ context-id internal-template)
       #'(context-id internal-template)])))



