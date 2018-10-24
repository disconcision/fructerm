#lang racket

(require memoize)
(provide match-lambda?
         multi-split
         multi-containment)

(define-syntax-rule (match-lambda? <pat>)
  (match-lambda [<pat> #t] [_ #f]))

; splits ls into segments of lengths
(define (multi-split ls lengths)
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

(define/memo (multi-containment match? xs)
  ; this returns a list of two elements
  ; the first element is the multi-holed context as a fn
  ; the second is a list of the contents of those holes
  (cond
    [(match? xs)
     (list (λ (x) x) `(,xs))]
    [(not (list? xs))
     (list (λ () xs) `())]
    [else
     (define subpairs
       (map (curry multi-containment match?) xs))
     (define subfns
       (map first subpairs))
     (define subresults
       (apply append (map second subpairs)))
     (define subfn-arities
       (map procedure-arity subfns))
     (define (fn-candidate . args)
       (for/list ([subfn subfns]
                  [arg-list (multi-split args subfn-arities)])
         (apply subfn arg-list)))
     (list (procedure-reduce-arity fn-candidate (apply + subfn-arities))
           subresults)]))
