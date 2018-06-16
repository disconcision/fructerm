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

- simple containment patterns


to-come:

- quote

- quasiquote/unquote

- wildcards

- complex containment patterns

- sets & hashes

|#



(provide runtime-match ; literals → pattern-templates → stx → stx
         destructure   ; stx → env
         restructure)  ; env → stx → stx

; ratch is a match-like syntactic form
(provide ratch)

#| desugar-pattern makes pattern combinators
   more explicit; in particular, it rewrites
   bare sexprs into pconses, and, if they contain
   ellipses literals, into pconses & p...es.|# 

#| i'll also take the opportunity to introduce
   some of the combinators. |#

(define (desugar-pattern stx)
  (define D desugar-pattern)
  (match stx
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
    
    [`(,(and (not 'p⋱)
             (not 'p⋱until)
             (not 'p...)
             (not 'pcons)) . ,_)
     (D (foldr
         (λ (x acc)
           (match acc
             [`(,_ ,(== '...) ,wh ...) `(p... ,x ,(first wh))]
             [_ `(pcons ,x ,acc)]))
         '() stx))]

    ; budget recursion scheme
    [(? list?) (map D stx)]
    [_ stx]))


#| similar to above. not sure if this function will
   continue to exist. unsure to this point about
   how much i want to enforce pattern/template
   symmetry. right now it's pretty similar. |#
(define (desugar-template stx)
  (define D desugar-pattern)
  (match stx
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
             [_
              `(pcons ,x ,acc)]))
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

; maybe-style bind to passthrough match failures
(define (bind x f)
  (if (equal? 'no-match x)
      'no-match
      (f x)))

; for capturing list variables along folds
(define (append-hashes h1 h2)
  (hash-union
   h1
   ; must be a better way to do below:
   (make-hash (hash-map h2 (λ (k v) (cons k (list v)))))
   #:combine (λ (a b) (append a b))))


