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
    0
    ))

; patterns i want to add;
; ...-style splicing unquote
; containment-singleton
; containment-list
; containment-tree
; locality anchor

#;(module+ test
    (require rackunit)
    (check-equal? ((fructerm '(`a 4) ) 'a)
                  4)
    (check-equal?))

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