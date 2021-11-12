#lang racket

(require "types.rkt")
(require "syntax-define.rkt")
(require "closure.rkt")

(define (get-goto closuref)
  (lambda (item-list a)
    (closuref
     (flatten
      (for/list ([item item-list])
        (if (empty? (LRItem-right-tail item))
            (list)
            (if (eq? a (first (LRItem-right-tail item)))
                (LRItem
                 (LRItem-left item)
                 (append (LRItem-right-head item) (list (first (LRItem-right-tail item))))
                 (rest (LRItem-right-tail item))
                 (LRItem-look-ahead item))
                (list))))))))

(define (closure-eq? a b)
  (all-true?
   (for/list ([ia a])
     (any-true?
      (for/list ([ib b])
        (LRItem-equal? ia ib)))
     )))

; (define (build-all-closures cl cf goto)
;   (define (rec closures)
;     (let ([addons (list)])
;       )))

(provide (all-defined-out))
