#lang racket

(require "types.rkt")
(require "syntax-define.rkt")
(require "closure.rkt")
(require "goto.rkt")
(require data/queue)

(define (stx-can-goto item-list)
  (set->list
   (list->set
    (for/list
        ([i item-list]
         #:when (not (empty? (LRItem-right-tail i))))
      (first (LRItem-right-tail i))))))

(define (costom-list-member? eqf lst item)
  (if (empty? lst)
      #f
      (if (eqf item (first lst))
          #t
          (costom-list-member? eqf (rest lst) item))))

(define (build-lr1-closure-set prod-list augmented)
  (define closure (get-closure-function prod-list))
  (define initial-item-list (closure (list augmented)))
  (define (generate-closure-from item-list)
    (for/list ([stx (stx-can-goto item-list)])
      (closure (goto item-list stx))))
  (define (find-not-in clist c)
    (for/list ([clos (generate-closure-from c)]
               #:when(not(costom-list-member? closure-eq? clist clos)))
      clos))
  (let ([q (make-queue)])
    (enqueue! q initial-item-list)
    (let expand-closure ([result (list)])
      (if (queue-empty? q)
          result
          (let* ([top (dequeue! q)])
            (printf "ITERATING NEXT...\n")
            (for-each display-lritem top)
            (set! result (append result (list top)))
            (for-each
             (lambda (g) (enqueue! q g))
             (find-not-in result top))
            (expand-closure result))))))

(provide build-lr1-closure-set)