#lang racket

(require "types.rkt")
(require "syntax-define.rkt")
(require "closure.rkt")
(require "goto.rkt")
(require data/queue)
(require racket/generator)

(define (look-ahead-and-reduce lritem-list sym)
  (for/list ([lri lritem-list]
             #:when(and (empty? (LRItem-right-tail lri))
                        (eq? sym (LRItem-look-ahead lri))))
    lri))

(define (try-to-shift-in lritem-list sym)
  (for/list ([lri lritem-list]
             #:when(and (not (empty? (LRItem-right-tail lri)))
                        (eq? sym (first (LRItem-right-tail lri)))))
    lri))

(define (get-all-terminals prod)
  (set->list(list->set(filter terminal? (flatten (for/list ([p prod]) (production-right p)))))))

(define (stx-can-go item-list)
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
    (for/list ([stx (stx-can-go item-list)])
      (closure (go item-list stx))))
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

(define (build-lr1-automata prod-list augmented-item eof-syntax)
  (define closure (get-closure-function prod-list))
  (define I0 (closure (list augmented-item)))
  (define term->stx-item
    (let ([all-terminals (get-all-terminals prod-list)])
      (lambda (tok)
        (let ([stxl (for/list
                        ([t all-terminals]
                         #:when((syntax-item-matcher t) tok))
                      t)])
          (if (empty? stxl)
              (if ((syntax-item-matcher eof-syntax) tok)
                  eof-syntax
                  (printf "ERROR: cannot identify this terminal.~a\n" tok))
              (first stxl))))))
  (define (generate-closure-from item-list)
    (for/list ([stx (stx-can-go item-list)])
      (closure (go item-list stx))))
  (define (find-not-in clist c)
    (for/list ([clos (generate-closure-from c)]
               #:when(not(costom-list-member? closure-eq? clist clos)))
      clos))
  (define all-closures (let ([q (make-queue)])
                         (enqueue! q I0)
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
  (define goto
    (let ([ht (make-hash)])
      (for-each (lambda (c) (hash-set! ht c (make-hash))) all-closures)
      (for*/list ([c1 all-closures]
                  [c2 all-closures])
        (for/list ([stx (stx-can-go c1)]
                   #:when(closure-eq? (closure (go c1 stx)) c2))
          (hash-set! (hash-ref ht c1) stx c2)))
      (lambda (cl stx) (hash-ref (hash-ref ht cl) stx))))

  (define (work-on-generator gen)
    (let ([ht (make-hash)]
          [sym (gen)])
      (for-each
       (begin
         (lambda (c)
           (hash-set!
            ht c
            (lambda (sym)
              (let redo ([buf sym])
                (printf "Entering state(closure):\n")
                (for-each (lambda (c) (display "\t- ") (display-lritem c)) c)
                (if (or (syntax-item? buf) (pair? buf))
                    (void)
                    (set! buf (term->stx-item buf)))
                (printf "Received a symbol [[")
                (if (pair? buf)
                    (printf "~a" (syntax-item-id (syntax-tree-node-head (car buf))))
                    (syntax-item-id buf))
                (displayln "]]\n\tCurrent Closure is \n")
                (for-each (lambda (c) (display "\t- ") (display-lritem c)) c)
                (if (pair? buf)
                    (let ([nt-tree-node (car buf)]
                          [next-symbol-in-buf (cdr buf)])
                      (printf "Here reduction complete when look-ahead is ~a\nAlso for the tree...\n\t" next-symbol-in-buf)
                      (begin (display-tree nt-tree-node) (displayln ""))
                      (printf "Current state is\n")
                      (for-each (lambda (c) (display "\t- ") (display-lritem c)) c)
                      (if (eq? (syntax-tree-node-head nt-tree-node) (first (LRItem-right-tail augmented-item)))
                          (begin
                            (printf "All done!!!\n") ; Augmented item
                            nt-tree-node)
                          (let* ([next-state (hash-ref ht (goto c (syntax-tree-node-head nt-tree-node)))]
                                 [retval (next-state next-symbol-in-buf)])
                            (let ([prod-in-use (cdr (car (car retval)))]
                                  [prod-head (car (car (car retval)))]
                                  [stack (cdr (car retval))]
                                  [next-symbol-in-buffer (cdr retval)])
                              (printf "Reducing... I am: ~a\n" nt-tree-node)
                              (for-each (lambda (c) (display "\t\t- ") (display-lritem c)) c)
                              (define stack-added (append
                                                   stack
                                                   (list nt-tree-node)))
                              (printf "\tcurrently in stack is\n\t[")
                              (for-each
                               (lambda (item)
                                 (printf "\n\t\t- ")
                                 (display-tree item))
                               (reverse stack-added))
                              (printf "\n\t]\n")
                              (if (eq? (length prod-in-use) 1)
                                  ; reduce complete!
                                  ; 当前的CLOSURE中一定有一个能够处理当前的syntax-node 对应的非终结符 （即执行了goto操作）
                                  (let ([final-tree  (syntax-tree-node prod-head (reverse stack-added))])
                                    (printf "Reduce done!!! now goto another state...(redo) Tree is\n\t")
                                    (display-tree final-tree)
                                    (printf "\n")
                                    (printf "From state:\n")
                                    (for-each (lambda (c) (display "\t- ") (display-lritem c)) c)
                                    (printf "To..")
                                    (redo (cons final-tree next-symbol-in-buffer)))
                                  ; reduce 还没有结束，继续退栈
                                  (begin
                                    (printf "\tReduce not complete : prod left is ~a\n" prod-in-use)
                                    (cons (cons (cons prod-head (rest prod-in-use))
                                                stack-added)
                                          next-symbol-in-buffer))))
                            )))
                    (let ([item-can-reduce (look-ahead-and-reduce c buf)]
                          [item-can-shiftin (try-to-shift-in c buf)])
                      ; 接收到一个终结符号，执行移进或者规约
                      (printf "\tItem->Reduce:~a\n" (length item-can-reduce))
                      (printf "\tItem->Shift: ~a\n" (length item-can-shiftin))
                      (if (empty? item-can-shiftin)
                          (if (empty? item-can-reduce)
                              (begin
                                (displayln "Error. nothing to do...")
                                (void)) ;error.
                              (if (< 1 (length item-can-reduce)); reduce to a list and do nothing.
                                  (displayln "Error: cannot decide which prod to reduce")
                                  (let ([item (first item-can-reduce)])
                                    ; return the production
                                    (printf "Reduce! (look-ahead = ~a) use item: \n\t" buf)
                                    (display-lritem item)
                                    (cons (cons (cons (LRItem-left item) (reverse (LRItem-right-head item)))
                                                (list))
                                          buf))))
                          (if (empty? item-can-reduce)
                              ; shift in the terminal and go to another state.
                              ; 执行 移进 + 转下一状态
                              (begin
                                (let* ([matched-stx buf]
                                       [next-state (hash-ref ht (goto c matched-stx))])
                                  (printf "Shift in! \n\tMatched-stx is ~a \n" matched-stx)
                                  (let([retval (next-state (gen))])
                                    (printf "Caught retval is ~a\n" retval)
                                    (let ([prod-in-use (cdr (car (car retval)))]
                                          [prod-head (car (car (car retval)))]
                                          [stack (cdr (car retval))]
                                          [next-symbol-in-buffer (cdr retval)])
                                      (printf "Reducing... I am: ~a\n" matched-stx)
                                      (for-each (lambda (c) (display "\t\t- ") (display-lritem c)) c)
                                      (define stack-added (append
                                                           stack
                                                           (list (syntax-tree-node (first prod-in-use) matched-stx))))
                                      (printf "\tcurrently in stack is\n\t[")
                                      (for-each
                                       (lambda (item)
                                         (printf "\n\t\t- ")
                                         (display-tree item))
                                       (reverse stack-added))
                                      (printf "\n\t]\n")
                                      (if (eq? (length prod-in-use) 1)
                                          ; reduce complete!
                                          ; 当前的CLOSURE中一定有一个能够处理当前的syntax-node 对应的非终结符 （即执行了goto操作）
                                          (let ([final-tree  (syntax-tree-node prod-head (reverse stack-added))])
                                            (printf "Reduce done!!! now goto another state...(redo) Tree is\n\t")
                                            (display-tree final-tree)
                                            (printf "\n")
                                            (printf "From state:\n")
                                            (for-each (lambda (c) (display "\t- ") (display-lritem c)) c)
                                            (printf "To..")
                                            (redo (cons final-tree next-symbol-in-buffer)))
                                          ; reduce 还没有结束，继续退栈
                                          (begin
                                            (printf "\tReduce not complete : prod left is ~a\n" prod-in-use)
                                            (cons (cons (cons prod-head (rest prod-in-use))
                                                        stack-added)
                                                  next-symbol-in-buffer)))))))
                              (displayln "Cannot decide which to use (RS).") ; TODO not clear -> decide on priority! or error here.
                              )))
                    )))
            )))
       all-closures)
      ((hash-ref ht I0) sym)))
  work-on-generator)

(provide build-lr1-closure-set)
(provide build-lr1-automata)