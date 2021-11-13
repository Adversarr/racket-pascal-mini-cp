#lang racket

(require rebellion/type/enum)

(require rebellion/streaming/reducer)
(require "../lex/token.rkt")


; # PART 1. Syntax Item
; a syntax item is conposed of its name(or id), and its matcher
; if it is a token -> it should be a function that can match the token (if matched it will return the token)
; if it is a non-terminal symbol, it should be void.

(struct syntax-item
  (id priority matcher)
  #:transparent)

(define (non-terminal? i)
  (void? (syntax-item-matcher i)))

(define (terminal? i)
  (not (non-terminal? i)))

(struct matched-item
        (stx content))

; # PART 2. Production.
; a list of 2-3 which can be described by:
;   1. a non-terminal Syntax Item -> left side of production
;   2. a list of Syntax Item -> right side of production
;   3. ID ?

(struct production
  (left right) #:transparent)

; # PART 3. Syntax Tree Node -> (Production, List[content])

(struct syntax-tree-node
  (head children) #:transparent)



; # PART 4: LRItem.
; a LRItem can be described as a list of 4:
;   1. left side of production
;   2. Head of right side. (parsed)
;   3. Tail of right side. (to parse)
;   4. Look ahead syntax item, notice that this item should be able to parse a terminal symbol.

(struct LRItem
  (left right-head right-tail look-ahead)
  #:transparent)



(define (LRItem-equal? a b)
  (and
   (eq? (LRItem-left a) (LRItem-left b))
   (equal? (LRItem-right-head a) (LRItem-right-head b))
   (equal? (LRItem-right-tail a) (LRItem-right-tail b))
   (eq? (LRItem-look-ahead a) (LRItem-look-ahead b))))

(define (any-true? sq)
  (if (empty? sq)
      #f
      (if (first sq)
          #t
          (any-true? (rest sq)))))
(define (all-true? sq)
  (if
   (empty? sq)
   #t
   (if
    (not (first sq))
    #f
    (all-true? (rest sq)))))


(define (display-production prod)
  (printf "~a -> " (syntax-item-id (production-left prod)))
  (for-each
   (lambda (stx)
     (printf "~a " (syntax-item-id stx)))
   (production-right prod))
  (printf "\n"))

(define (display-lritem item)
  (printf "Item(~a -> " (syntax-item-id (LRItem-left item)))
  (for-each (lambda (stx) (printf "~a " (syntax-item-id stx))) (LRItem-right-head item))
  (printf "· ")
  (for-each (lambda (stx) (printf "~a " (syntax-item-id stx))) (LRItem-right-tail item))
  (printf ", ~a)\n" (syntax-item-id (LRItem-look-ahead item))))

(define (display-tree node)
  (let rec ([node node])
    (printf "(~a" (syntax-item-id (syntax-tree-node-head node)))
    (if (list? (syntax-tree-node-children node))
        (begin
          (printf "-> {~a}[" (length (syntax-tree-node-children node)))
          (for-each rec (syntax-tree-node-children node))
          (printf "]"))
        (printf "~a" (matched-item-content (syntax-tree-node-children node))))
    (printf ")")))

(define (display-tree-mma node)
  (printf "Tree[~a," (syntax-item-id (syntax-tree-node-head node)))
  ; (display-tree node)
  (if (list? (syntax-tree-node-children node))
      (begin
        (printf "{")

        (for-each (lambda (x) (display-tree-mma x) (printf ", "))
         (reverse (rest (reverse (syntax-tree-node-children node)))))
        (display-tree-mma (last (syntax-tree-node-children node)))

        (printf "}"))
      (printf "\"~a\"" (token-content (matched-item-content (syntax-tree-node-children node)))))
  (printf "]"))

(provide (all-defined-out))