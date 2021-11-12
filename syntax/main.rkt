#lang racket

(require "types.rkt")
(require "syntax-define.rkt")
(require "closure.rkt")
(require "goto.rkt")
(require "build.rkt")
(define STX-FAKE-S
  (syntax-item "S'" 0 (void)))

; (define STX-E (syntax-item "E" 0 (void)))
; (define STX-T (syntax-item "T" 0 (void)))
; (define STX-F (syntax-item "F" 0 (void)))

; (define prod
;   (list
;    (production STX-FAKE-S (list STX-E))
;    (production STX-E (list STX-E stx-relop-plus STX-T))
;    (production STX-E (list STX-T))
;    (production STX-T (list STX-T stx-relop-multi STX-F))
;    (production STX-T (list STX-F))
;    (production STX-F (list stx-sep-lbracket STX-E stx-sep-rbracket))
;    (production STX-F (list stx-identifier))))
(define STX-S
  (syntax-item "S" 0 (void)))
(define STX-C
  (syntax-item "C" 0 (void)))
(define stx-c
  (syntax-item "c" 0 (lambda (ch) (eq? "c" ch))))
(define stx-d
  (syntax-item "d" 0 (lambda (ch) (eq? "d" ch))))

(define prod
  (list
   (production STX-FAKE-S (list STX-S))
   (production STX-S (list STX-C STX-C))
   (production STX-C (list stx-c STX-C))
   (production STX-C (list stx-d))))

(define aug (LRItem STX-FAKE-S (list) (list STX-S)
                    (syntax-item "EOF" 1024 (lambda (ch) (eq? ch #\nul)))))

(define initial-lritem
  (list aug))
(define cl (get-closure-function prod))

(define I0 (cl initial-lritem))

(for-each display-lritem I0)
(printf "The result of GOTO[I0, c] = \n")
(define I1 (cl (goto (cl (goto I0 STX-C)) stx-c)))
(for-each display-lritem I1)
(printf "Diff = ~a" (closure-eq? I1 I1))
(let rec ([result (build-lr1-closure-set prod aug)]
          [depth 0])
  (if (empty? result)
      (printf "DONE...\n")
      (begin
        (printf "I[~a] is:\n" depth)
        (for-each display-lritem (first result))
        (rec (rest result) (add1 depth)))))
