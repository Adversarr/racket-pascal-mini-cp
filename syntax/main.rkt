#lang racket

(require "types.rkt")
(require "syntax-define.rkt")
(require "closure.rkt")
(require "goto.rkt")
(require "build.rkt")
(require "../lex/lexer.rkt")
(require racket/generator)
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
  (syntax-item "c" 0 (lambda (ch) (eq? #\c ch))))
(define stx-d
  (syntax-item "d" 0 (lambda (ch) (eq? #\d ch))))

(define prod
  (list
   (production STX-FAKE-S (list STX-S))
   (production STX-S (list STX-C STX-C))
   (production STX-C (list stx-c STX-C))
   (production STX-C (list stx-d))))

(define STX-EOF (syntax-item "EOF" 1024 (lambda (tok) (void? tok))))

(define aug (LRItem
             STX-FAKE-S
             (list)
             (list STX-S)
             STX-EOF))

(define initial-lritem
  (list aug))

(define cl (get-closure-function prod))

(define I0 (cl initial-lritem))

(for-each display-lritem I0)
(printf "The result of GOTO[I0, c] = \n")
(let rec ([result (build-lr1-closure-set prod aug)]
          [depth 0])
  (if (empty? result)
      (printf "DONE...\n")
      (begin
        (printf "I[~a] is:\n" depth)
        (for-each display-lritem (first result))
        (rec (rest result) (add1 depth)))))

(define g (generator ()
              (let loop ([x (string->list "ccdd")])
                (if (null? x)
                    (void)
                    (begin
                      (displayln "GENERATOR:: gen!")
                      (yield (first x))
                      (loop (rest x)))))))



(define file-to-analysis
  (command-line
   #:program "Lexical Analyser"
   #:args (filename)
   filename))


(define content
  (bytes->string/utf-8 (file->bytes file-to-analysis)))

(printf "Content of ~a is:\n~a\n################## RESULT OF LEXER ##################\n\n" file-to-analysis content)

(define gen (lexical-generator content))

(define productions
  (list
   (production STX-FAKE-S (list STX-S))
   (production STX-S (list STX-S stx-relop-plus STX-S))
   (production STX-S (list STX-S stx-relop-multi STX-S))
   (production STX-S (list stx-number))))

(let ([retval ((build-lr1-automata productions aug STX-EOF) gen)])
  (display-tree-mma retval))