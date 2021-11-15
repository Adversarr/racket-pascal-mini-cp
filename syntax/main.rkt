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
(define STX-S
  (syntax-item "S" 0 (void)))

(define STX-EOF (syntax-item "EOF" 1024 (lambda (tok) (void? tok))))

(define aug
  (LRItem
   STX-FAKE-S
   (list)
   (list STX-S)
   STX-EOF))

(define file-to-analysis
  (command-line
   #:program "Lexical Analyser"
   #:args ([filename "test1.pas"])
   filename))

(define content
  (bytes->string/utf-8 (file->bytes file-to-analysis)))

(displayln content)

(define gen (lexical-generator content))

(define productions
  (list
   (production STX-FAKE-S (list STX-S))
   (production STX-S (list stx-sep-lbracket STX-S stx-sep-rbracket))
   (production STX-S (list STX-S stx-relop-plus STX-S))
   (production STX-S (list STX-S stx-relop-minus STX-S))
   (production STX-S (list STX-S stx-relop-divide STX-S))
   (production STX-S (list STX-S stx-relop-multi STX-S))
   (production STX-S (list stx-number))))

(let ([retval ((build-lr1-automata productions aug STX-EOF) gen)])
  (printf "Content of ~a is:\n~a\n################## RESULT OF LEXER ##################\n\n" file-to-analysis content)
  (display-tree-mma retval))