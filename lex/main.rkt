#lang racket

(require "lexer.rkt")

(define content
  (bytes->string/utf-8 (file->bytes "1.pas")))

(let ([result (postprocess (lex-analyse content))])
  (if (void? result)
      (printf "Error occured.\n")
      (for/list ([i result])
        (displayln i)))
  (void))