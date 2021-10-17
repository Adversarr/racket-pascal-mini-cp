#lang racket

(require "basic-utils.rkt")
(require "matchers.rkt")
(require "token.rkt")
(require "try-get.rkt")

; (println "matching d_")
; (Number! (string->list "123d"))
; (Identifier! (string->list "d123_+"))

(define (todo!) (printf "TODO!"))

(define (get-token str)
  (let* ([buffer (string->list str)]
         [s (first buffer)]) ; s: the first char in buffer
    (cond
      ; Matching whitespace
      [(ws? s)
       (let ([matched (Whitespace! buffer)])
         (token (list->string matched) token-whitespace (void)))]
      ; Matching seperator
      [(try-sep s)
       (token (list->string (list s)) token-sep (try-sep s))]
      ; Matching numbers
      [(digit? s)
       (let ([matched (Number! buffer)])
         (if (void? matched)
             ; Look Forward.
             (let ([matched (ArrayIndex! buffer)])
               (if (void? matched)
                   (void)
                   (if (and
                        (>= (string-length str) (+ 2 (length matched)))
                        (string=? ".." (substring str (length matched) (+ 2 (length matched)))))
                       (token (list->string matched) token-number (void))
                       (void))))
             (token (list->string matched) token-number (void))
             ))]
      [(relop-letter? s)
       (let ([matched (Relop! buffer)])
         (if (void? matched)
             (void)
             (let ([content (list->string matched)])
               (token content token-relop (try-relop content)))))]
      ; Matching identifier/keyword
      [(letter_? s)
       (let ([matched (Identifier! buffer)])
         (if (void? matched)
             (void)
             (let* ([content (list->string matched)]
                    [kw (try-keyword content)])
               (if kw
                   (token content token-keyword kw)
                   (token content token-identifier content))
               )))])))

(define (lex-analyse str)
  (if (= (string-length str) 0)
      `()
      (let ([curr-token (get-token str)])
        ; (displayln curr-token)
        (if (not (token? curr-token))
            (printf "Illegal at ~s.\n" str) ; error
            (let* ([token-length (string-length (token-content curr-token))]
                   [sub-str (substring str token-length)]
                   [recur (lex-analyse sub-str)])
              (if (void? recur)
                  (void)
                  (cons curr-token recur)))))))

(define content "
function max(num1, num2: integer): integer;

var
   (* local variable declaration *)
   result: integer;

begin
   if (num1 > num2) then
      result := num1
   
   else
      result := num2;
   max := result;
end;

var
   a: array [0..3, 0 .. 3] of integer;
   i, j : integer;

begin
   for i:= 0 to 3 do
      for j:= 0 to 3 do
         a[i,j]:= i * j;
end;
")


(define (postprocess l)
  (for/list ([i l])
    (match (token-type i)
      [(== token-whitespace) (struct-copy token i [content "<whitespace>"])]
      [else i])))

(let ([result (postprocess (lex-analyse content))])
  (if (void? result)
      (printf "Error occured.\n")
      (for/list ([i result])
        (displayln i)))
  (void))