#lang racket

(provide python-tokens python-operators python-keywords python-aux-tokens lex-python)

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         racket/generator)

(define-tokens python-tokens
  (leading-spaces number symbol string))

(define-empty-tokens python-aux-tokens
  (eof indent newline unindent))

(define-empty-tokens python-operators
  (+ - * / % ** // == != > < >= <= = += -= *= /= %= **= //= & vertical-bar ^ ~ <<
   >> and or not and-not or-not in not-in is is-not dot
   open-parenthesis close-parenthesis open-bracket close-bracket
   open-curly close-curly comma :))

(define-empty-tokens python-keywords
  (del from while as elif global with assert else if pass yield break
   except import print class exec raise continue finally return def for
   lambda try))

(define escape-characters
  #hash((#\n . #\newline)))

(define operators
  (make-hash
   `(("+" . ,token-+)
     ("-" . ,token--)
     ("*" . ,token-*)
     ("/" . ,token-/)
     ("%" . ,token-%)
     ("**" . ,token-**)
     ("//" . ,token-//)
     ("==" . ,token-==)
     ("!=" . ,token-!=)
     (">" . ,token->)
     ("<" . ,token-<)
     (">=" . ,token->=)
     ("<=" . ,token-<=)
     ("=" . ,token-=)
     ("+=" . ,token-+=)
     ("-=" . ,token--=)
     ("*=" . ,token-*=)
     ("/=" . ,token-/=)
     ("%=" . ,token-%=)
     ("**=" . ,token-**=)
     ("//=" . ,token-//=)
     ("&" . ,token-&)
     ("|" . ,token-vertical-bar)
     ("^" . ,token-^)
     ("~" . ,token-~)
     ("<<" . ,token-<<)
     (">>" . ,token->>)
     ("and" . ,token-and)
     ("or" . ,token-or)
     ("not" . ,token-not)
     ("and not" . ,token-and-not)
     ("or" . ,token-or)
     ("in" . ,token-in)
     ("not in" . ,token-not-in)
     ("is" . ,token-is)
     ("is not" . ,token-is-not)
     (":" . ,token-:)
     ("(" . ,token-open-parenthesis)
     (")" . ,token-close-parenthesis)
     ("[" . ,token-open-bracket)
     ("]" . ,token-close-bracket)
     ("{" . ,token-open-curly)
     ("}" . ,token-close-curly)
     ("," . ,token-comma)
     ("." . ,token-dot))))

(define keywords
  (make-hash
   `(("del" . ,token-del)
     ("from" . ,token-from)
     ("while" . ,token-while)
     ("as" . ,token-as)
     ("elif" . ,token-elif)
     ("global" . ,token-global)
     ("with" . ,token-with)
     ("assert" . ,token-assert)
     ("else" . ,token-else)
     ("if" . ,token-if)
     ("pass" . ,token-pass)
     ("yield" . ,token-yield)
     ("break" . ,token-break)
     ("except" . ,token-except)
     ("import" . ,token-import)
     ("print" . ,token-print)
     ("class" . ,token-class)
     ("exec" . ,token-exec)
     ("raise" . ,token-raise)
     ("continue" . ,token-continue)
     ("finally" . ,token-finally)
     ("return" . ,token-return)
     ("def" . ,token-def)
     ("for" . ,token-for)
     ("lambda" . ,token-lambda)
     ("try" . ,token-try))))

(define (escape-string s)
  (list->string
   (let loop ((i 0))
     (cond
       ((= i (string-length s)) '())
       ((char=? (string-ref s i) #\\)
        (define c (string-ref s (+ i 1)))
        (cons (hash-ref escape-characters c c)
              (loop (+ i 2))))
       (else (cons (string-ref s i) (loop (+ i 1))))))))

(define (escape-token-string s)
  (token-string (escape-string s)))

(define (get-token-string s delimiters-len)
  (escape-token-string
   (substring s delimiters-len (- (string-length s) delimiters-len))))

(define basic-lex-python
  (lexer-src-pos
   ((eof) (token-eof))
   ((:or "del" "from" "while" "as" "elif"
         "global" "with" "assert" "else" "if"
         "pass" "yield" "break" "except" "import"
         "print" "class" "exec" "raise" "continue"
         "finally" "return" "def" "for" "lambda"
         "try")
    ((hash-ref keywords lexeme)))
   ((:or "+" "-" "*" "/"
         "%" "**" "//" "=="
         "!=" ">" "<" ">="
         "<=" "=" "+=" "-="
         "*=" "/=" "%=" "**="
         "//=" "&" "|" "^" "~"
         "<<" ">>" "and" "or" "not"
         "and not" "or not" "in" "not in"
         "is" "is not"
         "(" ")" "[" "]" "{" "}" "," ":" ".")
    ((hash-ref operators lexeme)))

   ; whitespace
   ((:+ (:- whitespace #\newline))      ; non-semantic
    (return-without-pos (basic-lex-python input-port)))
   ((:: #\newline (:* (:- whitespace #\newline))) ; semantic
    (token-leading-spaces (- (string-length lexeme) 1)))

   ; comments
   ((:: #\# (complement (:: any-string #\newline any-string)))
    (return-without-pos (basic-lex-python input-port)))

   ; symbols
   ((:: (:or alphabetic #\_) (:* (:or alphabetic numeric #\_)))
    (token-symbol (string->symbol lexeme)))

   ; number
   ((:: (:* numeric) (:? #\.) (:+ numeric))
    (token-number (string->number lexeme)))

   ; strings
   ((:: "\"\"\"" (complement (:: any-string "\"\"\"" any-string)) "\"\"\"")
    (get-token-string lexeme 3))
   ((:: "\'\'\'" (complement (:: any-string "\'\'\'" any-string)) "\'\'\'")
    (get-token-string lexeme 3))
   ((:: #\" (complement (:: any-string (:~ #\\) #\" any-string)) #\")
    (get-token-string  lexeme 1))
   ((:: #\' (complement (:: any-string (:~ #\\) #\' any-string)) #\')
    (get-token-string  lexeme 1))))

(define (lexer->stream lexer p)
  (define t (lexer p))
  (stream-cons t (lexer->stream lexer p)))

(define (leading-spaces? t)
  (and (position-token? t)
       (equal? (token-name (position-token-token t)) 'leading-spaces)))

(define (leading-spaces-amount t)
  (token-value (position-token-token t)))

(define (from-position-token pos new-token)
  (position-token new-token
                  (position-token-start-pos pos)
                  (position-token-end-pos pos)))

(define (stream-remove-empty-newlines s)
  (for/stream ((a s)
               (b (stream-rest s))
               #:unless (and (leading-spaces? a) (leading-spaces? b)))
              a))

(define (unindent indentations to pos)
  (let loop ((indentations indentations) (result empty-stream))
    (cond
      ((= to (car indentations))
       (values indentations result))
      ((< to (car indentations))
       (loop (cdr indentations)
             (stream-cons (from-position-token pos (token-unindent))
                          result)))
      (else (error "Unindent does not match previous indentation level.")))))

(define (stream-indent s)
  (let loop ((s s) (indentations '(0)))
    (define t (stream-first s))
    (cond
      ((leading-spaces? t)
       (define amount (leading-spaces-amount t))
       (cond
         ((> amount (car indentations))
          (stream-cons (from-position-token t (token-indent))
                       (loop (stream-rest s)
                             (cons amount indentations))))
         ((= amount (car indentations))
          (stream-cons (from-position-token t (token-newline))
                       (loop (stream-rest s) indentations)))
         (else
          (define-values (is tokens) (unindent indentations amount t))
          (stream-append tokens (loop (stream-rest s) is)))))
      ((eq? (token-name (position-token-token t)) 'eof)
       (define-values (_ tokens) (unindent indentations 0 t))
       (stream-append tokens (stream t)))
      (else (stream-cons t (loop (stream-rest s) indentations))))))

(define (lex-python p)
  (stream-indent
   (stream-remove-empty-newlines
    (lexer->stream basic-lex-python p))))

(define (port->python-tokens (p (current-input-port)))
  (sequence->list (lex-python p)))

(define (string->python-tokens s)
  (with-input-from-string s port->python-tokens))

(module+ test
  (require rackunit)

  (define-syntax-rule (check-lex? s tokens ...)
    (check-equal? (map position-token-token (string->python-tokens s))
                  (append (list tokens ...) (list (token-eof)))))

  ; symbols
  (check-lex? "hello" (token-symbol 'hello))
  (check-lex? "__hello__" (token-symbol '__hello__))
  (check-lex? "_" (token-symbol '_))
  (check-lex? "foo123" (token-symbol 'foo123))
  (check-lex? "FooBar___12309123" (token-symbol 'FooBar___12309123))
  (check-lex? "define" (token-symbol 'define))

  ; keywords
  (check-lex? "def" (token-def))
  (check-lex? "break" (token-break))
  (check-lex? "class" (token-class))

  ; numbers
  (check-lex? "2.5" (token-number 2.5))
  (check-lex? ".5" (token-number .5))
  (check-lex? "50" (token-number 50))

  ; strings
  (check-lex? "\"hello\"" (token-string "hello"))
  (check-lex? "\"hello'world\"" (token-string "hello'world"))
  (check-lex? "'hello'" (token-string "hello"))
  (check-lex? "'hello\"world' 'hi'"
              (token-string "hello\"world")
              (token-string "hi"))
  (check-lex? "\"\\\"hello\\\"\"" (token-string "\"hello\""))
  (check-lex? "'\\'hello\\''" (token-string "'hello'"))

  ; sequences
  (check-lex? "1\n2"
              (token-number 1)
              (token-newline)
              (token-number 2))
  (check-lex? "1\n 2"
              (token-number 1)
              (token-indent)
              (token-number 2)
              (token-unindent))
  (check-lex? "1\n     2"
              (token-number 1)
              (token-indent)
              (token-number 2)
              (token-unindent))
  (check-lex? "1\n\n2"
              (token-number 1)
              (token-newline)
              (token-number 2))
  (check-lex? "1\n\n2\n"
              (token-number 1)
              (token-newline)
              (token-number 2)
              (token-newline))
  (check-lex? "1\n#\n2"
              (token-number 1)
              (token-newline)
              (token-number 2))
  (check-lex? "1#hello\n#hello world\n2"
              (token-number 1)
              (token-newline)
              (token-number 2))
  (check-lex? "one:\n two\nthree"
              (token-symbol 'one)
              (token-:)
              (token-indent)
              (token-symbol 'two)
              (token-unindent)
              (token-symbol 'three)))
