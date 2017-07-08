#lang racket

(require parser-tools/lex (prefix-in : parser-tools/lex-sre))

(define-tokens python-tokens
  (leading-spaces
   number
   symbol
   string
   keyword
   operator))

(define lex-python
  (lexer
   ((:or "del" "from" "while" "as" "elif"
         "global" "with" "assert" "else" "if"
         "pass" "yield" "break" "except" "import"
         "print" "class" "exec" "raise" "continue"
         "finally" "return" "def" "for" "lambda"
         "try")
    (token-keyword (string->symbol lexeme)))
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
    (token-operator lexeme))

   ; whitespace
   ;; ((:: #\newline
   ;;      (complement (:: whitespace #\newline whitespace))
   ;;      #\newline)
   ;;  (lex-python input-port))
   ((:+ (:- whitespace #\newline))
    (lex-python input-port))
   ((:: #\newline (:* (:- whitespace #\newline)))
    (token-leading-spaces (- (string-length lexeme) 1)))

   ((:: #\# (complement (:: any-string #\newline any-string)))
    (lex-python input-port))
   ((:: (:or alphabetic #\_) (:* (:or alphabetic numeric #\_)))
    (token-symbol (string->symbol lexeme)))
   ((:: (:* numeric) (:? #\.) (:+ numeric))
    (token-number (string->number lexeme)))

   ; strings
   ((:: "\"\"\"" (complement (:: any-string "\"\"\"" any-string)) "\"\"\"")
    (token-string (substring lexeme 3 (- (string-length lexeme) 3))))
   ((:: #\" (complement (:: any-string "\"" any-string)) #\")
    (token-string (substring lexeme 1 (- (string-length lexeme) 1))))
   ((:: #\' (complement (:: any-string "\'" any-string)) #\')
    (token-string (substring lexeme 1 (- (string-length lexeme) 1))))))

(define (port->python-tokens (p (current-input-port)))
  (define token (lex-python p))
  (if (eq? token 'eof)
      '()
      (cons token (port->python-tokens p))))

(define (string->python-tokens s) (with-input-from-string s port->python-tokens))

(define (display-tokens (p (current-input-port)))
  (define token (lex-python p))
  (unless (eq? token 'eof)
    (print token)
    (newline)
    (display-tokens p)))

(module+ test
  (require rackunit)

  ; symbols
  (check-equal? (string->python-tokens "hello")
                (list (token-symbol 'hello)))
  (check-equal? (string->python-tokens "__hello__")
                (list (token-symbol '__hello__)))
  (check-equal? (string->python-tokens "_")
                (list (token-symbol '_)))
  (check-equal? (string->python-tokens "foo123")
                (list (token-symbol 'foo123)))
  (check-equal? (string->python-tokens "FooBar___12309123")
                (list (token-symbol 'FooBar___12309123)))
  (check-equal? (string->python-tokens "define")
                (list (token-symbol 'define)))

  ; keywords
  (check-equal? (string->python-tokens "def")
                (list (token-keyword 'def)))
  (check-equal? (string->python-tokens "break")
                (list (token-keyword 'break)))
  (check-equal? (string->python-tokens "class")
                (list (token-keyword 'class)))

  ; numbers
  (check-equal? (string->python-tokens "2.5")
                (list (token-number 2.5)))
  (check-equal? (string->python-tokens ".5")
                (list (token-number .5)))
  (check-equal? (string->python-tokens "50")
                (list (token-number 50)))

  ; sequences
  (check-equal? (string->python-tokens "1\n2")
                (list (token-number 1)
                      (token-leading-spaces 0)
                      (token-number 2)))
  (check-equal? (string->python-tokens "1\n 2")
                (list (token-number 1)
                      (token-leading-spaces 1)
                      (token-number 2)))
  (check-equal? (string->python-tokens "1\n     2")
                (list (token-number 1)
                      (token-leading-spaces 5)
                      (token-number 2)))
  (check-equal? (string->python-tokens "1#hello\n#hello world\n2")
                (list (token-number 1)
                      (token-leading-spaces 0)
                      (token-leading-spaces 0)
                      (token-number 2)))
  )
