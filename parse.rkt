#lang racket

(require parser-tools/yacc "lex.rkt")

(define parse-python
  (parser
   (start <statement>)
   (end eof)
   (error void)
   (tokens python-tokens python-operators python-keywords python-aux-tokens)
   (src-pos)
   (yacc-output "parse-python.yacc")
   (precs
    (left comma)
    (left or)
    (left and)
    (left not)
    (left in not-in is is-not < <= > >= != ==)
    (left vertical-bar)
    (left ^)
    (left &)
    (left << >>)
    (left + -)
    (left * / %)
    (left ~)
    (left **)
    (left dot)
    (left :)
    (left close-bracket)
    (left open-bracket)
    (left close-parenthesis)
    (left open-parenthesis))
   (grammar

    (<statement>
     ((<expression>) $1)
     ((symbol = <expression>)
      `(assign ,$1 ,$3)))

    (<comma-separated-values>
     ((<comma-separated-values-rec>)
      (cons 'commas $1)))
    (<comma-separated-values-rec>
     ((<expression>) (list $1))
     ((<expression> comma <comma-separated-values-rec>)
      (cons $1 $3)))

    (<block>
     ((<block-rec>) (cons 'block $1)))
    (<block-rec>
     ((<statement>) (list $1))
     ((<statement> <block-rec>) (cons $1 $2)))

    (<expression>
     ((number) $1)
     ((string) $1)
     ((symbol) $1)

     ((open-parenthesis <expression> close-parenthesis)
      $2)

     ((<expression> + <expression>)
      (list '+ $1 $3))
     ((<expression> not-in <expression>)
      (list 'not-in $1 $3))
     ((<expression> << <expression>)
      (list '<< $1 $3))
     ((<expression> dot <expression>)
      (list 'dot $1 $3))
     ((<expression> and <expression>)
      (list 'and $1 $3))
     ((<expression> != <expression>)
      (list '!= $1 $3))
     ((<expression> // <expression>)
      (list '// $1 $3))
     ((<expression> > <expression>)
      (list '> $1 $3))
     ((<expression> / <expression>)
      (list '/ $1 $3))
     ((<expression> ~ <expression>)
      (list '~ $1 $3))
     ((<expression> + <expression>)
      (list '+ $1 $3))
     ((<expression> is <expression>)
      (list 'is $1 $3))
     ((<expression> >= <expression>)
      (list '>= $1 $3))
     ((<expression> >> <expression>)
      (list '>> $1 $3))
     ((<expression> or <expression>)
      (list 'or $1 $3))
     ((<expression> * <expression>)
      (list '* $1 $3))
     ((<expression> & <expression>)
      (list '& $1 $3))
     ((<expression> and-not <expression>)
      (list 'and-not $1 $3))
     ((<expression> == <expression>)
      (list '== $1 $3))
     ((<expression> ** <expression>)
      (list '** $1 $3))
     ((<expression> ^ <expression>)
      (list '^ $1 $3))
     ((<expression> < <expression>)
      (list '< $1 $3))
     ((<expression> - <expression>)
      (list '- $1 $3))
     ((<expression> vertical-bar <expression>)
      (list 'vertical-bar $1 $3))
     ((<expression> % <expression>)
      (list '% $1 $3))
     ((<expression> <= <expression>)
      (list '<= $1 $3))
     ((<expression> in <expression>)
      (list 'in $1 $3))
     ((<expression> not <expression>)
      (list 'not $1 $3))
     ((<expression> is-not <expression>)
      (list 'is-not $1 $3))
     ((<expression> /= <expression>)
      (list '/= $1 $3))))))

(define (parse-python-string s)
  (with-input-from-string s
    (lambda ()
      (define lex (thunk (lex-python (current-input-port))))
      (parse-python lex))))

(module+ test
  (require rackunit)

  ; numbers, strings, symbols
  (check-equal? (parse-python-string "123") 123)
  (check-equal? (parse-python-string "\"hello\"") "hello")
  (check-equal? (parse-python-string "a") 'a)

  ; operators
  (check-equal? (parse-python-string "a + b") '(+ a b))
  (check-equal? (parse-python-string "a + b + c") '(+ a (+ b c)))
  (check-equal? (parse-python-string "a * b + c + d") '(+ (* a b) (+ c d)))

  ; assignment
  (check-equal? (parse-python-string "a=0") '(assign a 0))
  (check-equal? (parse-python-string "a=b+c") '(assign a (+ b c))))
