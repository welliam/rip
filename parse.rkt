#lang racket

(require parser-tools/yacc "lex.rkt" racket/generator)

(define-syntax-rule (info x ...)
  (begin (printf "~s: ~s\n" 'x x) ... (newline)))

(define parse-python
  (parser
   (start <program>)
   (end eof)
   (error (lambda (tok-ok? tok-name tok-value start end)
            (error (string-append
                    (format "tok-name: ~s\n" tok-name)
                    (format "tok-value: ~s\n" tok-value)
                    (format "tok-ok?: ~s\n" tok-ok?)
                    (format "start: ~s\n" start)
                    (format "end: ~s\n" end)))))
   (tokens python-tokens python-operators python-keywords python-aux-tokens)
   (src-pos)
   (precs
    (left open-bracket close-bracket)
    (left open-parenthesis close-parenthesis)
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
    (left comma))
   (grammar

    (<program>
     (() '(program))
     ((<program-rec>) `(program . ,$1)))
    (<program-rec>
     (() '())
     ((<statement>) (list $1))
     ((newline <program-rec>) $2)
     ((<statement> newline <program-rec>) (cons $1 $3)))

    (<statement>
     ((<expression>) $1)
     ((<function>) $1)
     ((<assignment>) $1)
     ((<conditional>) $1)
     ((<while>) $1)
     ((<for>) $1)
     ((<print>) $1)
     ((<return>) $1)
     ((<import>) $1))

    (<block>
     ((indent <block-rec> unindent) (cons 'block $2)))
    (<block-rec>
     ((<statement>) (list $1))
     ((<statement> newline <block-rec>) (cons $1 $3)))

    (<assignment>
     ((symbol <assignment-operator> <expression>)
      (list $2 $1 $3))
     ((<expression> open-bracket <expression> close-bracket = <expression>)
      `(field-assign ,$1 ,$3 ,$6))
     ((<comma-separated-symbols> = <expression>)
      `(unpack ,$1 ,$3)))
    (<assignment-operator>
     ((=) '=)
     ((+=) '+=)
     ((*=) '*=))

    (<comma-separated-symbols>
     ((symbol) (list $1))
     ((symbol comma <comma-separated-symbols>) (cons $1 $3)))

    (<function>
     ((def symbol open-parenthesis <arg-list> close-parenthesis : <block>)
      `(def ,$2 ,$4 ,$7)))

    (<import>
     ((import symbol) `(import ,$2))
     ((from symbol import symbol) `(import ($2 $4))))

    (<return>
     ((return <expression>) `(return ,$2))
     ((return <expression> comma <comma-separated-values>)
      `(return (tuple ,$2 . ,$4))))

    (<print>
     ((print <expression>) `(print ,$2)))

    (<conditional>
     ((if <expression> : <block>)
      `(cond (,$2 ,$4)))
     ((if <expression> : <block> <condrec>)
      `(cond
         (,$2 ,$4) . ,$5)))
    (<condrec>
     (() (list))
     ((else : <block>)
      `((else ,$3)))
     ((elif <expression> : <block> <condrec>)
      (cons (list $2 $4) $5)))

    (<while>
     ((while <expression> : <block>)
      `(while ,$2 ,$4)))

    (<for>
     ((for symbol in <expression> : <block>)
      `(for ,$2 ,$4 ,$6)))

    (<arg-list>
     ((<arg-list-rec>) $1))
    (<arg-list-rec>
     (() (list))
     ((symbol) (list $1))
     ((* symbol) (list (list 'args $2)))
     ((** symbol) (list (list 'kwargs $2)))
     ((* symbol comma ** symbol)
      (list (list 'args $2) (list 'kwargs $5)))
     ((symbol comma <arg-list-rec>)
      (cons $1 $3)))

    (<comma-separated-values>
     ((<comma-separated-values-rec>) $1))
    (<comma-separated-values-rec>
     ((<expression>) (list $1))
     ((<expression> comma <comma-separated-values-rec>)
      (cons $1 $3)))

    (<application>
     ((<expression> open-parenthesis close-parenthesis)
      (list 'apply $1))
     ((<expression> open-parenthesis <comma-separated-values> close-parenthesis)
      (list* 'apply $1 $3)))

    (<array-slice>
     ((<expression> open-bracket : close-bracket)
      `(array-slice ,$1 #f #f))
     ((<expression> open-bracket <expression> : close-bracket)
      `(array-slice ,$1 ,$3 #f))
     ((<expression> open-bracket : <expression> close-bracket)
      `(array-slice ,$1 #f ,$4))
     ((<expression> open-bracket <expression> : <expression> close-bracket)
      `(array-slice ,$1 ,$3 ,$5)))

    (<dict-literal>
     ((open-curly <dict-rec> close-curly) (cons 'dict $2)))
    (<dict-rec>
     (() '())
     ((<expression> : <expression>) (list $1 $3))
     ((<expression> : <expression> comma <dict-rec>) (list* $1 $3 $5)))

    (<expression>
     ((number) $1)
     ((string) $1)
     ((symbol) $1)

     ((<dict-literal>) $1)
     ((<application>) $1)
     ((<array-slice>) $1)

     ((open-parenthesis <expression> close-parenthesis) $2)

     ((open-bracket close-bracket) '(list))

     ((<expression> open-bracket <expression> close-bracket)
      `(index ,$1 ,$3))

     ((<expression> if <expression> else <expression>)
      `(if ,$1 ,$3 ,$5))

     ((not <expression>) `(not ,$2))

     ((<expression> not-in <expression>)
      `(not-in ,$1 ,$3))
     ((<expression> << <expression>)
      `(<< ,$1 ,$3))
     ((<expression> dot <expression>)
      `(dot ,$1 ,$3))
     ((<expression> and <expression>)
      `(and ,$1 ,$3))
     ((<expression> != <expression>)
      `(!= ,$1 ,$3))
     ((<expression> // <expression>)
      `(// ,$1 ,$3))
     ((<expression> > <expression>)
      `(> ,$1 ,$3))
     ((<expression> / <expression>)
      `(/ ,$1 ,$3))
     ((<expression> ~ <expression>)
      `(~ ,$1 ,$3))
     ((<expression> + <expression>)
      `(+ ,$1 ,$3))
     ((<expression> is <expression>)
      `(is ,$1 ,$3))
     ((<expression> >= <expression>)
      `(>= ,$1 ,$3))
     ((<expression> >> <expression>)
      `(>> ,$1 ,$3))
     ((<expression> or <expression>)
      `(or ,$1 ,$3))
     ((<expression> * <expression>)
      `(* ,$1 ,$3))
     ((<expression> & <expression>)
      `(& ,$1 ,$3))
     ((<expression> and-not <expression>)
      `(and-not ,$1 ,$3))
     ((<expression> == <expression>)
      `(== ,$1 ,$3))
     ((<expression> ** <expression>)
      `(** ,$1 ,$3))
     ((<expression> ^ <expression>)
      `(^ ,$1 ,$3))
     ((<expression> < <expression>)
      `(< ,$1 ,$3))
     ((<expression> - <expression>)
      `(- ,$1 ,$3))
     ((<expression> vertical-bar <expression>)
      `(vertical-bar ,$1 ,$3))
     ((<expression> % <expression>)
      `(% ,$1 ,$3))
     ((<expression> <= <expression>)
      `(<= ,$1 ,$3))
     ((<expression> in <expression>)
      `(in ,$1 ,$3))
     ((<expression> is-not <expression>)
      `(is-not ,$1 ,$3))
     ((<expression> /= <expression>)
      `(/= ,$1 ,$3))))))

(define (parse-python-string s)
  (parse-python
   (sequence->generator (lex-python (open-input-string s)))))

(module+ test
  (require rackunit)

  (test-case "numbers, strings, symbols"
    (check-equal? (parse-python-string "123") '(program 123))
    (check-equal? (parse-python-string "\"hello\"") '(program "hello"))
    (check-equal? (parse-python-string "a") '(program a))
    (check-equal? (parse-python-string "___fooob1328713") '(program ___fooob1328713)))

  (test-case "operators"
    (check-equal? (parse-python-string "a + b")
                  '(program (+ a b)))
    (check-equal? (parse-python-string "a + b + c")
                  '(program (+ (+ a b) c)))
    (check-equal? (parse-python-string "a * b + c * d")
                  '(program (+ (* a b) (* c d)))))

  (test-case "assignment"
    (check-equal? (parse-python-string "a=0")
                  '(program (= a 0)))
    (check-equal? (parse-python-string "a=b+c")
                  '(program (= a (+ b c))))
    (check-equal? (parse-python-string "a*=b+c")
                  '(program (*= a (+ b c))))
    (check-equal? (parse-python-string "a+=b+c")
                  '(program (+= a (+ b c))))
    (check-equal? (parse-python-string "a, b = c")
                  '(program (unpack (a b) c))))

  (test-case "functions"
    (check-equal? (parse-python-string "def f():\n  50")
                  '(program (def f () (block 50))))
    (check-equal? (parse-python-string "def f():\n a\n b\n c")
                  '(program (def f () (block a b c))))
    (check-equal? (parse-python-string "def f(a):\n a")
                  '(program (def f (a) (block a))))
    (check-equal? (parse-python-string "def f(a, b, c):\n a\n b\n c")
                  '(program (def f (a b c) (block a b c))))
    (check-equal? (parse-python-string "def f():\n def g(a, b):\n  a\n g")
                  '(program (def f () (block (def g (a b) (block a)) g))))
    (check-equal? (parse-python-string "def f(*a):\n a")
                  '(program (def f ((args a)) (block a))))
    (check-equal? (parse-python-string "def f(a, *b):\n a")
                  '(program (def f (a (args b)) (block a))))
    (check-equal? (parse-python-string "def f(a, **b):\n a")
                  '(program (def f (a (kwargs b)) (block a))))
    (check-equal? (parse-python-string "def f(a, *b, **c):\n a")
                  '(program (def f (a (args b) (kwargs c)) (block a))))

    (check-equal? (parse-python-string "def f(a):\n return a[0]")
                  '(program (def f (a) (block (return (index a 0))))))
    (check-equal? (parse-python-string "def f(a):\n if 0:\n  1\n 2")
                  '(program
                    (def f (a)
                      (block
                       (cond (0 (block 1)))
                       2))))
    (check-equal? (parse-python-string "def f(a):\n if not foo:\n  print 'what'\n return a[0]")
                  '(program
                    (def f (a)
                      (block
                       (cond ((not foo) (block (print "what"))))
                       (return (index a 0))))))
    (check-equal? (parse-python-string "def f():\n return 1, 2")
                  '(program
                    (def f () (block (return (tuple 1 2))))))
    (check-equal? (parse-python-string "def f():\n return 1, 2, 3, 4")
                  '(program
                    (def f () (block (return (tuple 1 2 3 4)))))))


  (test-case "dicts"
    (check-equal? (parse-python-string "{}")
                  '(program (dict)))
    (check-equal? (parse-python-string "{a: 1}")
                  '(program (dict a 1)))
    (check-equal? (parse-python-string "{a: 1, b: 2}")
                  '(program (dict a 1 b 2))))

  (test-case "item assignment"
    (check-equal? (parse-python-string "foo[x] = x")
                  '(program (field-assign foo x x))))

  (test-case "print"
    (check-equal? (parse-python-string "print x")
                  '(program (print x))))

  (test-case "conditionals"
    (check-equal? (parse-python-string "if x:\n x")
                  '(program (cond (x (block x))))))

  (test-case "while"
    (check-equal? (parse-python-string "while x:\n print x")
                  '(program (while x (block (print x))))))

  (test-case "object fields"
    (check-equal? (parse-python-string "a.b")
                  '(program (dot a b)))
    (check-equal? (parse-python-string "a.b(x)")
                  '(program (apply (dot a b) x)))
    (check-equal? (parse-python-string "a.b(x, y)")
                  '(program (apply (dot a b) x y))))

  (test-case "for"
    (check-equal? (parse-python-string "for x in y:\n print x")
                  '(program (for x y (block (print x))))))


  (test-case "precedent"
    (check-equal? (parse-python-string "a.b.c")
                  '(program (dot (dot a b) c)))
    (check-equal? (parse-python-string "a.b(c)")
                  '(program (apply (dot a b) c)))
    (check-equal? (parse-python-string "a + (b < c)")
                  '(program (+ a (< b c))))
    (check-equal? (parse-python-string "(a + b) < c")
                  '(program (< (+ a b) c)))
    (check-equal? (parse-python-string "a + b < c")
                  '(program (< (+ a b) c)))))
