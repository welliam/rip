#lang racket

(require racket/stxparam "array.rkt")

(provide
 #%module-begin #%datum #%app #%top #%top-interaction
 and
 cond
 if
 not
 < <= > >= max min + - * /

 program
 block
 def
 += *=
 while
 return
 tuple
 unpack
 field-assign
 len
 dict
 index
 in not-in

 (rename-out
  (= ==)

  (displayln print)
  (python-apply apply)
  (python-= =)
  (python-list list)
  (python-for for)))

(define-syntax-parameter return
  (lambda (stx)
    (raise-syntax-error #f "Use of return outside of a function")))

(define-syntax-parameter assign (make-rename-transformer #'set!))

(define-syntax-rule (program forms ...)
  (begin forms ...))

(define-syntax-rule (block forms ...)
  (begin forms ...))

(define-syntax-rule (def f args body)
  (define (f . args)
    (call/cc
     (lambda (out)
       (syntax-parameterize ((return (syntax-rules () ((_ x) (out x)))))
         body)))))

(define-syntax-rule (python-= a b)
  (define a b))

(define-syntax-rule (while pred body)
  (let loop () (when pred body (loop))))

(define-syntax-rule (python-for x iter body)
  (for ((x iter)) body))

(define-syntax-rule (python-apply f args ...)
  (f args ...))

(define (python-list . xs)
  (define a (empty-array))
  (for ((x xs))
    (array-append! a x))
  a)

(define-syntax-rule (+= v x)
  (= v (+ v x)))

(define-syntax-rule (*= v x)
  (= v (+ v x)))

(define-syntax-rule (unpack (xs ...) iter)
  (begin
    (let ((var-len (length '(xs ...)))
          (iter-len (sequence-length iter)))
      (cond
        ((> iter-len var-len)
         (error "Too many values to unpack."))
        ((< iter-len var-len)
         (error (format "Need more than ~A values to unpack." iter-len)))))
    (define i 0)
    (begin (python-= xs (sequence-ref iter i))
           (set! i (+ i 1)))
    ...))

(define tuple list)

(define (field-assign of index x)
  (cond
    ((array? of) (array-set! of index x))
    ((hash? of) (hash-set! of index x))
    (else (error "?!?"))))

(define (index of i)
  (cond
    ((array? of) (array-ref of i))
    ((hash? of) (hash-ref of i))))

(define (in x of)
  (cond
    ((array? of) (array-contains? of x))
    ((hash? of) (hash-has-key? of x))))

(define not-in (negate in))

(define (len x) (array-length x))

(define (dict . xs)
  (define result (make-hash))
  (let loop ((xs xs))
    (unless (null? xs)
      (hash-set! result (car xs) (cadr xs))
      (loop (cddr xs))))
  result)
