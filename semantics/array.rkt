#lang racket

(require racket/struct "types.rkt")

(provide (all-defined-out))

; 'append', 'count', 'extend', 'index', 'insert', 'pop', 'remove',
; 'reverse', 'sort'

(struct array (data length) #:mutable
  #:property prop:sequence
  (match-lambda
    ((array d len)
     (for/stream ((i (in-range len)) (x d)) x)))
  #:methods gen:custom-write
  ((define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'array)
      (lambda (obj)
        (match-define (array data len) obj)
        (sequence-map (lambda (i) (vector-ref data i))
                      (in-range len)))))))

(define (empty-array) (array (make-vector 3) 0))

(define (array-resize! a)
  (match-define (array data len) a)
  (define allocated (vector-length data))
  (unless (>= allocated len (arithmetic-shift allocated -1))
    (define new-data
      (make-vector (inexact->exact (floor (* len 4/3)))))
    (for ((i (in-range len)) (x data))
      (vector-set! new-data i x))
    (set-array-data! a new-data)))

(define (array-set! a i x)
  (vector-set! (array-data a) i x))

(define (array-ref a i)
  (vector-ref (array-data a) i))

(define (array-count a looking)
  (sequence-count (curry = looking) a))

(define (array-append! a x)
  (define i (array-length a))
  (set-array-length! a (+ i 1))
  (array-resize! a)
  (vector-set! (array-data a) i x))

(define (array-extend! a iter)
  (for ((x iter)) (array-append! a x)))

(define (array-index a looking)
  (let loop ((i 0))
    (when (< i (array-length a))
      (if (equal? (array-ref a i) looking)
          i
          (loop (+ i 1))))))

(define (array-insert! a i x)
  (array-append! a #f)
  (let loop ((i (min i (- (array-length a) 1))) (x x))
    (cond
      ((= i (array-length a)) python-none)
      (else
       (define old (array-ref a i))
       (array-set! a i x)
       (loop (+ i 1) old)))))

(define (array-pop! a (at (- (array-length a) 1)))
  (define value (array-ref a at))
  (for ((i (in-range at (- (array-length a) 1))))
    (array-set! a i (array-ref a (+ i 1))))
  (set-array-length! a (- (array-length a) 1))
  (array-resize! a)
  value)

(define (array-contains? array x)
  (sequence-ormap (curry equal? x) array))
