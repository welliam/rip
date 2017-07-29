#lang racket

(provide python-none)

(struct none () #:transparent)

(define python-none (none))
