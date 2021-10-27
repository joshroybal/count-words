#lang racket/base

(provide merge-sort)

(define (extract-runs ls fn)
  (define (iter in run out)
    (cond ((null? (cdr in))
           (if (null? run)
             (cons (reverse (list (car in))) out)
             (cons (reverse (cons (car in) run)) out)))
          ((fn (car in) (cadr in))
           (iter (cdr in) (cons (car in) run) out))
          (#t
           (iter
            (cdr in)
            '()
            (cons (reverse (cons (car in) run)) out)))))
  (iter ls '() '()))

(define (merge-runs x y fn)
  (define (iter x y z)
    (cond ((and (null? x) (null? y))
           (reverse z))
          ((null? x)
           (iter x (cdr y) (cons (car y) z)))
          ((null? y)
           (iter (cdr x) y (cons (car x) z)))
          ((fn (car x) (car y))
           (iter (cdr x) y (cons (car x) z)))
          (#t
           (iter x (cdr y) (cons (car y) z)))))
  (iter x y '()))

(define (merge-sort ls fn)
  (define (aux in out)
    (cond ((and (null? in) (equal? (length out) 1))
           (car out))
          ((and (equal? (length in) 1) (null? out))
           (car in))
          ((null? in)
           (aux out '()))
          ((null? (cdr in))
           (aux (cons (merge-runs (car in) (car out) fn) (cdr out)) '()))
          (#t
           (aux (cddr in) (cons (merge-runs (car in) (cadr in) fn) out)))))
  (aux (extract-runs ls fn) '()))

(define (sorted? ls fn)
  (define (iter ls fn)
    (cond ((null? (cdr ls)) #t)
          ((not (fn (car ls) (cadr ls))) #f)
          (#t (iter (cdr ls) fn))))
  (iter ls fn))

(define (compare-pair p1 p2 fn)
  (fn (car p1) (car p2)))
