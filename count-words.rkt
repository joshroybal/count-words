#lang racket/base

(require "avl.rkt")

(define (read-symbols filename)
  (let ((textfile (open-input-file filename)))
    (define (iter obj seq)
      (if (eof-object? obj)
          seq
          (if (symbol? obj)
              (iter (read textfile) (cons obj seq))
              (iter (read textfile) (cons (cadr obj) seq)))))
    (iter (read textfile) '())))

(define (read-chars filename)
  (let ((textfile (open-input-file filename)))
    (define (iter ch seq)
      (if (eof-object? ch)
          (begin
            (close-input-port textfile)
            (reverse seq))
          (iter (read-char textfile) (cons ch seq))))
    (iter (read-char textfile) '())))

(define (letters->words char-seq)
  (define (iter s word result)
    (cond ((null? s)
           (reverse result))
          ((or (equal? (car s) #\') (equal? (car s) #\-))
           (iter (cdr s) (cons (car s) word) result))
          ((char-whitespace? (car s))
           (iter (cdr s) '() (cons (reverse word) result)))
          ((char-upper-case? (car s))
           (iter (cdr s) (cons (char-downcase (car s)) word) result))
          ((char-lower-case? (car s))
           (iter (cdr s) (cons (car s) word) result))
          (else
           (iter (cdr s) word result))))
  (iter char-seq '() '()))

(define (words->symbols str-seq)
  (define (iter s result)
    (cond ((null? s)
           (reverse result))
          ((null? (car s))
           (iter (cdr s) result))
          (else
           (iter
            (cdr s)
            (cons (string->symbol (list->string (car s))) result)))))
  (iter (letters->words str-seq) '()))

;;; command line input necessary
(define args (current-command-line-arguments))
(cond ((< (vector-length args) 1)
       (display "Usage: count-words textfile")
       (newline)
       (exit 1)))
(define filename (vector-ref args 0))

(define bst
  (list->bst
    (words->symbols (read-chars filename))))

(let ((out (open-output-file "report.txt"
                             #:mode 'text
                             #:exists 'replace)))
  (for-each
   (lambda (x)
     (display (car x) out)
     (display ": " out)
     (display (cdr x) out)
     (newline out))
   (bst->sorted-values bst >))
  (close-output-port out))

(display " word frequency report written to file report.txt")
(newline)