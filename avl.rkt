#lang racket/base

(require "merge-sort.rkt")

(provide list->bst)
(provide bst->sorted-values)

(define (log2 x) (/ (log x) (log 2)))
(define (bound x) (inexact->exact (floor (* 1.44 (log2 x)))))

(define (data node) (car node))
(define (key node) (car (data node)))
(define (value node) (cdr (data node)))
(define (ht node) (cadr node))
(define (left node) (caddr node))
(define (right node) (cadddr node))

(define (make-node data left-node right-node)
  (list
   data
   (+ 1 (max (height left-node) (height right-node)))
   left-node
   right-node))

(define (update-node node)
  (list
   (cons (key node) (+ 1 (value node)))
   (ht node)
   (left node)
   (right node)))

(define (make-leaf k)
  (list (cons k 1) 0 '() '()))

(define (leaf? node)
  (and (null? (left node)) (null? (right node))))

(define (balance-factor node)
  (- (height (left node)) (height (right node))))

(define (balanced? node)
  (and (>= (balance-factor node) -1) (<= (balance-factor node) 1)))

(define (rotate-left node)
  (make-node
   (data (right node))
   (make-node (data node) (left node) (left (right node)))
   (right (right node))))

(define (rotate-right node)
  (make-node
   (data (left node))
   (left (left node))
   (make-node (data node) (right (left node)) (right node))))

(define (balance-node node)
  (cond ((balanced? node)
	 node)
	((> (balance-factor node) 1)
	 (cond ((> (balance-factor (left node)) 0)
		(rotate-right node))
	       (else
		(rotate-right
		 (make-node
		  (data node)
		  (rotate-left (left node))
		  (right node))))))
	(else
	 (cond ((< (balance-factor (right node)) 0)
		(rotate-left node))
	       (else
		(rotate-left
		 (make-node
		  (data node)
		  (left node)
		  (rotate-right (right node)))))))))		

(define (insert-node k node)
  (cond ((null? node)
	 (make-leaf k))
	((equal? k (key node))
	 (update-node node))
	((string<? (symbol->string k) (symbol->string (key node)))
	 (balance-node
	  (make-node
           (data node)
           (insert-node k (left node))
           (right node))))
	 ((string>? (symbol->string k) (symbol->string (key node)))
	  (balance-node
	   (make-node
	    (data node)
            (left node)
            (insert-node k (right node)))))))
  
(define (node? k node)
  (cond ((null? node)
	 '())
	((equal? k (key node))
         node)
	((string<? (symbol->string k) (symbol->string (key node)))
	 (node? k (left node)))
	((string>? (symbol->string k) (symbol->string (key node)))
	 (node? k (right node)))))

(define (key? k node)
  (let ((result (node? k node)))
    (if (null? result)
	'()
	(key result))))

(define (count-nodes node)
  (if (null? node)
      0
      (+ 1 (count-nodes (left node)) (count-nodes (right node)))))

(define (count-leaves node)
  (cond ((null? node) 0)
	((leaf? node) 1)
	(else (+ (count-leaves (left node)) (count-leaves (right node))))))

(define (height node)
  (if (null? node)
      -1
      (ht node)))

(define (min-node node)
  (if (null? (left node))
      node
      (min-node (left node))))

(define (max-node node)
  (if (null? (right node))
      node
      (max-node (right node))))

(define (min-key node)
  (key (min-node node)))

(define (max-key node)
  (key (max-node node)))

(define (parent k node)
  (cond ((or (null? node) (leaf? node) (equal? k (key node)))
	 '())
	((and (not (null? (left node))) (equal? k (key (left node))))
	 node)
	((and (not (null? (right node))) (equal? k (key (right node))))
	 node)
	((string<? (symbol->string k) (symbol->string (key node)))
	 (parent k (left node)))
	((string>? (symbol->string k) (symbol->string (key node)))
	 (parent k (right node)))))

(define (predecessor-node k bst)
  (define (aux k predecessor node)
    (cond ((null? node)
	   '())
	  ((string<? (symbol->string k) (symbol->string (key node)))
	   (aux k predecessor (left node)))
	  ((string>? (symbol->string k) (symbol->string (key node)))
	   (aux k node (right node)))
	  (else
	   (if (null? (left node))
	       predecessor
	       (max-node (left node))))))
  (aux k '() bst))

(define (successor-node k bst)
  (define (aux k successor node)
    (cond ((null? node)
	   '())
	  ((string<? (symbol->string k) (symbol->string (key node)))
	   (aux k node (left node)))
	  ((string>? (symbol->string k) (symbol->string (key node)))
	   (aux k successor (right node)))
	  (else
	   (if (null? (right node))
	       successor
	       (min-node (right node))))))
  (aux k '() bst))

(define (predecessor-key k bst)
  (let ((node (predecessor-node k bst)))
    (if (null? node)
	'()
	(key node))))

(define (successor-key k bst)
  (let ((node (successor-node k bst)))
    (if (null? node)
	'()
	(key node))))

(define (remove-node k node)
  (cond ((null? node)
	 '())
	((string<? (symbol->string k) (symbol->string (key node)))
	 (balance-node
	  (make-node
	   (key node)
	   (remove-node k (left node))
	   (right node))))
	((string>? (symbol->string k) (symbol->string (key node)))
	 (balance-node
	  (make-node
	   (key node)
	   (left node)
	   (remove-node k (right node)))))
	(else
	 (cond ((null? (left node))
		(balance-node (right node)))
	       ((null? (right node))
		(balance-node (left node)))
	       (else
		(let ((new-key (max-key (left node))))
		  (balance-node
		   (make-node
		    new-key
		    (remove-node new-key (left node))
		    (right node)))))))))

(define (defoliate node)
  (if (or (null? node) (leaf? node))
      '()
      (make-node
       (key node)
       (defoliate (left node))
       (defoliate (right node)))))
       
(define (display-node-pad-left node)
  (display " ")
  (if (null? node)
      (display '())
      (display (key node))))

(define (display-node node)
  (display (data node))
  (newline))

(define (pre-order f node)
  (cond ((null? node) 'done)
        (else
         (f node)
         (pre-order f (left node))
         (pre-order f (right node)))))

(define (in-order f node)
  (cond ((null? node) 'done)
        (else
         (in-order f (left node))
         (f node)
         (in-order f (right node)))))

(define (post-order f node)
  (cond ((null? node) 'done)
        (else
         (post-order f (left node))
         (post-order f (right node))
         (f node))))

(define (pre-order->list node)
  (if (null? node)
      '()
      (cons (data node)
            (append
             (pre-order->list (left node))
             (pre-order->list (right node))))))

(define (serialize node)
  (if (null? node)
      (list '())
      (cons (data node)
            (append
             (serialize (left node))
             (serialize (right node))))))
  
(define (list->bst s)
  (define (iter rest bst)
    (if (null? rest)
	bst
	(iter (cdr rest) (insert-node (car rest) bst))))
  (iter s '()))

(define (read-file filename)
  (call-with-input-file filename
    (lambda (infile)
      (define (iter result)
	(let ((obj (read infile)))
	  (if (eof-object? obj)
	      (reverse result)
	      (iter (cons obj result)))))
      (iter '()))))

(define (text-file->bst filename)
  (list->bst (read-file filename)))

(define (compare-keys p1 p2 fn)
  (fn (car p1) (car p2)))

(define (compare-values p1 p2 fn)
  (fn (cdr p1) (cdr p2)))

(define (bst->sorted-values bst comparator)
  (let ((s (pre-order->list bst)))
    (merge-sort s (lambda (x y) (compare-values x y comparator)))))
