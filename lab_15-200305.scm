#lang scheme

; trees essential funcs definition
(define (make-tree entry left right) (list entry left right))
(define (tree-entry tree) (car tree))
(define (tree-left tree) (cadr tree))
(define (tree-right tree) (caddr tree))
(define (leaf? tree) (and (not (empty? tree)) (empty? (tree-left tree)) (empty? (tree-right tree))) )

; task 1
(define (contains-node-div-by-child? Tree)
	(if (or (empty? Tree) (leaf? Tree)) #f
		(if (or (and (not (empty? (tree-left Tree))) (not (= 0 (tree-entry (tree-left Tree)))) (= 0 (remainder (tree-entry Tree) (tree-entry (tree-left Tree)))))
				(and (not (empty? (tree-right Tree))) (not (= 0 (tree-entry (tree-right Tree)))) (= 0 (remainder (tree-entry Tree) (tree-entry (tree-right Tree)))))
			) (tree-entry Tree)
			(or (contains-node-div-by-child? (tree-left Tree)) (contains-node-div-by-child? (tree-right Tree)))
		)
	)
)

; task 2
(define (invert-tree Tree)
	(if (or (empty? Tree) (leaf? Tree) ) Tree
		(make-tree (tree-entry Tree) (invert-tree (tree-right Tree)) (invert-tree (tree-left Tree)))
	)
)

; task 3
(define (left-childs-quantity Tree)
	(if (or (empty? Tree) (leaf? Tree)) 0
		(if (not (empty? (tree-left Tree)))
			(+ 1 (left-childs-quantity (tree-left Tree)) (left-childs-quantity (tree-right Tree)) )
			(left-childs-quantity (tree-right Tree))
		)
	)
)

; task 4
(define (min-leaf Tree)
	(if (empty? Tree) +inf.0
		(if (leaf? Tree) (tree-entry Tree)
			(min
				(min-leaf (tree-left Tree))
				(min-leaf (tree-right Tree))

			)
		)
	)
)

; task 5
(define (flatten-by-floors Tree)
	(define (reverse+filter lst out)
		(if (empty? lst) out
			(reverse+filter (cdr lst)
				(if (empty? (car lst)) out (cons (car lst) out))
			)
		)
	)
	(define (subtree lst lst-subtr lst-vals)
		(if (empty? lst) (cons (reverse+filter lst-subtr null) (reverse lst-vals) )
			(subtree (cdr lst)
				(cons (tree-right (car lst)) (cons (tree-left (car lst)) lst-subtr))
				(cons (tree-entry (car lst)) lst-vals)
			)
		)
	)
	(define (iter tlst out)
		(if (empty? tlst) (reverse out)
			(let ((sub (subtree tlst null null)))
				(iter (car sub) (cons (cdr sub) out))
			)
		)
	)
	(if (empty? Tree) #f (iter (list Tree) null))
)