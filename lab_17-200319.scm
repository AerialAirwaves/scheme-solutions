#lang scheme

; trees essential funcs definition
(define (make-tree entry left right) (list entry left right))
(define (node-entry tree) (car tree))
(define (tree-left tree) (cadr tree))
(define (tree-right tree) (caddr tree))
(define (leaf? tree) (and (not (empty? tree)) (empty? (tree-left tree)) (empty? (tree-right tree))) )

; task 1
(define (map-tree func . trees)
; Вот так, с помощью нехитрых приспособлений, буханку белого (или черного)
; ХЛЕБА можно превратить в троллейбус... НО ЗАЧЕМ?
	(if (empty? (car trees)) null
		(make-tree
			; evaluate node entry via given func
			(apply func (foldr (lambda (x out) (cons (car x) out)) null trees))
			; build left subtree
			(apply map-tree func (foldr (lambda (x res) (cons (tree-left x) res))  null trees))
			; build right subtree
			(apply map-tree func (foldr (lambda (x res) (cons (tree-right x) res)) null trees))
		)
	)
)

; task 2
(define (foldl-tree func init-val . trees)
; а можно ненадо? :(
	(cond
		[  (leaf? (car trees)) ; if we'd reached leaves, process its value via given func & initial value
			(apply func (cons init-val (foldr (lambda (x out) (cons (car x) out)) null trees ) ) )
		]

		[  (and (not (empty? (tree-left (car trees))))  (empty? (tree-right (car trees))) ) ; if non-empty child is only left one, process it
			(apply foldl-tree func
				(apply func (cons init-val (foldr (lambda (x out) (cons (car x) out)) null trees)))
				(foldr (lambda (x out) (cons (tree-left x) out)) null trees)
			)
		]

		[  (and (not (empty? (tree-right (car trees))))  (empty? (tree-left (car trees))) ) ; if non-empty child is only right one, process it
			(apply foldl-tree func
				(apply func (cons init-val (foldr (lambda (x out) (cons (car x) out)) null trees)))
				(foldr (lambda (x out) (cons (tree-right x) out)) null trees)
			)
		]

		[else ; otherwise, process the results of both childs recursive func application via given func
			(func
				(apply foldl-tree func
					(apply func (cons init-val (foldr (lambda (x out) (cons (car x) out)) null trees)))
					(foldr (lambda (x out) (cons (tree-left x) out)) null trees)
				)
				(apply foldl-tree func
					(apply func (cons init-val (foldr (lambda (x out) (cons (car x) out)) null trees)))
					(foldr (lambda (x out) (cons (tree-right x) out)) null trees)
				)
			)
		]
	)
)


; task 3
(define (cut-transient-nodes Tree)
	(define (recur Tree)
		(cond
			[ (leaf? Tree)	Tree ] ; bypass leaves
			[ (not (or (empty? (tree-left Tree)) (empty? (tree-right Tree)))) ; if both childs aren't leaves, wrap both subtrees via recursion
				(make-tree (node-entry Tree) (recur (tree-left Tree)) (recur (tree-right Tree)) ) ]
			[ (empty? (tree-left Tree)) (recur (tree-right Tree)) ] ; if one child is null, jump directly to another
			[ else (recur (tree-left Tree)) ] ; if not, jump to child, which is not null
		)
	)
	(if (empty? Tree) null
		(recur Tree)
	)
)

; task 4
(define (mk-tree-by-edges edges)

	(define (root edges)
		(car (ormap (lambda (x) (if (ormap (lambda (y) (equal? (car x) (cdr y))) (remove x edges)) #f x)) edges))
	)

	(define (mk-branches edges)
		(define (iter lst1 lst2)
			(if (empty? lst1) lst2
				(let ((f (findf (lambda (x) (equal? (caar lst1) (car x))) (remove (car lst1) edges))))
					(iter (remove f (cdr lst1)) (cons (list (caar lst1) (cdar lst1) (if f (cdr f) null)) lst2))
				)
			)
		)
		(iter edges null)
	)

	(define branches (mk-branches edges))

	(define (recur tree)
		(if (leaf? tree) tree
			(make-tree
				(node-entry tree)
				(recur
					(or (findf (lambda (x) (equal? (car x) (tree-left tree))) branches)  (list (tree-left tree)  null null))
				)
				(recur
					(or (findf (lambda (x) (equal? (car x) (tree-right tree))) branches) (list (tree-right tree) null null))
				)
			)
		)
	)
	(recur (findf (lambda (x) (equal? (car x) (root edges))) branches) )
)