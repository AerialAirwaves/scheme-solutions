#lang scheme

; task 1
(define (mk-triples lst n)
	; step 1: divide all elements to remainder equality classes-like lists
	(define (remainder-classes-division lst eq0 eq1 eq2)
		(if (empty? lst) (list eq0 eq1 eq2)
			(let ((el (car lst)))
				(remainder-classes-division
					(cdr lst)
					(if (= 0 (remainder el 3)) (cons el eq0) eq0 )
					(if (= 1 (remainder el 3)) (cons el eq1) eq1 )
					(if (= 2 (remainder el 3)) (cons el eq2) eq2 )
				)
		))
	)
	; perform division & make result easy accessible for the rest part of function
	(define eqlsts (remainder-classes-division lst null null null))
	(define eq0 (car eqlsts))		(define eq0l (length eq0))
	(define eq1 (cadr eqlsts))		(define eq1l (length eq1))
	(define eq2 (caddr eqlsts))		(define eq2l (length eq2))

	; make combination by linear lists composition - цыганские фокусы
	(define (compose1 i res eq0 e12l)
		(if (= i n) res
			(compose1 (+ i 1)
				(cons (car eq0) res)
				(if (= (remainder i e12l) (- e12l 1)) (cdr eq0) eq0)
				e12l
			)
		)
	)
	(define (compose2 i res e1)
		(if (= i n) res
			(compose2 (+ i 1)
				(cons (car e1) res)
				(if (= (remainder i eq2l) (- eq2l 1)) (if (empty? (cdr e1)) eq1 (cdr e1)) e1)
			)
		)
	)
	(define (compose3 i res e2)
		(if (= i n) res
			(compose3 (+ i 1)
				(cons (car e2) res)
				(if (empty? (cdr e2)) eq2 (cdr e2))
			)
		)
	)

	; pre-match simple combinatory check for defined triples quantity build ability
	(if (> n (* eq0l eq1l eq2l)) #f
		(map list ; and finally, merge all built lists to triples
			(compose1 0 null eq0 (* eq1l eq2l))
			(compose2 0 null eq1)
			(compose3 0 null eq2)
			
		)
	)

)

; task 2
(define (determine-workdir-abs-path lst)

	(define (isDriveRoot? dir)
		(define (iter lst s)
			(if (empty? lst) #f
				(if (equal? #\: (car lst))
					(iter (cdr lst) #t)
					(if (equal? #\\ (car lst)) s
						(iter (cdr lst) s)
					)
				)
			)
		)
		(iter (string->list dir) #f)
	)

	(define (format-out lst)
		(define (iter lst out)
			(if (empty? (cdr lst)) (string-append (car lst) out)
				(iter (cdr lst) (string-append (car lst) "\\" out))
			)
		)
		(iter (cdr lst) (car lst))
	)
	
	(define (iter lst out rdr)
		(if (empty? lst)
			(if rdr (format-out out) #f)
			(if (isDriveRoot? (car lst))
				(iter (cdr lst) (list (car lst)) #t)
				(if rdr
					(iter
						(cdr lst)
						(if (and (not (empty? (cdr out))) (equal? (car lst) (cadr out)))
							(cdr out)
							(cons (car lst) out)
						)
						#t
					)
					(iter (cdr lst) null rdr)
				)
			)
		)
	)
	(iter lst null #f)
)

; trees funcs definition
(define (make-tree entry left right) (list entry left right))
(define (tree-entry tree) (car tree))
(define (tree-left tree) (cadr tree))
(define (tree-right tree) (caddr tree))
(define (leaf? tree) (and (empty? (tree-left tree)) (empty? (tree-right tree))) )
(define (tree->list tree)
	(define (iter tree result-list)
		(if (empty? tree) result-list
			(iter (tree-left tree)
				(cons (tree-entry tree)
				(iter (tree-right tree)
					result-list)
				)
			)
		)
	)
	(iter tree null)
)
; task 3

; unoptimal way just via fallback to lists
(define (max-count tree)
	(define (iter lst n)
		(if (empty? lst) 0 
			(if (empty? (cdr lst)) n
				(if (= (car lst) (cadr lst))
					(iter (cdr lst) (+ n 1) ) n
				)
			)
		)
	)
	(iter (sort (tree->list tree) >) 1)

)

; task 4
(define (leafs-quantity tree)
	(if (empty? tree) 0
		(if (leaf? tree) 1
			(+ (leafs-quantity (tree-right tree)) (leafs-quantity (tree-left tree)))
		)
	)
)

; task 5
(define (contains-both-childs? tree)
	(if (or (empty? tree) (leaf? tree)) #t
		(if (xor (empty? (tree-left tree)) (empty? (tree-right tree))) #f
			(and (contains-both-childs? (tree-left tree)) (contains-both-childs? (tree-right tree)))
		)
	)
)