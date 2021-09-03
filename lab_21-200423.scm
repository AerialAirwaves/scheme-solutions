#lang scheme

; task 1
(define (TSort graphAdjacencyLists)
	(define (iter2 q lst)
		(define (iter lst c)
			(if (= (car lst) q) c
				(iter (cdr lst) (+ c 1))
			)
		)
		(iter lst 0)
	)
	(define (iter T1 T2 vects_left g out)
	    (if (empty? vects_left) out
			(if (empty? (cdr T2))
				(iter
					(car g) (car g) (remove (car T2) vects_left) (remove T1 g)
					(if (member (car T2) out) out (cons (car T2) out))
				)
				(if (member (cadr T2) vects_left)
					(iter (list-ref g (iter2 (cadr T2) vects_left)) (list-ref g (iter2 (cadr T2) vects_left)) vects_left g out)
					(iter T1 (cons (car T2) (cddr T2)) vects_left g out)
				)

			)
		)
	)
	(iter
		(car graphAdjacencyLists)
		(car graphAdjacencyLists)
		(map (lambda(x) (car x)) graphAdjacencyLists) 
		graphAdjacencyLists null
	)
)

; task 2
(define (remove-vertex graphEdgesList vertexes)
	(define (q v)
		(define (iter n lst)
			(if (empty? lst) n
				(iter (- n (if (> v (car lst)) 1 0)) (cdr lst))
			)
		)
		(iter v vertexes)
	)
	(foldl
		(lambda (n p)
			(if (or (member (car n) vertexes) (member (cdr n) vertexes)) p
				(cons
					(cons
						(q (car n))
						(q (cdr n))
					) p
				)
			)
		)
		null
		graphEdgesList
	)
)

; task 3
(define (inverse graphAdjacencyLists)
	(define verts (foldl (lambda (n p) (cons (car n) p)) null graphAdjacencyLists))
	(map
		(lambda (el)
			(cons
				(car el)
				(foldl (lambda (n p) (if (or (equal? n (car el)) (member n (cdr el))) p (cons n p) )) null verts)
			)
		)
		graphAdjacencyLists
	)
)

; task 4
(define (graph-split graphAdjacencyLists)
	(define (DFS a b g) ; any route finder DFS that taken from lectures
		(define G (map (lambda(x) (cdr x)) g))
		(define (iter prosm stack)
			(if (> (list-ref prosm a) 0) stack
				(if (empty? stack) #f
					(let* (
						(pos (car stack))
						(next
							(foldl
								(lambda (x y)
									(if (equal? y #f)
										(if (= 0 (list-ref prosm x)) x #f) y
									)
								)
							#f
							(list-ref G pos)
							)
						)
						(step (list-ref prosm pos))
					)
						(if (equal? next #f)
							(iter prosm (cdr stack))
							(iter
								(append (take prosm next) (cons (+ step 1) (drop prosm (+ next 1))))
								(cons next stack)
							)
						)
					)
				)
			)
		)
		(iter (build-list (length G) (lambda (i) (if (= i b) 1 0))) (list b))
	)

	(define (split node G) (map (lambda(x) (if (DFS x node G) #t #f)) (build-list (length G) values) ))

	(define (tl lst)
		(define (iter lst cnt)
			(if (empty? lst) cnt
				(if (car lst)
					(iter (cdr lst) (add1 cnt))
					(iter (cdr lst) cnt)
				)
			)
		)
		(iter lst 0)
   )

	(define (iter lst out)
		(define cm
			(if (empty? lst) lst
				(filter
					(lambda (x) (not (empty? x) ) )
					(map (lambda(x y) (if (equal? y #t) (car x) null)) graphAdjacencyLists (car lst) )
				)
			)
		)
		(if (empty? lst) out
			(iter (cdr lst)
				(if (member cm out) out (cons cm out))
			)
		)
	)

	(iter
		(sort
			(build-list (length graphAdjacencyLists) (lambda(x) (split x graphAdjacencyLists)) )
			(lambda (x y) (> (tl x) (tl y)))
		) null
	)
)

; task 5
(define (graph-center graphAdjacencyLists)
	(define (vertex-excentricity graphAdjacencyLists vertex)
		; based on lectures DFS implementation and weightless (each edge has weight equal to 1) edges Dijkstra's algo modification
		(define G (make-vector (length graphAdjacencyLists) null))
		(map (lambda (x) (vector-set! G (car x) (cdr x))) graphAdjacencyLists)
		(define distances (make-vector (length graphAdjacencyLists) +inf.0))
		(vector-set! distances vertex 0)

		(define (iter non_visited_vertexes stack add)
			(if (and (empty? non_visited_vertexes) (empty? stack))
				
				(apply max (vector->list distances))

				(if (empty? stack)
					(let ((new_beginning
						(foldl (lambda (n p) (if (and (not (= +inf.0 (vector-ref distances n))) (< (vector-ref distances n) (vector-ref distances p))) n p))
							(car non_visited_vertexes)
							(cdr non_visited_vertexes)
						)
						))
						(iter
							(remove new_beginning non_visited_vertexes)
							(list new_beginning)
							(vector-ref distances new_beginning)
						)
					)
					(let* (
						(pos (car stack))
						(next
							(foldl
								(lambda (x y)
									(if (equal? y #f)
										(if (> (vector-ref distances x) (+ add (length stack))) x #f)
										y
									)
								)
								#f
								(vector-ref G pos)
							)							
						)
						(step (vector-ref distances pos))
					)
						(if (equal? next #f)
							(iter non_visited_vertexes (cdr stack) add)
							(begin
								(let ((neighbor-dist (length stack)))
									(cond [(< neighbor-dist (vector-ref distances next)) (vector-set! distances next neighbor-dist)])
								)
								(iter non_visited_vertexes (cons next stack) add)
							)
						)
					)
				)
			)
		)
		(iter
			(foldl (lambda (n p) (if (equal? vertex (car n)) p (cons (car n) p))) null graphAdjacencyLists)
			(list vertex) 0
		)
	)

	(define excentricities ; compose excentricities list of pairs ({vertex_name} . {excentricity})
		(foldl
			(lambda (n p)
				(cons
					(cons (car n)
						(vertex-excentricity graphAdjacencyLists (car n) )
					)
					p
				)
			) null graphAdjacencyLists
		)
	)

	(define graph-rad (foldl (lambda (n p) (if (< (cdr n) p) (cdr n) p)) +inf.0 excentricities)) ; obvious

	(foldl (lambda (n p) (if (= (cdr n) graph-rad) (cons (car n) p) p)) null excentricities)
)