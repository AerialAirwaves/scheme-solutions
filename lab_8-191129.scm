#lang scheme

; Task 1
(define (sumMatrix m1 m2)
	(map
		(lambda (row1 row2)
			(map + row1 row2)
		)
		m1 m2
	)
)

; Task 2
(define (non-negative? matrix)
	(andmap
		(lambda (row)
			(andmap (lambda (x) (>= x 0)) row)
		)
		matrix
	)
)

; Task 3
(define (low-angle? matrix)
	(andmap
		(lambda (row i)
			(andmap
				(lambda (x j) (or (>= i j) (= x 0)) ) ; (i < j) -> (x = 0) <=> ( (not (i < j)) or (x = 0) ) <=> ((i >= j) or ( x = 0))
				row
				(build-list (length row) values)
			)
		)
		matrix
		(build-list (length matrix) values)
	)
)

; Task 4
(define (diagonal-predominance? matrix)
	(define r
		(map
			(lambda (row i)
				(let ((d (abs (list-ref row i))))
					(let ((f (- (foldl (lambda (n p) (+ p (abs n)) ) 0 row) d)))
						(cons (> d f) (>= d f))
					)
				)
			)
			matrix
			(build-list (length (car matrix)) values)
		)
	)
	(and (ormap car r) (andmap cdr r))
)

; Task 5
(define (make-build-matrix n)
	(build-list
		n
		(lambda (x)
			(append
				(reverse (build-list x (lambda (y) (- n y) ) ) )
				(build-list (- n x 1) (lambda (y) (+ y 1) ) )
			)
		)
	)
)

; Task 6
(define (zero-rows-quantity matrix)
	(foldl + 0
		(map
			(lambda (row)
				(if (andmap (lambda (x) (= x 0)) row) 1 0)
			)
			matrix
		)
	)
)