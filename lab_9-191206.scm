#lang scheme

; Task 1
(define (max-zeros-row matrix)
	(cdr
		(foldl 
			(lambda (row p)
				(let ((zeros (foldl (lambda (n p) (if (= n 0) (+ p 1) p) ) 0 row)))
					(if (> zeros (car p)) (cons zeros row) p )
				)
			)
		'(0 empty) matrix)
	)
)

; Task 2
(define (make-build-matrix n)
	(build-list n
		(lambda (j)
			(build-list n
				(lambda (i)
					(let ((k (if (> (+ j 1) (+ (quotient n 2) (remainder n 2) ) ) (- n j) (+ j 1) ) ))
						(if (or (< i k) (>= i (- n k) ) ) 1 0 )
					)
				)
			)
		)
	)
)

; Task 3
(define (settle-points mx)
	(define cols (apply map (cons (lambda (fel . oels) (cons fel oels)) mx ) ) )
	(define out
		(filter values
			(apply append
				(build-list (length cols)
					(lambda (j)
						(build-list (length mx)
							(lambda (i)
								(let ((crow (list-ref mx i) ))
								(let ((row-min (foldl (lambda (n p) (if (< n p) n p)) +inf.0 crow ) ) )
									(if (equal? (foldl (lambda (n p) (if (> n p) n p)) -inf.0 (list-ref cols j))
										row-min )
										(if (equal? row-min (list-ref crow j)) row-min #f ) #f
							)))))))))
	)
	(if (empty? out) #f out)
)

; Task 4
(define (magic-quad? matrix)
	; will return #f if it is not a magic quad,
	; otherwise, because everything, that not lie, is truth, func will return magic quad common sum
	(foldl
		(lambda (n p) (if (and p (= n p) ) n #f ) )
		(foldl
			(lambda (n p) (if (and p (= n p) ) n #f ) )
			(foldl ; primary diag sum
				(lambda (n i p) (+ p (list-ref n i)) )
				0
				matrix
				(build-list (length matrix) values)
			)
			(cons
				(let ((lm (length  matrix))) ; secondary diag sum
					(foldl
						(lambda (n i p) (+ p (list-ref n (- lm i 1))) )
						0
						matrix
						(build-list lm values)
					)
				)
				(apply map ; columns sums
					(cons
						(lambda (fel . oels)
							(apply + (cons fel oels))
						)
						matrix
					)
				)
			)
		)
		(map (lambda (x) (apply + x)) matrix) ; and, finally, rows sums
	)
)

; Task 5
(define (row=col? matrix)
	(foldl
		(lambda (n p) (or n p) )
		#f
		(apply map
			(cons
				(lambda (farg . args)
					(let ((col (cons farg args)))
						(if (member col matrix) col #f)
					)
				)
				matrix
			)
		)
	)
)