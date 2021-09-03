(define (generic-chess h)
	(build-list
		h
		(lambda (j)
			(build-list
			h
				(lambda (i) (- 1 (remainder (+ i j) 2) ) )
			)
		)
	)
)