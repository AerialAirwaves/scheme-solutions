#lang scheme

; quantity of uprising digit ranges
(define (uprising q)
	(define (iter q n)
		(if (= q 0) n
			(if (< (remainder (quotient q 10) 10) (remainder q 10))
				(iter (quotient q 10) n)
				(iter (quotient q 10) (+ n 1))
			)
		)
	)
	(iter q 1)
)