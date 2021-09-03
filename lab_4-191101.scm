#lang scheme

; Task 1
(define (div x k)
	(define (pow10 n)
		(define (iter p q)
			(if (= p 0) q
			(iter (- p 1) (* q 10)))
		)
		(iter n 1)
	)
	(define q (pow10 k))
	(cons (quotient x q) (remainder x q))
)

; Task 2
(define (list-even? l)
	(define (sLE l)
		(if (empty? l) #t
			(if (= 1 (remainder (car l) 2)) #f 
			(sLE (cdr l) ))
		)
	)
	(if (empty? l) #f
		(sLE l)
	)
)

; Task 3
(define (fmax l)
	(if (empty? l) #f
		(if (empty? (cdr l)) (car l)
			(if (>= (car l) (cadr l)) (car l)
				(fmax (cdr l))
			)
		)
	)
)

; Task 4
(define (trev l)
	(define (iter out tmp)
		(if (empty? tmp) out
			(iter (cons (car tmp) out) (cdr tmp))
		)
	)
	(if (or (empty? l) (empty? (cdr l))) l
		(iter l (cdr l))
	)
)


; Task 5
(define (mrest l n)
	(define (pow10 n)
		(define (iter p q)
			(if (= p 0) q
			(iter (- p 1) (* q 10)))
		)
		(iter n 1)
	)
	(define q (pow10 n))
	(define (iter l k)
		(if (empty? l) k
			(if (< (car l) q)
				(iter (cdr l) (* k (car l)))
				(iter (cdr l) k)
			)
		)
	)

	(if (or (empty? l) (= n 0)) 0
		(iter l 1)
	)
)