#lang scheme

; Task 1
(define (divByNum lst)
	(filter
		(lambda (f) (= 0 (remainder (car f) (cdr f)) ) )
		(map 
			(lambda (f i)
				(cons f (+ i 1))
			)
			lst
			(build-list (length lst) values)
		)
	)
)

; Task 2
(define (3nfSum lst)
	(foldl
		(lambda (i p)
			(if (and (< i 1000) (> i 99))
				(+ p i) p
			)
		)
		0 lst
	)
)

; Task 3
(define (op-apply args ops)
	(foldl (lambda (n o s) ( (eval o) s n )) (car args) (cdr args) ops)
)

; Task 4
(define (sublists lst)
	(build-list (length lst) (lambda (a) (list-tail lst a)  ) )
)

; Task 5
(define (accountBalanceCalc initialBalance dailyPercentage ops)
	(define incK (+ 1 (/ dailyPercentage 100)))
	(define (** n k)
		(define (iter r k)
			(if (= k 0) r
				(iter (* r n) (- k 1))
			)
		)
		(iter 1 (if (< k 0) 0 k))
	)

	(let ((res
			(foldl (lambda (new prev)
					(if (> (car new) 30) prev
						(cons (car new) 
							( (eval (cadr new)) (* (cdr prev) (** incK (- (car new) (car prev) ))) (caddr new) )
						)
					)
				)
				(cons 1 initialBalance) (sort ops #:key car <)
			)
		))
		(* (cdr res) (** incK (- 30 (car res) )))
	)
)
