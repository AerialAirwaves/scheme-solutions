#lang scheme
; Lab 191018

; Task 1
(define (minDigit num)
	(define (sMinDigit num mn)
		(cond
			((= num 0) mn)
			((< (remainder num 10) mn) (sMinDigit (quotient num 10) (remainder num 10) ) )
			(else (sMinDigit (quotient num 10) mn ))
		)
	)
	(sMinDigit num 10)
)

; Task 2
(define (sameDigits num)
	(define (sSameDigits num bool)
		(if (< num 10) bool
			(sSameDigits
				(quotient num 100)
				(and bool
					(= (remainder (floor (/ num 10)) 10) (remainder num 10))
				)
			)
		)
	)
	(sSameDigits num #t)
)

; Task 3

(define (isFact num)
	(define (sIsFact num cN cV)
		(if (>= cV num) (= cV num)
			(sIsFact num (+ 1 cN) (* cV cN) )
		)
	)
	(sIsFact num 1 1)
)

; Task 4
(define (cfib num)
	(define (fib n)
		(define (it a b k)
			(if (= k 0) b (it b (+ a b) (- k 1))))
			(it 1 1 (- n 1)))
	(define (scfib num cn)
		(if (>= num (fib cn)) (scfib num (+ cn 1))
			(if (>= (- (fib cn) num ) (/ (fib (- cn 2)) 2))
				(fib (- cn 1))
				(fib cn)
			)
		)
	)
	(scfib num 1)
)

; Task 5
(define (isPerfect n)
	(define (sIsPerfect c s)
		(if (= c 0) (= s n)
			(if (= 0 (remainder n c))
				(sIsPerfect (- c 1) (+ s c))
				(sIsPerfect (- c 1) s)
			)
		)
	)
	(sIsPerfect (- n 1) 0)
)