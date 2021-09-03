#lang scheme

; Task 1
(define (divByOwnDigits x)
	(define (sDivByOwnDigits k)
		(if (= k 0) #t
			(if (= 0 (remainder k 10))
				#f
				(if (= 0 (remainder x (remainder k 10)))
					(sDivByOwnDigits (quotient k 10))
					#f
				)
			)
		)
	)
	(if (= x 0) #f
		(sDivByOwnDigits x)
	)
)

; Task 2
(define (binpower n)
	(define (isInteger q) (= (floor q) q))
	(define r (/ (log n) (log 2)))
	(if (isInteger r) r #f)
)


; Task 3
(define (collatzSequence n a0)
	(define (iter aK n seq)
		(if (= n 1) seq
			(let ( 
					(aO (+ (* 3 aK) 1) )  
					(aE (/ aK 2) )
				)
				(if (= 0 (remainder aK 2))
					(iter aE (- n 1) (cons aE seq))
					(iter aO (- n 1) (cons aO seq))
				)
			)
		)
	)

	(reverse ; it is more optimal to reverse list once, than always rebuild it to add something to its ending
		(iter a0 n (cons a0 null))
	)
)


; Task 4
(define (isSemiprime number)
	(define (isPrime n) ; dependency subfunc
		(define (sIsPrime q)
			(if (and (<= (* q q) n) (not (= 0 (remainder n q))) ) (sIsPrime (+ q 2))
				(> (* q q) n)
			)
		)
		(if (= 0 (remainder n 2)) (= n 2)
			(sIsPrime 3)
		)
	)
	(define (sIsSemiprime q)
		(if (= q 1) #t
			(if (= 0 (remainder number q)) (and (isPrime q) (isPrime (/ number q)))
				(sIsSemiprime (- q 1))

			)
		)
	)
	(if (isPrime number) #t
		(sIsSemiprime (- (ceiling (sqrt number)) 1))
	)
)

; Task 5
(define (digitInCell cell)
	(define (pow10 n) ; dependency subfunc
		(define (iter n q)
			(if (= n 0) q
			(iter (- n 1) (* q 10) ) )
		)
		(iter n 1)
	)
	; pow10-equal range determination iteration-recursive procedure
	(define (detRange probeLen prevTotal)
		(let ( (q (* 9 probeLen (pow10 (- probeLen 1)) ) ) )
			(if (> 0 (- cell (+ prevTotal q)))
				(cons probeLen (- cell prevTotal) )
				(detRange (+ 1 probeLen) (+ q prevTotal))
			)
		)
	)
	(define range (detRange 1 0))
	(define digQ (car range))
	(define dist (cdr range))

	; get N'th digit from number
	(if (= dist 0) 9 ; zero distance means right edge between ranges, so this is the last digit of last number of that range. definetly, this is 9
		(remainder (quotient
			(+ (pow10 (- digQ 1)) (ceiling (/ dist digQ)) -1 )
		(pow10 (- digQ
			(if (= 0 (remainder dist digQ) ) digQ (remainder dist digQ) )
		))) 10)
	)

)