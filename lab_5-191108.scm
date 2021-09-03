#lang scheme
; Task 1
(define (primeDivs num)
	; subfuncs definition

	; subfunc: its destination is obvious
	(define (isPrime n) 
		(define (sIsPrime q)
			(if (and (<= (* q q) n) (not (= 0 (remainder n q))) ) (sIsPrime (+ q 2))
				(> (* q q) n)
			)
		)
		(if (= 0 (remainder n 2)) (= n 2)
			(sIsPrime 3)
		)
	)

	; iter subfunc: determine divisor's prime num power
	(define (iter2 n i t1 t2)
		(if (= 0 (remainder n i))
			(iter2 (/ n i) i (+ t1 1) (* t2 i))
			(cons t1 t2)
		)
	)

	; primary iter subfunc
	(define (iter1 n i d)
		(if (= n 1) (reverse d)
			(if (= 0 (remainder n i))
				(if (isPrime i)
					(let ((q (iter2 n i 0 1)))
						(iter1 (/ n (cdr q)) (+ i 1) (cons (cons i (car q)) d) )
					)
					(iter1 n (+ i 1) d)
				)
				(iter1 n (+ i 1) d)
			)
		)
	)

	; match
	(iter1 num 2 null)
)





; Task 2
(define (minACE lst)
	; subfuncs definition & usefull load computation

	; iter subfunc: max list value
	(define (max-lst lst)
		(define (iter lst m)
			(if (empty? lst) m
				(iter (cdr lst)
					(if (> (car lst) m) (car lst) m)
				)
			)
		)
		(iter lst -inf.0)
	)

	; iter subfunc: calculate list avg
	(define (avgg lst s k)
		(if (empty? lst) (if (= k 0) -1 (/ s k))
			(avgg (cdr lst) (+ s (car lst)) (+ k 1) )
		)
	)
	(define avg (avgg lst 0 0)) ; then, save computation result into constant

	; iter subfunc: find list element, complying given task criterias
	(define (acee lst cand)
		(if (empty? lst) cand
			(acee (cdr lst)
				(let ((el (car lst)))
					(if (and
							(<= ; closest to list elements avg
								(abs (- avg el)) 
								(abs (- avg cand))
							)
							(<= el avg) ; minimal: in our conditions, lower or equal to list elements avg
						)
						el
						cand
					)
				)
			)
		)
	)
	(define ace (acee lst (max-lst lst))) ; then, save computation result into constant

	; subfunc: its destination is obvious
	(define (ocurrence-indexes lst il i)
		(if (empty? lst) (reverse il)
			(ocurrence-indexes (cdr lst)
				(if (= ace (car lst))
					(cons i il)
					il
				)
			(+ i 1))
		)
	)

	; match
	(if (empty? lst) null
		(if (empty? (cdr lst)) (cons (car lst) 0)
			(cons ace (ocurrence-indexes lst null 0))
		)
	)
)





; Task 3
(define (mxSpL lst) ; maximum same parity sequence length
	; subfuncs definition

	(define (iter lst c m) ; primary iter subfunc
		(if (empty? (cdr lst)) m
			(if (= (remainder (car lst) 2) (remainder (cadr lst) 2))
				(iter (cdr lst) (+ c 1)
					(if (> (+ c 1) m ) (+ c 1) m)
				)
				(iter (cdr lst) 1 m)
			)
		)
	)

	; match
	(if (empty? (cdr lst))
		1
		(iter lst 1 1)
	)
)





; Task 4
(define (divByNum lst)
	; subfuncs definition

	(define (iter src dest n) ; primary iter subfunc
		(if (empty? src) (reverse dest)
			(iter (cdr src)
			(if (= 0 (remainder (car src) n))
				(cons (car src) dest)
				dest
			)
			(+ n 1))
		)
	)

	; match
	(if (or (empty? lst) (empty? (cdr lst)) ) null
		(iter (cdr lst) null 1)
	)
	
)





; Task 5
(define (list-mirror? lA lB )
	; subfuncs definition

	; subfunc: its destination is obvious
	(define (sign k)
		(cond
			((> k 0)  1)
			((< k 0) -1)
			((= k 0)  0)
		)
	)

	; subfunc: reverse integer
	(define (revInt k)
		(define (iter k m)
			(if (> k 0)
				(iter (quotient k 10) (+ (* 10 m) (remainder k 10)))
				m
			)
		)
		(* (sign k) (iter (abs k) 0))
	)

	; primary iter subfunc
	(define (iter A B)
		(if (and (empty? A) (empty? B)) #t ; if both lists are empty, everything is alright
			(let ((a (car A)) (b (car B)) )
				(if (= a (revInt b))
					(iter (cdr A) (cdr B)) 
					#f
				)
			)
		)
		
	)

	; match
	(if (and (empty? lA) (empty? lB)) #t ; if both lists are empty, we have nothing to do
		; otherwise, our function have a lot of things to do
		(if (= (length lA) (length lB)) ; check lists length before matching
			(iter lA (reverse lB)) ; initially, reverse list B
			#f
		)
	)
)