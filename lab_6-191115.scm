#lang scheme

(define (my-member a lst)
	(if (empty? lst) #f
		(if (equal? a (car lst)) (cdr lst)
			(my-member a (cdr lst))
		)
	)
)

; Task 1
(define (ulength lst)
	(define (iter origin unique)
		(if (empty? origin) (length unique)
			(iter (cdr origin)
				(if (my-member (car origin) unique) unique	
					(cons (car origin) unique)
				)
			)
		)
	)
	(iter lst '())
)

; Task 2
(define (uquantity lst)
	(define (quantity a lst)
		(define (iter lst k)
			(if (empty? lst) k
				(iter (cdr lst)
					(+ k
						(or (and (equal? (car lst) a) 1) 0)
					)
				)
			)
		)
		(iter lst 0)
	)
	(define (iter origin unique q)
		(if (empty? origin) (reverse q)
			(if (my-member (car origin) unique)
				(iter (cdr origin) unique q) 
				(iter (cdr origin) (cons (car origin) unique) (cons (cons (car origin) (quantity (car origin) lst) ) q ) ) 
			)
		)
	)
	(iter lst '() '())
)

; Task 3
(define (fibmembq lst)
	(define (fib n)
		(define (iter a b k)
			(if (= k 0) b (iter b (+ a b) (- k 1)))
		)
		(if (= 0 n) 0
			(iter 1 1 (- n 1))
		)
	)
	(define (fib-index-prior-to n)
		(define (iter k)
			(if (> (fib k) n) (- k 1)
				(iter (+ k 1))
			)
		)
		(iter 1)
	)
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

	(define (iter k n)
		(if (= k -1) n
			(iter (- k 1)
				(+ n
					(if (my-member (fib k) lst)
						1
						0
					)
				)
			)
		)
	)
	(iter (fib-index-prior-to (max-lst lst)) 0)
)

; Task 4
(define (mprimes lst1 lst2)
	(define (gcd a b)
		(if (= b 0) a
			(gcd b (remainder a b))
		)
	)
	(define (iter l1 l2 out)
		(if (or (empty? l1) (empty? l2)) (reverse out)
			(iter (cdr l1) (cdr l2)
				(let ((a (car l1)) (b (car l2)))
					(if (= 1 (gcd a b))
						(cons (cons a b) out)
						out
					)
				)
			)
		)
	)
	(iter lst1 lst2 '())
)

; Task 5
(define (sublists lst)
	(define (iter rev out)
		(if (empty? rev) out
			( iter (cdr rev) (cons (reverse rev) out) )
		)
	)
	(iter (reverse lst) '())
)