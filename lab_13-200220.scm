#lang scheme
; scheme lab set 13
; by Melnikov Sergei, MT-102 (MEH-190102)

; service functions
(define (trim-insignificant-zeros p)
	(if (empty? p) p
		(if (= (car p) 0) (trim-insignificant-zeros (cdr p)) p)
	)
)
(define (polynomial-cproduct p c) ; product of polynomial p & constant value c
	(define (iter p out)
		(if (empty? p) out
			(iter (cdr p) (cons (* c (car p)) out))
		)
	)
	(trim-insignificant-zeros (iter (reverse p) empty))
)
(define (polynomial-eval p x)
	(define (iter p k)
		(if (empty? p) k
			(iter (cdr p)
				(* (+ k (car p)) (if (empty? (cdr p)) 1 x))
			)
		)
	)
	(if (empty? p) 0 (iter p 0))
)

; task 1
(define (polynomial-derivative p)
	(define deg (length p))
	(define (iter p out i)
		(if (empty? (cdr p)) (reverse out)
			(iter (cdr p) (cons (* (- deg i) (car p)) out) (+ i 1) )
		)
	)
	(trim-insignificant-zeros (iter p empty 1) )
)

; task 2
(define (polynomial-sum p1 p2)
	(define (iter p1 p2 out)
		(if (or (empty? p1) (empty? p2))
			(if (empty? p2) 
				(append (reverse p1) out)
				(append (reverse p2) out)
			)
			(iter (cdr p1) (cdr p2) (cons (+ (car p1) (car p2)) out))

		)
	)
	(trim-insignificant-zeros (iter (reverse p1) (reverse p2) empty))
)

; task 3
(define (polynomial-product p1 p2)
	(define (iter p1 p2 k out)
		(if (empty? p2) out
			(iter p1 (cdr p2) (+ k 1)
				(polynomial-sum out
					(polynomial-cproduct (append p1 (build-list k (lambda (x) 0))) (car p2))
				)
			)
		)
	)
	(iter p1 (reverse p2) 0 empty)
)

; task 4
(define (sum k . lst) ; sum of natural numbers in lst, present in cs k: 2<=k<=10
	(define (num->lst n)
		(define (iter n lst)
			(if (= n 0) lst
				(iter (quotient n 10) (cons (remainder n 10) lst))
			)
		)
		(iter n empty)
	)
	(define (lst->num lst)
		(define (iter lst n)
			(if (empty? lst) n
				(iter (cdr lst) (+ (* 10 n) (car lst)))
			)
		)
		(iter (cdr lst) (car lst))
	)

	(define (nums->lst lst)
		(define (iter lst out)
			(if (empty? lst) out
				(iter (cdr lst)
					(if (equal? 0 (car lst))
						out
						(cons (reverse (num->lst (car lst))) out)
					)
				)
			)
		)
		(define (max-deg polynomials_lst)
			(define (iter lst n)
				(if (empty? lst) n
					(let ((deg (length (car lst))))
						(iter (cdr lst) (if (> deg n) deg n))
					)
				)
			)
			(iter polynomials_lst 0)
		)
		(define (mk-same-deg n lst out)
			(if (empty? lst) out
				(let ((cdeg (length (car lst))))
					(mk-same-deg n (cdr lst)
						(cons
							(append (car lst) (build-list (- n cdeg) (lambda (x) 0) ))
						out)
					)
				)
			)
		)
		(let ((out (iter lst empty)))
			(mk-same-deg (max-deg out) out empty)
		)
	)
	(define (iter q lst out)
		(if (empty? lst) (if (= q 0) out (cons q out))
			(iter
				(quotient (+ q (car lst)) k)
				(cdr lst)
				(cons (remainder (+ q (car lst)) k) out)
			)			
		)
	)
	(if (or (< k 2) (> k 10)) (error "Incorrect positional counting system base!")
		(if (= (foldl + 0 lst) 0) 0
			(lst->num
				(iter 0
					(apply map
						(cons
							(lambda (farg . args) (foldl + farg args))
							(nums->lst lst)
						)
					)
					empty
				)
			)
		)
	)
)
; task 5
(define (polynomial-eval-approximation p x0)
	(define p-derivative (polynomial-derivative p))
	(define (iter Xn)
		(define Xn+1
			(- Xn (/ (polynomial-eval p Xn) (polynomial-eval p-derivative Xn)))
		)
		(if (= Xn Xn+1) Xn
			(iter Xn+1)
		)
	)
	(iter (+ x0 .0))
)