#lang scheme

; writing our own bicycles just because
(define (my-string->list str)
	(define (iter i out)
		(define l (string-length str))
		(if (= i l) (reverse out)
			(iter (+ i 1) (cons (string-ref str i) out) )
		)
	)
	(iter 0 empty)
)
(define (my-list->string lst)
	(apply string lst)
)

; Task 1
(define (strtoint str)
	(define (char2num chr)
		(- (char->integer chr) 48) ; why should you write a lot of cond switchcases, if you can just use ASCII magic
	)
	(define (iter lst num)
		(if (empty? lst) num
			(iter (cdr lst) (+ (* num 10) (char2num (car lst)) ) )
		)
	)
	(iter (my-string->list str) 0)
)

; Task 3
(define (split str delim)
	(define origin-len (string-length str))
	(define delim-len (string-length delim))
	(define delim-lst (my-string->list delim))
	(define delim-first-char (car delim-lst))

	(define (iter lst cur c out)
		(if (empty? lst) (reverse (if (empty? cur) out (cons (my-list->string (reverse cur)) out)))
			(if
				(and ; making step-by-step check to prevent func crash. this is possible only thnx to normal reduction order of built-in logical operations
					(equal? (car lst) delim-first-char )
					(>= (- origin-len c) delim-len)
					(equal? (take lst delim-len) delim-lst )
				)
				(iter (list-tail lst delim-len) null (+ c delim-len) (if (empty? cur) out (cons (my-list->string (reverse cur)) out)))
				(iter (cdr lst) (cons (car lst) cur) (+ c 1) out)
			)
		)
	)
	(iter (my-string->list str) null 0 null)
)

; Task 2
(define (words-count str delim)
	(length (split str delim))
)

; Task 4
(define (contains-repeated-words? str)
	(define (iter words-list)
		(if (empty? words-list) #f
			(if (member (car words-list) (cdr words-list))
				(car words-list)
				(iter (cdr words-list))
			)
		)
	)
	(iter (split str " ") )
)