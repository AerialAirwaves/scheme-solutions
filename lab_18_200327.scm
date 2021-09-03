#lang scheme

; Lab_18
; by Melnikov Sergei, MEH-190102

; task 1
(define (qsort lst [partial-order-relation <=])
	; linear iteration, that divides list to three sublists according to given partial order relation: LOwer, EQual, HIgher than/to chosen element
	(define (iter s lst lo eq hi partial-order-relation)
		(if (empty? lst) (list lo eq hi)
			(cond
				[ (equal? s (car lst))   (iter s (cdr lst) lo (cons (car lst) eq) hi ) ]
				[ (partial-order-relation (car lst) s) (iter s (cdr lst) (cons (car lst) lo ) eq hi ) ]
				[ else (iter s (cdr lst) lo eq (cons (car lst) hi))]
			)
		)
	)

	(if (empty? lst) null ; do not handle null lists, bc they cannot be car'ed
		(let ((d (iter (car lst) (cdr lst) null (list (car lst)) null partial-order-relation ) )) ; run our linear list division subfunc, using first element as division sample
			(append
				(qsort (car d) partial-order-relation)
				(cadr d)
				(qsort (caddr d) partial-order-relation)
			)
		)
	)
)

; task 2

(define (inssetion-sort-by-nums-sum lst)
	(define (nums-sum-iter n t) ; subfunc task is obvious
		(if (= 0 n) t
			(nums-sum-iter (quotient n 10) (+ t (remainder n 10)))
		)
	)

	(define (insert n lst)
		(if (empty? lst) (list n)
			(let ((sn (nums-sum-iter n 0)))
				(if (>= sn (nums-sum-iter (car lst) 0)) (cons n lst)
					(cons (car lst) (insert n (cdr lst)))
				)
			)
		)
	)

	(if (empty? lst) null
		(insert (car lst) (sort (cdr lst)))
	)
)

; task 3
(define (process-ints)
	; filenames definition
	(define input_file "in.txt")
	(define output_file "out.txt")

	; opening files IO flows/ports
	(define in  (open-input-file  input_file) )
	(define out (open-output-file output_file #:exists 'replace))

	(define (iter state n sum quantity)
		(define new_char (read-char in))
		(if (equal? new_char eof)
			(begin
				(if (= state 1) (writeln n out) null)
				(writeln sum out)
				(writeln quantity out)
				(close-input-port in)
				(close-output-port out)
			)
			(let ((t (char->integer new_char)))
				(cond
					[ (= state 0) ; state 0: previously we've read nothing that could be part of integer
						(cond
							[ (and (<= 48 t) (<= t 57) ) (iter 1 (- t 48) sum quantity) ]  ; to state 1 with non-negaitive n
							[ (equal? new_char #\-) (iter 2 0 sum quantity)]
							[ else (iter 0 0 sum quantity)] ; otherwise do nothing
						)

					]
					[ (= state 1) ; state 1: previously we've read number character
						(if (and (<= 48 t) (<= t 57) ) ; let's assume, that integers in file we're dealing with is not contain non-significant zeros, otherwise, negative numbers with insignigicant zeros will be interpreted as positive ones
							(iter 1 ( (if (>= n 0) + -) (* n 10) t -48 ) sum quantity )							
							(begin
								(writeln n out)
								(iter (if (equal? new_char #\-) 2 0) 0 (+ sum n) (+ quantity 1))
							)
							
						)
					]
					[ (= state 2) ; state 2: previously we've read dash character
						(cond
							[ (and (<= 48 t) (<= t 57) ) (iter 1 (- 48 t) sum quantity) ] ; to state 1 with negative n
							[ (equal? new_char #\-) (iter 2 0 sum quantity)]
							[else (iter 0 0 sum quantity)] ; otherwise do nothing
						)
					]
				)
			)
		)
	)
	(iter 0 0 0 0)
)

; task 4
(define (remove-hyphenations)
	; filenames definition
	(define input_file "in.txt")
	(define output_file "out.txt")

	; opening files IO flows/ports
	(define in  (open-input-file  input_file) )
	(define out (open-output-file output_file #:exists 'replace))

	(define (iter state)
		(define new_char (read-char in))
		(cond
			[ (= state 0) ; state 0: previous symbol belongs to set PLAINTEXT \ { #\space #\return #\newline #\- }
			(cond
				[ (equal? new_char eof)
					(begin
						(close-input-port in)
						(close-output-port out)
					)
				]
				[ (equal? new_char #\space)
					(begin
						(display new_char out)
						(iter 1)
					)
				]
				[ (equal? new_char #\return)
					(begin
						(display new_char out)
						(iter 2)
					)
				]
				[ (equal? new_char #\newline)
					(begin
						(display new_char out)
						(iter 3)
					)
				]
				[ (equal? new_char #\-) (iter 5) ]
				[ else
					(begin
						(display new_char out)
						(iter 0)
					)
				]
			)]
			[ (= state 1) ; state 1: previous symbol was space
			(cond
				[ (equal? new_char eof)
					(begin
						(close-input-port in)
						(close-output-port out)
					)
				]
				[ (equal? new_char #\space)
					(begin
						(display new_char out)
						(iter 1)
					)
				]
				[ (equal? new_char #\return)
					(begin
						(display new_char out)
						(iter 2)
					)
				]
				[ (equal? new_char #\newline)
					(begin
						(display new_char out)
						(iter 3)
					)
				]
				[ (equal? new_char #\-)
					(begin
						(display new_char out)
						(iter 4)
					)
				]
				[ else
					(begin
						(display new_char out)
						(iter 0)
					)
				]
			)]
			[ (= state 2) ; state 2: previous symbol was return
			(cond
				[ (equal? new_char eof)
					(begin
						(close-input-port in)
						(close-output-port out)
					)
				]
				[ (equal? new_char #\space)
					(begin
						(display new_char out)
						(iter 1)
					)
				]
				[ (equal? new_char #\return)
					(begin
						(display new_char out)
						(iter 2)
					)
				]
				[ (equal? new_char #\newline)
					(begin
						(display new_char out)
						(iter 3)
					)
				]
				[ (equal? new_char #\-)
					(begin
						(display new_char out)
						(iter 4)
					)
				]
				[ else
					(begin
						(display new_char out)
						(iter 0)
					)
				]
			)]
			[ (= state 3) ; state 3: previous symbols was newline
			(cond
				[ (equal? new_char eof)
					(begin
						(close-input-port in)
						(close-output-port out)
					)
				]
				[ (equal? new_char #\space)
					(begin
						(display new_char out)
						(iter 1)
					)
				]
				[ (equal? new_char #\return)
					(begin
						(display new_char out)
						(iter 2)
					)
				]
				[ (equal? new_char #\newline)
					(begin
						(display new_char out)
						(iter 3)
					)
				]
				[ (equal? new_char #\-)
					(begin
						(display new_char out)
						(iter 4)
					)
				]
				[ else
					(begin
						(display new_char out)
						(iter 0)
					)
				]
			)]
			[ (= state 4) ; state 4: previous symbols sequence " -" seems like dash
			(cond
				[ (equal? new_char eof)
					(begin
						(close-input-port in)
						(close-output-port out)
					)
				]
				[ (equal? new_char #\space)
					(begin
						(display new_char out)
						(iter 1)
					)
				]
				[ (equal? new_char #\return)
					(begin
						(display new_char out)
						(iter 2)
					)
				]
				[ (equal? new_char #\newline)
					(begin
						(display new_char out)
						(iter 3)
					)
				]
				[ (equal? new_char #\-)
					(begin
						(display new_char out)
						(iter 4)
					)
				]
				[ else
					(begin
						(display new_char out)
						(iter 0)
					)
				]
			)]
			[ (= state 5) ; state 5: previous symbols sequence "-" seems like hyphen
			(cond
				[ (equal? new_char eof)
					(begin
						(display #\- out)
						(close-input-port in)
						(close-output-port out)
					)
				]
				[ (equal? new_char #\space)
					(begin
						(display #\- out)
						(display new_char out)
						(iter 1)
					)
				]
				[ (equal? new_char #\return) (iter 6) ]
				[ (equal? new_char #\newline) (iter 6) ]
				[ (equal? new_char #\-)
					(begin
						(display new_char out)
						(iter 4)
					)
				]
				[ else
					(begin
						(display #\- out)
						(display new_char out)
						(iter 0)
					)
				]
			)]

			[ (= state 6) ; state 6: previous symbol is a hyphenation sign, just ignoring one newline symbols sequence, and then moving to state 7
			(cond
				[ (equal? new_char eof)
					(begin
						(display #\- out)
						(close-input-port in)
						(close-output-port out)
					)
				]
				[ (equal? new_char #\space)
					(begin
						(display #\return out)
						(display #\newline out)
						(iter 1)
					)
				]
				[ (equal? new_char #\return)
					(begin
						(iter 6)
					)
				]
				[ (equal? new_char #\newline)
					(begin
						(iter 7)
					)
				]
				[ (equal? new_char #\-)
					(begin
						(iter 5)
					)
				]
				[ else
					(begin
						(display new_char out)
						(iter 6)
					)
				]
			)]

			[ (= state 7) ; state 7: previous symbol is a part of hyphenated word's part
			(cond
				[ (equal? new_char eof)
					(begin
						(close-input-port in)
						(close-output-port out)
					)
				]
				[ (equal? new_char #\space)
					(begin
						(display #\return out)
						(display #\newline out)
						(iter 1)
					)
				]
				[ (equal? new_char #\return)
					(begin
						(display new_char out)
						(iter 2)
					)
				]
				[ (equal? new_char #\newline)
					(begin
						(display new_char out)
						(iter 3)
					)
				]
				[ (equal? new_char #\-)
					(begin
						(iter 5)
					)
				]
				[ else
					(begin
						(display new_char out)
						(iter 7)
					)
				]
			)]
		)
	)
	(iter 0)
)

; task 5
(define (find&replace-translate)
	; filenames definition
	(define input_file "in.txt")
	(define dictionary_file "dict.txt")
	(define output_file "out.txt")

	; opening files IO flows/ports
	(define in  (open-input-file  input_file) )
	(define out (open-output-file output_file #:exists 'replace))

	; even if this method rereads dictionary file as many times as we have words in input file, this solution does not load whole dictionary file into RAM so can be executed even if free RAM is not enough
	(define (find sub dictf)
		(define word (read dictf))
		(cond 
			[ (equal? word eof) (begin (close-input-port dictf) sub) ]
			[ (equal? word sub) (read dictf) ]
			[ else (find sub dictf) ]
		)
	)

	(define (iter)
		(define sub (read in))
		(if (equal? sub eof) (close-output-port out) 
			(begin
				(display (find sub (open-input-file dictionary_file)) out)
				(display " " out)
				(iter)
			)
		)
	)
	(iter)
)