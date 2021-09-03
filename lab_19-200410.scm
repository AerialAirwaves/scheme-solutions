#lang scheme

; task 1
(define (format ifName ofName . params)
	(define (write-buf out_flow buf)
		(if (empty? buf) null
			(begin
				(display (car buf) out_flow)
				(write-buf out_flow (cdr buf))
			)
		)
	)
	(define (buf->num buf)
		(define (iter lst n)
			(if (empty? lst) n
				(if (or (> (char->integer (car lst)) 57) (< (char->integer (car lst)) 48) ) -1
					(iter (cdr lst) (+ (* n 10) (- (char->integer (car lst)) 48)))
				)
			)
		)
		(if (empty? buf) -2
			(iter (reverse buf) 0)
		)
	)

	(define in  (open-input-file  ifName) )
	(define out (open-output-file ofName #:exists 'replace))

	(define (iter state buf)
		(define new_char (read-char in))
		(cond
			[ (= state 0) ; within regular text
				(cond
					[(equal? new_char eof)
						(begin
							(close-input-port in)
							(close-output-port out)
						)
					]
					[(equal? new_char #\%) (iter 1 null)]
					[else
						(display new_char out)
						(iter 0 null)
					]
				)
			]
			[ (= state 1) ; reached opening %
				(cond
					[(equal? new_char eof)
						(write-buf out (cons #\% (reverse buf)))
						(close-input-port in)
						(close-output-port out)
					]
					[(equal? new_char #\return)
						(read-char in) ; prevent #\newline , which should appear next, from handling
						(write-buf out (cons #\% (reverse buf)))
						(write-buf out '(#\return #\newline)) ; but write entire standardised new line symbols sequence
						(iter 0 null)
					]
					[(equal? new_char #\newline)
						(write-buf out (cons #\% (reverse buf)))
						(display #\newline out)
						(iter 0 null)
					]
					[(equal? new_char #\%)
						(let ((o (buf->num buf)))
							(if (> o 0) (display (list-ref params (- o 1)) out)
								(begin
									(display #\% out)
									(if (= o -2) null (write-buf out (reverse (cons #\% buf))))
								)
							)
						)
						(iter 0 null)
					]
					[else (iter 1 (cons new_char buf))]
				)
			]
		)
	)
	(iter 0 null)
)

; task 2

(define (task2)
	; filenames definition
	(define input_file "in.txt")
	(define output_file "out.txt")

	; opening files IO flows/ports
	(define in  (open-input-file  input_file) )
	(define out (open-output-file output_file #:exists 'replace))

	(define (rev&unite l1 l2)
		(define (iter l1 l2 n out)
			(if (empty? l1) out
				(if (empty? l2)
					(iter (cdr l1) l2 (quotient (+ n (car l1)) 10) (cons (remainder (+ n (car l1)) 10) out))
					(iter (cdr l1) (cdr l2) (quotient (+ n (car l1) (car l2)) 10) (cons (remainder (+ n (car l1) (car l2)) 10) out))
				)
			)
		)
		
		(if (>= (length l1) (length l2)) (iter l1 l2 0 null) (iter l2 l1 0 null))
	)
	(define (lst->num lst)
		(define (iter lst n)
			(if (empty? lst) n
				(iter (cdr lst) (+ (* 10 n) (car lst)))
			)
		)
		(iter (cdr lst) (car lst))
	)

	(define (write-result lsums out)
		(if (empty? lsums) (close-output-port out)
			(begin
				(display (car lsums) out)
				(if (not (empty? (cdr lsums))) (display #\space out) null)
				(write-result (cdr lsums) out)
			)
		)
	)

	(define (iter state l1 l2 lsums)
		(define new_char (read-char in))
		(cond
			[(= state 0) ; reading first number
				(cond
					[(equal? new_char eof)
						(close-input-port in)
						(write-result (sort lsums >) out)
					]
					[(equal? new_char #\+) (iter 1 l1 l2 lsums)]
					[else (iter 0 (cons (- (char->integer new_char) 48) l1) null lsums) ]
				)
			]

			[(= state 1) ; reading second number
				(cond
					[(equal? new_char eof)
						(write-result
							(sort
								(let ((o (rev&unite l1 l2)))
									(if (equal? o (sort o <)) (cons (lst->num o) lsums) lsums)
								) >
							)
							out
						)
						(close-input-port in)
					]
					[(equal? new_char #\return) (iter 1 l1 l2 lsums)]
					[(equal? new_char #\newline)
						(let ((o (rev&unite l1 l2)))
							(iter 0 null null (if (equal? o (sort o <)) (cons (lst->num o) lsums) lsums) )
						)
					]
					[else (iter 1 l1 (cons (- (char->integer new_char) 48) l2) lsums) ]
				)
			]
		)
	)
	(iter 0 null null null)
)

;task 3
(define (funcsQuantity inF)
	; notice: counts all functions, defined via template (define (funcname)).
	; functions. defined via (define funcname (lambda (x))) are valid, but ignored, because task definition says we shoudn't count lambdas
	(define in (open-input-file inF))
	(define (iter state n)
		(define new_char (read-char in))
		(cond
			[(equal? new_char eof) (begin (close-input-port in) n)]
			[(equal? new_char #\( ) 
				(cond
					[(= state 0) (iter 1 n)]
					[(= state 7) (iter 0 (+ n 1))]
					[(= state 13) (iter 0 n)]
					[(and (>= state 8) (<= state 16)) (iter state n)]
					[else (iter 0 n)]
				)
			]
			[(and (equal? new_char #\d) (= state 1) ) (iter 2 n)]
			[(equal? new_char #\e)
				(cond
					[(and (>= state 8) (<= state 16)) (iter state n)]
					[(= state 2) (iter 3 n)]
					[(= state 6) (iter 7 n)]
					[else (iter 0 n)]
				)
			]
			[(and (equal? new_char #\f) (= state 3)) (iter 4 n)]
			[(and (equal? new_char #\i) (= state 4)) (iter 5 n)]
			[(and (equal? new_char #\n) (= state 5)) (iter 6 n)]

			[(or (equal? new_char #\newline)
				 (equal? new_char #\return)
			 )
				(iter (if (or (= state 8) (and (not (= state 11)) (>= state 0) (<= state 15))) 0 state) n)
			]
			[(equal? new_char #\ ) (iter (if (or (= state 1) (= state 7) (= state 8) (= state 11) (= state 15)) state 0) n) ]
			[(equal? new_char #\;) (iter (if (>= state 8) state 8) n)]
			[(equal? new_char #\#)
				(cond
					[(= state 8) (iter 8 n)] ; ignore switch inside whole line comment
					[(= state 9) (iter 10 n)] ; not a code until space
					[(= state 10) (iter 10 n)]
					[(= state 12) (iter 0 n)]
					[(= state 13) (iter 13 n)]
					[else (iter 9 n)]
				)
			]
			[(equal? new_char #\|)
				(cond
					[(= state 9) (iter 11 n)]
					[(= state 11) (iter 12 n)]
					[else (iter state n)]
				)
			]
			[(equal? new_char #\\)
				(cond
					[(= state 8) (iter 8 n)]
					[(= state 9) (iter 13 n)]
					[(= state 13) (iter 0 n)]
					[(= state 15) (iter 16 n)]
					[(= state 16) (iter 15 n)]
					[else (iter 14 n)]
				)
			]
			[(equal? new_char #\")
				(cond
					[(= state 15) (iter 0 n)]
					[(= state 16) (iter 15 n)]
					[else (iter 15 n)]
				)
			]
			[else (iter (if (or (= state 8) (= state 11)) state 0) n)]
		)
	)
	(iter 0 0)
)

;task 4
(define (funcsList inF outNonRecF outRecursiveF)
	; this thing probably doesn't contain any good algorythm at all.. but! IT WORKS!
	(define in (open-input-file inF))
	(define (func? lst)
		(if (and (list? lst) (not (empty? lst)) (equal? (car lst) 'define) (>= (length lst) 3) (list? (cadr lst)) (not (empty? (cadr lst))) )
			(caadr lst) #f
		)
	)
	(define (func-body lst) (cddr lst))

	(define (app-unique lst1 lst2)
		(if (empty? lst1) lst2
			(app-unique (cdr lst1)
				(if (member (car lst1) lst2) lst2 (cons (car lst1) lst2))
			)
		)
	)

	(define (lst->funcs lst nrfs rfs) ; analyse nested lists and returns pair, contains lists: (nonrecursivefuncs . recursivefuncs)
		(define (iter lst nrfs rfs depth sub)
		#| constants meaining:
			lst: current list
			nrfs: NonRecursiveFuncS
			rfs: obvious
			depth: list containing names of nested functions we are inside
			sub: boolean, that becomes enabled in nested non-definitive brackets in order to prevent unnecessary recent function name writedown, that normally occurs each time we encoutered end of list
		|#
			(if (empty? lst)
				(cons
					(if (or (empty? depth) sub (member (car depth) rfs) )
						nrfs
						(cons (car depth) nrfs)
					)
					rfs
				)
				(if (func? (car lst))
					; if next list element seems like function, then we should inspect it as function
					(let ((out (iter (func-body (car lst)) nrfs null (cons (func? (car lst)) depth ) sub )))
						(iter (cdr lst) (car out) (if (empty? (cdr out)) rfs (app-unique (cdr out) rfs)) depth sub)
					)
					(if (empty? depth)
						; if we aren't inside function and have goal to search for recursive function self-call, we need to inspect nothing, but functions
						(iter (cdr lst) nrfs rfs depth sub)
						; otherwise, we should inspect any function call. in terms of lists analysing, we need any non-empty list
						(if (and (list? (car lst)) (not (empty? (car lst)))) ; if this is non-empty list
							(if (member (caar lst) depth)
							; if called function is present in nested funcs list, then this is a recursive function
							; notice: this is a primitive algo, that doesn't model functions sequential recursive call.
							; by this algo's design, recursive func is literally any function, that contains instruction to execute inself
								(let ((out (iter (car lst) nrfs null depth #t)))
									(iter (cdr lst) (car out) (if (empty? (cdr out)) (cons (caar lst) rfs) (app-unique (cdr out) (cons (caar lst) rfs))) depth sub)
								)
								(let ((out (iter (car lst) nrfs null depth #t)))
									(iter (cdr lst) (car out) (if (empty? (cdr out)) rfs (app-unique (cdr out)  rfs)) depth sub)
								)
							)
							(iter (cdr lst) nrfs rfs depth sub)
						)	
					)
				)
			)
		)
		(iter lst nrfs rfs null #f)
	)

	; main iterative subfunc
	(define (main nrfs rfs)
		(with-handlers ([exn:fail? (lambda (exn) (main nrfs rfs) )])
			(let (( new-data (read in) ))
				(if (equal? new-data eof) (begin (close-input-port in) (cons nrfs rfs))
					(if (and (list? new-data) (not (empty? new-data)))
						(let ((out (lst->funcs (list new-data) nrfs rfs)))
							(main (car out) (cdr out))
						)
						(main nrfs rfs)
					)
				)
			)
		)
	)
	; evaluate result
	(define result (main null null))
	; then, finally write it to file
	(define nout (open-output-file outNonRecF #:exists 'replace))
	(define rout (open-output-file outRecursiveF #:exists 'replace))
	(define (write-result lst fileport)
		(if (empty? lst) (close-output-port fileport)
			(begin
				(writeln (car lst) fileport)
				(write-result (cdr lst) fileport)
			)
		)
	)
	(write-result (car result) nout)
	(write-result (cdr result) rout)
)