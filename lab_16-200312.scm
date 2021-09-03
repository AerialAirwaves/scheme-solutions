#lang scheme

(define (trim code n)
	(if (= n 0) code
		(trim (cdr code) (- n 1))
	)
)
(define (udct-prefix? ctable-pref) ; check if codetable codes list (list of characters codes only) is prefix uniquely decodable
	(define (iter1 clist tpl tpl-len)
		(if (empty? clist) #t
			(let ((c-len (length (car clist)) ))
				(if
					(not
						(if (>= c-len tpl-len)
							(equal? (trim (car clist) (- c-len tpl-len)) tpl )
							(equal? (trim tpl (- tpl-len c-len)) (car clist)) 
						)
					)
					(iter1 (cdr clist) tpl tpl-len)
					#f
				)
			)
		)
	)
	(define (iter0 clist)
		(if (or (empty? clist) (empty? (cdr clist))) #t
			(if (iter1 (cdr clist) (car clist) (length (car clist)))
				(iter0 (cdr clist)) #f
			)
		)
	)
	(iter0 ctable-pref)
)

; task 1

(define (uniquely-decodable? codeTable)
	(let ((ctable-pref (map (lambda (x) (cadr x) ) codeTable ) ))
	(let ((ctable-post (map reverse ctable-pref ))) ; build postfix codes table
		(or
			(udct-prefix? ctable-pref) ; check prefix code table uniquely docodablitiy
			(udct-prefix? ctable-post) ; check postfix code table uniquely docodablitiy
		)
	))
)

; task 2

(define (new-symbol-min-len? codeTable)
	(define (sort-ct codeTable)
		(define (iter ct code0 code1)
			(if (empty? ct) (list code0 code1)
				(iter
					(cdr ct)
					(if (= 0 (caadar ct)) (cons (list (caar ct) (cdadar ct)) code0 ) code0 )
					(if (= 1 (caadar ct)) (cons (list (caar ct) (cdadar ct)) code1 ) code1 )

				)
			)
		)
		(iter codeTable null null)
	)
	(define (build-tree codeTable)
		(if (empty? codeTable) null
			(if (empty? (cadar codeTable)) (caar codeTable)
				(let ((srt (sort-ct codeTable) ))
					(cons (build-tree (car srt)) (build-tree (cadr srt)))
				)
			)
		)
	)
	(define (find-empty2insert codeTable)
		(define (iter ct len code)
			(if (not (or (empty? ct) (pair? ct))) (cons +inf.0 null)
				(if (empty? (car ct))
					(cons (+ 1 len) (cons 0 code))
				(if (empty? (cdr ct))
					(cons (+ 1 len) (cons 1 code))

					(let
						((lout (iter (car ct) (+ 1 len) (cons 0 code) ) ))
					(let
						((rout (iter (cdr ct) (+ 1 len) (cons 1 code) ) ))
						(if (<= (car lout) (car rout)) lout rout)
					)
					)
				)
				)
			)
		)
		(if (empty? codeTable) (list 0)
			(cdr (iter codeTable 0 null))
		)
	)

	; funcs match superposition
	(define (pref-code codeTable) ; shortest new sym code for prefix unique code table
		(let ((o (find-empty2insert (build-tree (map (lambda (x) (cons (car x) (cons (reverse (cadr x)) null) )) codeTable))) ))
			(if (empty? o) #f o)
		)
	)
	(define (post-code codeTable) ; shortest new sym code for postfix unique code table
		(let ((o (find-empty2insert (build-tree codeTable)) ))
			(if (empty? o) #f (reverse o))
		)
	)

	; correctly match both methods and choose optimal
	(let ((ctable-pref (map (lambda (x) (cadr x) ) codeTable ) ))
	(let ((ctable-post (map reverse ctable-pref ) ))
	(let ((pref-decodable (udct-prefix? ctable-pref) )) ; check prefix code table uniquely docodablitiy
	(let ((post-decodable (udct-prefix? ctable-post) )) ; check postfix code table uniquely docodablitiy
		(if (or pref-decodable post-decodable)
			(if pref-decodable
				(if post-decodable
					(let ((o1 (pref-code codeTable) ))
					(let ((o2 (post-code codeTable) ))
						(if (and o1 o2) ; if both are built successfully
							(if (<= (length o1) (length o2)) o1 o2 ) ; return shortest code if both decodable
							
							(or o1 o2) ; else return any not null
						)
					))
					(pref-code codeTable) ; if only prefix decodable, handle via it's method only
				)
				(post-code codeTable) ; if only postfix decodable, handle via it's method only
			) #f
		)
	))))
)

; task 3

(define (shortenable-codes-present? codeTable)
	(if (not (empty? codeTable))
		; first step - find max length coded element
		(let ((o2  (foldl (lambda (n p) (if (> (length (cadr n)) (length (cadr p))) n p) ) (car codeTable) (cdr codeTable) ) ))
		; second step - determine, how short symbol can be appended to current code table with no max code length symbol present
		(let ((o1 (new-symbol-min-len? (remove (car o2) codeTable (lambda (e x) (equal? e (car x)) ) ) )))
			(if o1 ; if new symbol can be inserted, check if its code length lower than origin max code length
				(if (> (- (length (cadr o2)) (length o1)) 0) (car o2) #f)
				#f
			) ; otherwise, press #f to pay respect
		)
		)
		#f
	)
)

; task 4

(define (haffman-tree-freq-distrib codeTree)
	(define (iter tree depth)
		(if (not (pair? (car tree))) (cons (cdr tree) depth)
			(let ((l (iter (car tree) (+ depth 1) )))
			(let ((r (iter (cdr tree) (+ depth 1))))
				(if (not (or (empty? (car l)) (empty? (car r))))
					(cons l r)
					(if (and (empty? (car l)) (empty? (car r))) null
						(if (empty? (car l)) r l)
					)
				)
			))

		)
	)
	; tree -> list (sym1 codelen1 sym2 codelen2 .. )
	(define res (flatten (iter codeTree 0)))

	; get the longest coded word's code length
	(define (m rlst i)
		(if (empty? rlst) i
			(m (cddr rlst) (max i (cadr rlst)) )
		)
	)
	(define M (m res -inf.0))

	; get total quantity of syms
	; formula: q(symbol) = max_symbol_length + 1 - symbol.code.length // the longer symbol's code, the more odd its presence in data
	(define (t rlst i)
		(if (empty? rlst) i
			(t (cddr rlst) (+ i (- (+ M 1) (cadr rlst) )) ) 
		)
	)
	(define T (t res 0))


	; evaluate & format final result
	(define (p rlst out)
		(if (empty? rlst) out
			(p (cddr rlst) (cons (cons (car rlst) (/ (+ .0 (- (+ M 1) (cadr rlst) )) T ) ) out) )
		)
	)
	(p res null)
)