#lang scheme
; linear equations systems solver v0.3 by nm101

; Changelog

; v0.3
; + Now shows pseudosolutions, if system is unsolvable

; v0.2:
; + Simplifies FSS vectors components whenever it possible
; + Now works with complex numbers

; v0.1:
; + Was literally rewritten to scheme from ulearn task solution
; + Shows entire system solution, not private one only

(define accuracy 1e-9) ; infinitesimal positive for double comparison


; matrix is list of matrix rows lists
; free-members is single column of free members
(define (get-augmented matrix free-members)
	(define rowsCount (length (car matrix)))
	(define columnsCount (length free-members))
	(define free-membersVec (list->vector free-members))
	(define matrix-vec (list->vector (map list->vector matrix)))

	(build-vector columnsCount
		(lambda (row)
			(build-vector (+ 1 rowsCount)
				(lambda (column)
					(if (< column rowsCount)
						(vector-ref (vector-ref matrix-vec row) column)
						(vector-ref free-membersVec row)
					)
				)
			)
		)
	)
)

(define (abs-complexSafe x) ; to deal with both complex and real numbers
	(if (real? x) (abs x)
		(magnitude x)
	)
)

(define (complex-conjugate x)
	(if (real? x) x
		(- (real-part x) (* (imag-part x) 0+1i))
	)
)

(define (swap vec posA posB) ; swap two elements with positions posA & posB in vector
	(build-vector (vector-length vec)
		(lambda (pos)
			(vector-ref vec
				(cond
					((= pos posA) posB)
					((= pos posB) posA)
					(else pos)
				)
			)
		)
	)
)

(define (transpose matrix-vec) ; transpose matrix !in vector representation!
	(define rowsCount (vector-length matrix-vec))
	(define columnsCount (vector-length (vector-ref matrix-vec 0)))

	(build-vector columnsCount
		(lambda (i)
			(build-vector rowsCount
				(lambda (j)
					(vector-ref (vector-ref matrix-vec j) i)
				)
			)
		)
	)
)

(define (matrix-map matrix-vec op) ;performs map on each matrix element, matrix must be submitted as vector
	(define rowsCount (vector-length matrix-vec))
	(define columnsCount (vector-length (vector-ref matrix-vec 0)))

	(build-vector rowsCount
		(lambda (i)
			(build-vector columnsCount
				(lambda (j)
					(op (vector-ref (vector-ref matrix-vec i) j))
				)
			)
		)
	)
)

(define (matrix-complex-conjugate matrix-vec)
	(matrix-map matrix-vec complex-conjugate)
)

(define (matrix-product m1 m2) ; works with vector matrices only
	(define rowsCount1 (vector-length m1))
	(define columnsCount1 (vector-length (vector-ref m1 0)))
	(define rowsCount2 (vector-length m2))
	(define columnsCount2 (vector-length (vector-ref m2 0)))

	(if (not (= columnsCount1 rowsCount2)) #f
		(build-vector rowsCount1
			(lambda (i)
				(build-vector columnsCount2
					(lambda (k)
						(foldl
							(lambda (j sum)
								(+ sum
									(* 
										(vector-ref (vector-ref m1 i) j)
										(vector-ref (vector-ref m2 j) k)
									)
								)
							)
							0 (build-list columnsCount1 +)
						)
						
					)
				)
			)
		)
	)
)

(define (rows-addition matrix modifiableRow addableRow coefficient)
	(define vec1 (vector-ref matrix modifiableRow))
	(define vec2 (vector-ref matrix addableRow))
	(build-vector (vector-length matrix)
		(lambda (row)
			(if (= row modifiableRow)
				(build-vector (vector-length (vector-ref matrix 0))
					(lambda (col)
						(+ (vector-ref vec1 col) (* coefficient (vector-ref vec2 col)))
					)
				)
				(vector-ref matrix row)
			)
		)
	)
)

(define (gauss-elim matrix row column answer-rows) ; gauss elimination: make extended matrix step matrix & find answer rows
	(define rowsCount (vector-length matrix))
	(define columnsCount (- (vector-length (vector-ref matrix 0)) 1))

	(if (or (>= row rowsCount) (>= column columnsCount))
		(cons 
			(vector-filter ; remove null rows
				(lambda (row) (> (foldl (lambda (n p) (+ p (abs-complexSafe n))) 0 (vector->list row)) accuracy))
				matrix
			)
			answer-rows
		)

		(let ((maxByAbsInColumn
			(first
				(sort
					(map
						(lambda (rowIndex)
							(cons (abs-complexSafe (vector-ref (vector-ref matrix rowIndex) column)) rowIndex)
						)
						(range row rowsCount)
					
					)
					(lambda (x y) (>= (car x) (car y)))
				)
			)))
			(if (< (car maxByAbsInColumn) accuracy)
			(gauss-elim matrix row (+ column 1) answer-rows)
			(gauss-elim
				(foldl
					(lambda (modifiableRow matrix)
						(if (= row modifiableRow) matrix
							(rows-addition matrix modifiableRow row
								(- (/ (vector-ref (vector-ref matrix modifiableRow) column)
									(vector-ref (vector-ref matrix row) column)))
							)
						)
					)
					(swap matrix row (cdr maxByAbsInColumn))
					(range rowsCount)
				)
				(+ row 1) (+ column 1)
				(build-vector (vector-length answer-rows) (lambda (index) (if (= index column) row (vector-ref answer-rows index))))
			)
		)
		)
	)
)

(define (solvable? step-matrix)
	(define v (vector->list step-matrix))
	(=
		(foldl ; evaluate augmented matrix rank
			(lambda (row rank)
				(+ rank (if (> (foldl (lambda (n c) (+ c (abs-complexSafe n))) 0 (vector->list row)) accuracy) 1 0))
			)
			0 v
		)
		(foldl ; evaluate main matrix rank
			(lambda (row rank)
				(+ rank (if (> (foldl (lambda (n c) (+ c (abs-complexSafe n))) 0 (vector->list (vector-drop-right row  1))) accuracy) 1 0))
			)
			0 v
		)
	)
)

(define (get-solution matrix answer-rows) ; get signular solution / shift vector for linear variety of solutions
	(define columnsCount (- (vector-length (vector-ref matrix 0)) 1))
	(build-vector (vector-length answer-rows)
		(lambda (col)
			(define row (vector-ref answer-rows col))
			(if (= row -1) 0
				(/ (vector-ref (vector-ref matrix row) columnsCount)
					(vector-ref (vector-ref matrix row) col))
			)
		)
	)
)

(define (get-fss matrix answer-rows) ; get fundamental solution system of corresponding homogeneous system
	(define columnsCount (vector-length answer-rows))
	(define rowsCount (vector-length matrix))
	(define (iter index fss)
		(if (<= columnsCount index) fss
			(iter (+ index 1)
				(if (not (= -1 (vector-ref answer-rows index))) fss
					(cons 
						(build-vector columnsCount
							(lambda (x)
								(if (= x index) 1 
									(if (or (>= x rowsCount) (= 0 (vector-ref (vector-ref matrix x) x))) 0
										(- (/ (vector-ref (vector-ref matrix x) index) (vector-ref (vector-ref matrix x) x)))
									)
								)
							)
						)
						fss
					)
				)
			)
			
		)
	)
	(beautify-fss (iter 0 null))
)

(define (beautify-fss fss) ; make fss vectors w/ rational coordinates look less complicated
	(map 
		(lambda (vec)
			(define lst (vector->list vec))
			(define coefficient
				(foldl lcm 1 (map denominator
						(append (filter rational? lst) ; handle rational values
							; handle complex numbers w/ rational both real & imaginary parts
							(map (lambda (x) (gcd (real-part x) (imag-part x)))
								(filter (lambda (x) (and (complex? x) (not (real? x))
									(rational? (real-part x)) (rational? (imag-part x)))) lst)
							)
						)
					)
				)
			)
			(build-vector (vector-length vec) (lambda (index) (* coefficient (vector-ref vec index))))
		)
		fss
	)
)

; main function
; receives linear equations system as main matrix and free members column
; main matrix interpretation: list of lists, each of 'em represents single row
; free members column interpretation: list, representing column of free members
; Ex:
; 3x + 2y = 2
; 5x - 2z = 3
; 2y + 5z = 4
; should be submitted as:
; (solve '( (3 2 0) (5 0 -2) (2 0 5) ) '(2 3 4))
; returns #f if systems is incompatible ( has no solutions )
; if system is compatible, returns two-element list,
; which first element is generic private solution vector
; and the second is list of fundamental solutions system vectors
; notice: solution vectors has scheme data type list!

(define (solve matrix free-members)
	(define res (gauss-elim (get-augmented matrix free-members) 0 0 (make-vector (length (car matrix)) -1)))
	(define step-matrix (car res))

	(define answer-rows (cdr res))
	(if (not (solvable? (car res))) #f
		(list
			(vector->list (get-solution step-matrix answer-rows))
			(map vector->list (get-fss step-matrix answer-rows))
		)
	)
)

(define (get-pseudosolutions matrix free-members)
	(define free-members-vec (list->vector free-members))
	(define matrix-vec (list->vector (map list->vector matrix)))
	(define matrix-transposed-vec (transpose matrix-vec))
	(define gram-matrix-vec
		(matrix-product
			matrix-transposed-vec
			(matrix-complex-conjugate matrix-vec)
		)
	)
	(define new-free-members-vec
		(transpose
			(matrix-product
				matrix-transposed-vec
				(transpose
					(build-vector 1 (lambda x free-members-vec))
				)
			)
		)
	)
	(define gram-matrix (vector->list (vector-map vector->list gram-matrix-vec)))
	(define new-free-members (vector->list (vector-ref new-free-members-vec 0)))
	
	(solve gram-matrix new-free-members)
)

(define (cui-show-solution matrix result)
	(let ((variablesCount (length (car matrix)))
		(fss-rank (length (cadr result))))

		(display "LES rank: ")
		(displayln (- variablesCount fss-rank))
		(display "Corresponding HLES FSS rank: ")
		(displayln fss-rank)

		(display "\nPrivate solution: ")
		(displayln (car result))
		
		(display "\nFundamental solutions system of corresponding homogeneous system: \n")
		(displayln (cadr result))
	)
)

(define (cui) ; primitive commandline user interface
	(displayln "Linear Equations Systems solver v0.3 by nm101")
	(displayln "Usage: Submit main matrix as list of lists, each of 'em represents single row")
	(displayln "	Submit free members column as list\n")
	(display "Submit main matrix> ")
	(define matrix (read))
	(display "Submit free members> ")
	(define free-members (read))
	(define result (solve matrix free-members))

	(if (not result)
		(begin
			(displayln "!!! This system has no solutions, yet pseudosolution will be provided")
			(cui-show-solution matrix
				(get-pseudosolutions matrix free-members)
			)
		)
		
		(cui-show-solution matrix result)
	)
)

(cui) ; execute CUI on file call