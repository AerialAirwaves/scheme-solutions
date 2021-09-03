#lang scheme

; division with int result function definition (a: real, b:int), all real values, including output, will be rounded via floor func
(define (// a b) (floor (/ (- a (remainder (floor a) b)) b)))

; destination task function itself
(define (clk h m)
	(define H (remainder h 12)) ; cut extra 12 hours in case of someone type hour in 24h format
	(remainder 
			(// ; integer dividing result by 11 at the last step for more accuracy
				(-
					(* 3600 12) ; single arrows meet period * 11

					(remainder ; calculating how many seconds left until both arrows meet

						(* 11 (+ (* 3600 H) (* 60 m))) ; current 12h time in seconds * 11
						(* 3600 12) ; single arrows meet period * 11
					)
				)
				11
			) 
		3927 ; cut whole wait period if we right at it
	)
	
)
; that feeling, when 3 years of python coding literally forces you to write easy readable any complex code even on such a masochist programming language as Scheme

; fancy commandline user interaction
(display "Current hour> ")
(define h (read))
(display "Current minute> ")
(define m (read))
(define result (clk h m) )

(display "[Result] Seconds till both arrows meet: ")
(displayln result)

(display ".. same, but with a bit fancy format: ")
(display (// result 3600))
(display ":")
(display (// (remainder result 3600) 60))
(display ":")
(display (remainder result 60))