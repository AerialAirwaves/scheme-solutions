#lang scheme
; Scheme_1 laboratory work tasks solution
; Laboratory work timestamp: Thu, Oct 11 2019
; Done by Melnikov Sergei, group MT-102 / MEH-190102

; Task 1
(define (distance a x y)
	(define dX (if ( > (- (abs x) a) 0) (- (abs x) a) 0))
	(define dY (if ( > (- (abs y) a) 0) (- (abs y) a) 0))
	(define result
		(if (>= y 0)
			(- (sqrt (+ (* x x) (* y y))) a) 
			(sqrt (+ (* dX dX ) (* dY dY) ))
		)
	)
	(if (> result 0) result 0)
)

; Task 2
(define (angle h m)
	(define out (abs (-
		(* 30 (+ h (/ m 60)))
		(* 6 m)
	)))
; show the shortest angle representation value
	(if (> out 180) (- 360 out) out)
)

; Task 3
(define (n2l n)
	(define des (floor (/ n 10)))
	(define edn (remainder n 10))
	(cond
		( (= n 11) (display "одиннадцать" ))
		( (= n 12) (display "двенадцать" ))
		( (= n 13) (display "тринадцать" ))
		( (= n 14) (display "четырнадцать" ))
		( (= n 15) (display "пятнадцать" ))
		( (= n 16) (display "шестнадцать" ))
		( (= n 17) (display "семнадцать" ))
		( (= n 18) (display "восемнадцать" ))
		( (= n 19) (display "девятнадцать" ))
		(else (

				(cond
					( (= des 2) (display "двадцать ") )
					( (= des 3) (display "тридцать ") )
					( (= des 4) (display "сорок ") )
					( (= des 5) (display "пятьдесят ") )
					( (= des 6) (display "шесть ") )
					( (= des 7) (display "семьдесят ") )
					( (= des 8) (display "восемьдесят ") )
					( (= des 9) (display "девяносто ") )
				)
				(cond
					( (= edn 1) (display "один") )
					( (= edn 2) (display "два") )
					( (= edn 3) (display "три") )
					( (= edn 4) (display "четыре") )
					( (= edn 5) (display "пять") )
					( (= edn 6) (display "шесть") )
					( (= edn 7) (display "семь") )
					( (= edn 8) (display "восемь") )
					( (= edn 9) (display "девять") )
				)

		))
	)
)

; Task 4
(define (elephantandpawn sx sy px py)
	(if (= (remainder (+ (remainder sx 2) (remainder sy 2)) 2) (remainder (+ (remainder px 2) (remainder py 2)) 2))
		(if (= (abs (- sx px)) (abs (- sy py)) ) 1 2)
		-1 ; if colors of squares, where figures placed, are different, mission impossible, returning error code
	)
)

; Task 5

(define (kingandpawn kx ky px py)
	(and (not (= py 8))
		(or
			(and (>= ky py) (<= (- ky py) 1) (<= (abs (- px kx)) 1) )
			(and (<= ky py) (<= (max (abs (- kx px)) (abs (- ky py)) ) (- py 1) ))
		)
	)
)

; math model description:

; if playing according to chess rules, the first move is belongs to white figure - white king
; pawn's move direction is from 8th to 1st line (according to explanation, given by our examiner)
; black pawn cannot be placed at 8th line (according to explanation, given by our examiner)
; if that not happened, two success (white king can beat black pawn) cases are possible:
; 1) if white king is behind black pawn move direction or black pawn at 1st line and king is able to attack it by his first move in both cases
; then pawn's Y >= king's Y and coordinates difference not exceeds 1
; 2) if white king is ahead of black pawn's move direction or they at the same line and king is able to reach and attack pawn before pawns first move as queen
; then pawn's Y - 1 >= maximal coordinates difference between figures coordinates
; 1 is subtracted from pawn's distance to 1st line 'cuz pawn can pass two cells by its first turn
