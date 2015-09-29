;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixing The World - Unit Tests    			     ;;
;; 25/3/15                                                   ;;
;; < Alan Berman - BRMALA003 >                               ;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;-------------------HELPER FUNCTIONS----------------------
;; ;print function for debugging purposes
(define (print . args)
  (cond ((not (null? args))
        (display (car args))
        (apply print (cdr args)))
  )
)

;; ;gets nth index of 0-indexed list. Can use list-ref instead
(define (index lst idx)
    (if (null? lst)
        lst
        (if (= idx 0)
            (car lst)
            (index (cdr lst) (- idx 1))
        )
    )
)	
;; ;TESTS
;; ; (print (= 1 (index '(1 2 3 4 5) 0)) "\n")
;; ; (print (= 4 (index '(1 2 3 4 5) 3)) "\n")
;; ; (print (not (= 1 (index '(1 2 3 4 5) 2))) "\n")
;; ; (print (not (= 0 (index '(1 2 3 4 5) 0))) "\n")

;; ;checks if an item is in a list
;; You might want to do a more efficient version of this.
;;
(define (in item lst)
    (if (null? lst)
        #f
        (if (equal? item (car lst))
            #t
            (in item (cdr lst))
        )
    )
)
;; ;TESTS
;; ; (print (in 1 '(1 2 3)) "\n")
;; ; (print (in 2 '(1 2 3)) "\n")
;; ; (print (not (in 4 '(1 2 3))) "\n")
;; ; (print (in '(1 2) '((1 2) (3 4) 5)) "\n")

;; ;helper function for finding the length of a list
(define (lengthHelper n lst)
    (if (null? lst)
        n
        (lengthHelper (+ n 1) (cdr lst))
    )
)

;; ;finds length of a list
(define (length lst)
    (lengthHelper 0 lst)
)
;; ;TESTS
;; ; (print (= 4 (length '(1 2 3 4))) "\n")
;; ; (print (= 1 (length '(1))) "\n")
;; ; (print (= 2 (length '((1 2) (3 4)))) "\n")
;; ; (print (not (= 4 (length '(1 2 3 4 5)))) "\n")
;; ;-----------------------------------------------------------


;---------------------SOLVED STATES------------------------
;solved states of a 2x2x2 rubiks cube
(define solvedStates
    '(  ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
        ((3 1) (1 1) (4 1) (2 1) (7 3) (5 3) (8 3) (6 3))
        ((4 1) (3 1) (2 1) (1 1) (8 3) (7 3) (6 3) (5 3))
        ((2 1) (4 1) (1 1) (3 1) (6 3) (8 3) (5 3) (7 3))

        ((5 5) (1 6) (7 5) (3 6) (6 5) (2 6) (8 5) (4 6))
        ((7 5) (3 6) (8 5) (4 6) (5 5) (1 6) (6 5) (2 6))
        ((8 5) (4 6) (6 5) (2 6) (7 5) (3 6) (5 5) (1 6))
        ((6 5) (2 6) (5 5) (1 6) (8 5) (4 6) (7 5) (3 6))

        ((2 5) (6 6) (4 5) (8 6) (1 5) (5 6) (3 5) (7 6))
        ((4 5) (8 6) (3 5) (7 6) (2 5) (6 6) (1 5) (5 6))
        ((3 5) (7 6) (1 5) (5 6) (4 5) (8 6) (2 5) (6 6))
        ((1 5) (5 6) (2 5) (6 6) (3 5) (7 6) (4 5) (8 6))

        ((7 1) (8 1) (5 1) (6 1) (3 3) (4 3) (1 3) (2 3))
        ((5 1) (7 1) (6 1) (8 1) (1 3) (3 3) (2 3) (4 3))
        ((6 1) (5 1) (8 1) (7 1) (2 3) (1 3) (4 3) (3 3))
        ((8 1) (6 1) (7 1) (5 1) (4 3) (2 3) (3 3) (1 3))

        ((3 2) (4 2) (7 4) (8 4) (1 2) (2 2) (5 4) (6 4))
        ((1 2) (3 2) (5 4) (7 4) (2 2) (4 2) (6 4) (8 4))
        ((2 2) (1 2) (6 4) (5 4) (4 2) (3 2) (8 4) (7 4))
        ((4 2) (2 2) (8 4) (6 4) (3 2) (1 2) (7 4) (5 4))

        ((5 2) (6 2) (1 4) (2 4) (7 2) (8 2) (3 4) (4 4))
        ((7 2) (5 2) (3 4) (1 4) (8 2) (6 2) (4 4) (2 4))
        ((8 2) (7 2) (4 4) (3 4) (6 2) (5 2) (2 4) (1 4))
        ((6 2) (8 2) (2 4) (4 4) (5 2) (7 2) (1 4) (3 4))
    )
)
;; ;-----------------------------------------------------


;; ;---------------------QUESTION 1.1-----------------------
;; ;helper function for rotating the cube. Recalculates the various orientations
;; ;of the sub-cubes
(define (recalculateOrientation orientation axis)
    (cond
        [(= axis 0)
            (if (> orientation 4)
                orientation
                (if(= orientation 4)
                    1
                    (+ orientation 1)
                )
            )
        ]
        [(= axis 1)
            (if (or (= orientation 1) (= orientation 3))
                orientation
                (cond
                    [(= orientation 2) 6]
                    [(= orientation 4) 5]
                    [(= orientation 5) 2]
                    [(= orientation 6) 4]
                )
            )
        ]
        [(= axis 2)
            (if (or (= orientation 2) (= orientation 4))
                orientation
                (cond
                    [(= orientation 1) 5]
                    [(= orientation 3) 6]
                    [(= orientation 5) 3]
                    [(= orientation 6) 1]
                )
            )
        ]
	;To facilitate rotateX with ispositive false
        [(= axis 3)
            (if (or (= orientation 5) (= orientation 6))
                orientation
                (cond
                    [(= orientation 1) 4]
                    [(= orientation 2) 1]
                    [(= orientation 3) 2]
                    [(= orientation 4) 3]
                )
            )
        ]
	;To facilitate rotateY with ispositive false
        [(= axis 4)
            (if (or (= orientation 1) (= orientation 3))
                orientation
                (cond
                    [(= orientation 2) 5]
                    [(= orientation 4) 6]
                    [(= orientation 5) 4]
                    [(= orientation 6) 2]
                )
            )
        ]
	;To facilitate rotateZ with ispositive false
        [(= axis 5)
            (if (or (= orientation 2) (= orientation 4))
                orientation
                (cond
                    [(= orientation 1) 6]
                    [(= orientation 3) 5]
                    [(= orientation 5) 1]
                    [(= orientation 6) 3]
                )
            )
        ]
        
    )
)
;; ;TESTS
;; ; (print (= 2 (recalculateOrientation 1 0)) "\n")
;; ; (print (= 5 (recalculateOrientation 5 0)) "\n")
;; ; (print (= 1 (recalculateOrientation 1 1)) "\n")
;; ; (print (= 6 (recalculateOrientation 2 1)) "\n")
;; ; (print (= 5 (recalculateOrientation 1 2)) "\n")
;; ; (print (= 2 (recalculateOrientation 2 2)) "\n")

;rotations are performed using the left hand rule
;rotates left 4 cubes along x axis
(define (rotateX ispositive state)
    ;"x" rotation
    (if ispositive
    	(list 
		(list 
    			(list (car (index state 4)) (recalculateOrientation (car (cdr (index state 4))) 0))
    			(index state 1)
        		(list (car (index state 0)) (recalculateOrientation (car (cdr (index state 0))) 0))
        		(index state 3)
       			(list (car (index state 6)) (recalculateOrientation (car (cdr (index state 6))) 0))
    			(index state 5)
    			(list (car (index state 2)) (recalculateOrientation (car (cdr (index state 2))) 0))
    			(index state 7)
		)
    	(list "x")
	)
    	
    	;"X" rotation
    	(list
		(list 
    			(list (car (index state 2)) (recalculateOrientation (car (cdr (index state 2))) 3))
     			(index state 1)
    			(list (car (index state 6)) (recalculateOrientation (car (cdr (index state 6))) 3))
   			(index state 3)
        		(list (car (index state 0)) (recalculateOrientation (car (cdr (index state 0))) 3))
    			(index state 5)
    			(list (car (index state 4)) (recalculateOrientation (car (cdr (index state 4))) 3))
    			(index state 7)
		)
    	(list "X")
	)
))
;rotates bottom 4 cubes along y axis
(define (rotateY ispositive state)
   ;"y" rotation
  (if ispositive
	(list
		 (list
    			(index state 0)
			(index state 1)
    			(index state 2)
    			(index state 3)
    			(list (car (index state 5)) (recalculateOrientation (car (cdr (index state 5))) 1))
			(list (car (index state 7)) (recalculateOrientation (car (cdr (index state 7))) 1))
			(list (car (index state 4)) (recalculateOrientation (car (cdr (index state 4))) 1))
			(list (car (index state 6)) (recalculateOrientation (car (cdr (index state 6))) 1))
		)
	(list "y")
	)
	
	;"Y" rotation
	(list 
		(list 
    			(index state 0)
			(index state 1)
    			(index state 2)
    			(index state 3)
    			(list (car (index state 6)) (recalculateOrientation (car (cdr (index state 6))) 4))
			(list (car (index state 4)) (recalculateOrientation (car (cdr (index state 4))) 4))
			(list (car (index state 7)) (recalculateOrientation (car (cdr (index state 7))) 4))
			(list (car (index state 5)) (recalculateOrientation (car (cdr (index state 5))) 4))
		)
	(list "Y")
	)
))
;rotates back 4 cubes along z axis
(define (rotateZ ispositive state)
    ;"z" rotation
    (if ispositive
	(list 
		(list 
    			(list (car (index state 1)) (recalculateOrientation (car (cdr (index state 1))) 2))
			(list (car (index state 5)) (recalculateOrientation (car (cdr (index state 5))) 2))
			(index state 2)
			(index state 3)
			(list (car (index state 0)) (recalculateOrientation (car (cdr (index state 0))) 2))
			(list (car (index state 4)) (recalculateOrientation (car (cdr (index state 4))) 2))
			(index state 6)
			(index state 7)
		)
	(list "z")
	)
	
	;"Z" rotation
	(list 
		(list 
    			(list (car (index state 4)) (recalculateOrientation (car (cdr (index state 4))) 5))
			(list (car (index state 0)) (recalculateOrientation (car (cdr (index state 0))) 5))
			(index state 2)
			(index state 3)
			(list (car (index state 5)) (recalculateOrientation (car (cdr (index state 5))) 5))
			(list (car (index state 1)) (recalculateOrientation (car (cdr (index state 1))) 5))
			(index state 6)
			(index state 7)
		)
	(list "Z")
	)
))
;; ;helper for rotate function
(define (rotateHelper char state)
    (cond
        [(char=? char #\x) (car (rotateX #t state))]
        [(char=? char #\X) (car (rotateX #f state))]
        [(char=? char #\y) (car (rotateY #t state))]
        [(char=? char #\Y) (car (rotateY #f state))]
        [(char=? char #\z) (car (rotateZ #t state))]
        [(char=? char #\Z) (car (rotateZ #f state))]
    )
)

;; ;parses a string for rotations
(define (rotate rotations state)
    (if (= (string-length rotations) 0)
        state
        (rotate (substring rotations 1 (string-length rotations)) (rotateHelper (string-ref rotations 0) state))
    )
)
;; ;TESTS
;; ; (print (equal? (rotate "x" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))) "\n")
;; ; (print (equal? (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateZ #t (car (rotateY #t (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (equal? (rotate "xXx" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #t (car (rotateX #f (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (equal? (rotate "yXz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateZ #t (car (rotateX #f (car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (not (equal? (rotate "xXy" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #f (car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)))))))) "\n")
;; ;------------------------------------------------------------

;-----------------------QUESTION 1.2-----------------------
;generates the successor states of the current given rubiks cube state
(define (generateSuccessorStates state prevMoves) 
    (list
        (list 
	    
	    (car (rotateX #t state ))
            (car (rotateX #f state ))
            (car (rotateY #t state ))
            (car (rotateY #f state ))
            (car (rotateZ #t state ))
            (car (rotateZ #f state ))
        )
        (list (append prevMoves '("x")) (append prevMoves '("X")) (append prevMoves '("y"))
        (append prevMoves '("Y"))  (append prevMoves '("z")) (append prevMoves '("Z")))
    )
	
)

;; ;TESTS
;; ; (print (equal? (generateSuccessorStates '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
;; ;         (list
;; ;             (list
;; ;                 (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateX #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateY #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateZ #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;             )
;; ;             '(("x") ("X") ("y") ("Y") ("z") ("Z"))
;; ;         )
;; ;     )
;; ; "\n")


;-----------------------------QUESTION 2.1--------------------------
;extracts all states from a list
;by recursively extracting the car of the car of a list
;i.e. the first state in the list of states
(define (extractStates alist)
	(if (null? alist)
		'()
		(append (car (car  alist)) (extractStates (cdr alist))) 
	)
)

;extracts all histories from a list
;by recursively extracting the car of the cdr of a list
;i.e. the first history in the list of histories
(define (extractHistories alist)
	(if (null? alist)
	'()
	(append (car (cdr (car alist))) (extractHistories (cdr alist)))
	)
)

;helper method that runs generateSucessorStates on all items in a list
;and then using extractStates and extractHistories returns  a sorted list
;of the list of states and a list of their respecitve histories
(define (genStatesHelper stateList moves )
	(append 
		(list (extractStates(map generateSuccessorStates stateList moves)))
		(list (extractHistories (map generateSuccessorStates stateList moves)))
	)
)
;finds all the states at a specific depth
;using genStatesHelper and generateSuccessorStates
(define (genStates n state  moves)

	(cond 
	[(= n 0)  (list (list state) (list moves))]
	[(= n 1)  (generateSuccessorStates state moves)] 
	[(>= n 2) (genStatesHelper
 			(car (genStates (- n 1) state moves))
			(cadr (genStates (- n 1) state moves))
	)]
	)
)

;---------------------------QUESTION 3.1-----------------------
;Solves a rubiks cube using breadth first search. Can solve up to roughly 7 moves.
;(define (solveCubeHelper solved aList)
;  (if (null? aList)
; '(("?"))
;    (if (in (car(car aList)) solved )
;        (car (cadr aList))
;            (solveCubeHelper solved 
;                 (list (cdr (car aList)) 
;                        (cdr (cadr aList))))   ;        )))))
;    )
;  )
;)




(define (solveCubeHelper solved aList)
  (if (or (null? aList) (null? (car aList)))
 '(("?"))
    (if (in (car(car aList)) solved )
        (car (cadr aList))
            (solveCubeHelper solved 
                 (list (cdr (car aList)) 
                        (cdr (cadr aList))))   ;        )))))
    )
  )
)

(define (isNZero n)
(if (= n 0)
(+ n 1)
n)
)

(define (solveCube solved initial n)
(let ([ans (solveCubeHelper solved (genStates (isNZero n) initial '()))])
   
   
    (if (and (equal? ans '(("?"))) (< n 8))
        (solveCubeHelper solved (genStates (+ (isNZero n) 1) initial '()))
        ans
    )

)
)

;; ;TESTS
;recalculateOrientation

;Taken from skeleton assignment3.scm
;rotateX,rotateY,rotateZ
;Should all return #t
(print (equal? (rotate "x" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))) "\n")
(print (equal? (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateZ #t (car (rotateY #t (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
(print (equal? (rotate "xXx" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #t (car (rotateX #f (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
(print (equal? (rotate "yXz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateZ #t (car (rotateX #f (car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
(print (not (equal? (rotate "xXy" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #f (car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)))))))) "\n")

;generateSucessorStates test
;Taken from skeleton assignment3.scm
(print (equal? (generateSuccessorStates '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
(list
(list
(car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
(car (rotateX #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
(car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
(car (rotateY #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
(car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
(car (rotateZ #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
)
'(("x") ("X") ("y") ("Y") ("z") ("Z"))
)
)
"\n")

;solveCube tests
;(thus also genStates tests)

;solveCube test for 1 move needed
;Should be ("x"), as to undo 3 x's, just rotate 'x'
;Thus this should return #t
(print (equal?(solveCube solvedStates (rotate "xxx" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0) '("x"))  "\n")

;Should be ("X"), as to undo a 'x' rotation
;rotate back in the opposite, negative way ('X')
;Thus this should return #t
(solveCube solvedStates (rotate "x" '((5 2) (6 2) (1 4) (2 4) (7 2) (8 2) (3 4) (4 4)))
0)

;solveCube tests for 2 moves needed
;Should be ("Y" "X"), as to undo a 'x' rotation followed
; by a 'y'rotation, rotate back by their opposites
;(negative twists, or the capital "X" and "Y" rotations)
;in the reverse order
;Thus this should return #t
(print (equal?(solveCube solvedStates (rotate "xy" '((5 2) (6 2) (1 4) (2 4) (7 2) (8 2) (3 4) (4 4)))
0) '("Y" "X")) "\n")

;Should be ("x" "x"), as to undo 2 'x' rotations 
;rotate a further 2 'x' rotations to effectively
;not rotate the cube (and return to the original state)
;Thus this should return #t
(print (equal?(solveCube solvedStates (rotate "xx" '((5 2) (6 2) (1 4) (2 4) (7 2) (8 2) (3 4) (4 4)))
0) '("x" "x")) "\n")

;solveCube test for 3 moves needed
;Should be ("z" "x" "y"), as to undo a 'Y' rotation followed
; by a 'X' and 'Z'  rotation, rotate back by their opposites
;(positive twists, or the lowercase "X", "Y" and "Z" rotations)
;in the reverse order
;Thus this should return #t
;Note: This returns #t on other interpreters (e.g. repl.it) 
;but returns #f on gambit v4.2.8
(print (equal?(solveCube solvedStates (rotate "YXZ" '((3 2) (4 2) (7 4) (8 4) (1 2) (2 2) (5 4) (6 4)))
0) '("z" "x" "y")) "\n")


