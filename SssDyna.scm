;; Sub Set Sum problem
;; Dynamic solution
;; Guile compatible

;;  for curly infix notation
;; (read-enable 'curly-infix)

;; ne marche pas:
;; export GUILE_AUTO_COMPILE=0

;; touch file.scm works if you change included files but not source file.scm


;; (load "SssDyna.scm")


(include "../library-FunctProg/first-and-rest.scm")
(include "../library-FunctProg/array.scm")
(include "../library-FunctProg/pair.scm")
(include "../library-FunctProg/number.scm")
(include "../library-FunctProg/list.scm")
(include "../library-FunctProg/let.scm")

;;(define L-init '(1 3 4 16 17 64 256 275 723 889 1040 1041 1093 1111 1284 1344 1520 2027 2734 3000 4285 5027))
(define L-init '(1 3 4 16 17 24 45 64 197 256 275 323 540 723 889 915 1040 1041 1093 1099 1111 1284 1344 1520 2027 2500 2734 3000 3267 3610 4285 5027))
;;(define t-init 19836)
(define t-init 35267)

(define ls (length L-init))

(define dyna (make-array 0 {ls + 1} {t-init + 1}))

(define (one-two b)
  (if b 1 2))

(define cpt 0)



;; scheme@(guile-user)> (ssigma-dyna L-init t-init)
;; $2 = #t
;; scheme@(guile-user)> cpt
;; $3 = 147801
(define (ssigma-dyna L t)

  {cpt <- {cpt + 1}}
  
  ;;(display L) (display " ") (display t) (newline)
  
  (let*  [(ls (length L))
	  (dyn (array-ref dyna ls t))]
    
    ;; dyna[ls][t] means 0: unknown solution, 1: solution found, 2: no solution
    (one?
     (if (not (zero? dyn))
	
	dyn
	
	(array-ref-set! dyna ;; set the array but return the variable
			(one-two
			 (if (null? L)
			     #f
			     (let [ (c (first L))
				    (R (rest L)) ]
			       (cond [ {c = t} #t ] ;; c is the solution
				     [ {c > t} (ssigma-dyna R t) ] ;; c is to big to be a solution but can be an approximation
				     ;; c < t at this point
				     ;; c is part of the solution or his approximation
				     ;; or c is not part of solution or his approximation
				     [ else {(ssigma-dyna R {t - c}) or (ssigma-dyna R t)} ] ))))
			ls
			t  )))))


;; scheme@(guile-user)> (ssigma-dyna-local L-init t-init)
;; $1 = #t
;; scheme@(guile-user)> cpt
;; $2 = 147801
(define (ssigma-dyna-local L t)

  {cpt <- {cpt + 1}}
  
  ;;(display L) (display " ") (display t) (newline)
  
  (local  [ ls (length L)
	    dyn {dyna[ls t]} ]
    
    ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
    (one?
     (if (not (zero? dyn))
	
	dyn

	;; set the array but return the variable
	{ dyna[ls t] <-  (one-two
			  (if (null? L)
			      #f
			      (local [ c (first L)
				       R (rest L) ]
				     (cond [ {c = t} #t ] ;; c is the solution
					   [ {c > t} (ssigma-dyna-local R t) ] ;; c is to big to be a solution but can be an approximation
					   ;; c < t at this point
					   ;; c is part of the solution or his approximation
					   ;; or c is not part of solution or his approximation
					   [ else {(ssigma-dyna-local R {t - c}) or (ssigma-dyna-local R t)} ] )))) } ))))


;; scheme@(guile-user)> (ssigma-proto L-init t-init)
;;  = #t
;; scheme@(guile-user)> cpt
;; $2 = 147801
(define (ssigma-proto L t)

  ;;(display L) (display " ") (display t) (newline)
  (set! cpt {cpt + 1})
  ;; (display L)
  ;; (newline)
  ;; (display t)
  ;; (newline)
  ;; (newline)

  (let*  [(ls (length L))
	  (dyn (array-ref dyna ls t))]
    
    ;; dyna[ls][t] means 0: unknown solution, 1: solution found, 2: no solution

    (cond [(not (zero? dyn)) (one? dyn)]
	  [(null? L) (begin
		       (array-set! dyna 2 ls t)
		       
		       #f)] ;; return #f
	  
	  [else (let [(c (first L))]

		  
		  (if {c = t} ;; c is the solution
		      
		      (begin
			(array-set! dyna 1 ls t)
			#t)  ;; return #t

		      ;; else
		      (let [(R (rest L))]
			
			(if {c > t}   ;; continue searching a solution in the rest
			    
			    (let [(s (ssigma-proto R t))]
			      (array-set! dyna
					  (one-two s)
					  ls t)
			      
			      s) ;; return s
			

			    ;; else
			    ;; c < t at this point
			    ;; c is part of the solution or his approximation
			    ;; or c is not part of solution
			    (let [(s {(ssigma-proto R {t - c}) or (ssigma-proto R t)})]
			      (array-set! dyna (one-two s)
					  ls t)
			      s)))))
		] )))


;; (best-sol 100 '(101) '(90 4 3))
;; (101)

(define (best-sol t L1 L2)
  ;; (display "L1=")
  ;; (display L1)
  ;; (newline)
  ;; (display "L2=")
  ;; (display L2)
  ;; (newline)
  (let [(s1 (apply + L1))
	(s2 (apply + L2))]
    (if {(abs {t - s1}) <= (abs {t - s2})}
	L1
	L2)))

(define (best-sol3 t L1 L2 L3)
  ;; (display "best-sol3") (newline)
  ;; (display "t=") (display t) (newline)
  ;; (display "L1=")
  ;; (display L1)
  ;; (newline)
  ;; (display "L2=")
  ;; (display L2)
  ;; (newline)
  ;; (display "L3=")
  ;; (display L3)
  ;; (newline)
  (let [(L22 (best-sol t L2 L3))]
    (best-sol t L1 L22)))

;; functions below are not good
(define (start-ssigma-sol-approx-dyna L t)
  ;; (display "start-ssigma-sol-approx")
  ;; (newline)
  ;; (display "L=") (display L)
  ;; (newline)
  ;; (display "t=") (display t)
  ;; (newline)
  ;; (newline)
  


  (let [(ls (length L))
  	(dyn (array-ref dyna ls t))]
    (if (not (null? dyn))
  	dyn
	(ssigma-sol-approx-dyna L t '() t '()))))

;; TODO try to get out function constant parameters (if there are)
(define (ssigma-sol-approx-dyna L t S t-init AS) ;; AS:approximative solution

  ;; (display "L=") (display L)
  ;; (newline)
  ;; (display "S=") (display S)
  ;; (newline)
  ;; (display "AS=") (display AS)
  ;; (newline)
  ;; (newline)
  
  (if (null? L)
      
      (begin
	;; (display "null L")
	;; (newline)
	;; (display "S=") (display S)
	;; (newline) 
	;; (display "AS=") (display AS)
	;; (newline)
	;; (display "return best-sol")
	;; (newline)
	(best-sol t-init AS S)) ;; must return S or AS and save it in dyna
      
      (let [ (c (first L))
	     (R (rest L)) ]
	(cond [ {c = t} (best-sol t-init AS (cons c S)) ] ;; c is the solution and save the best in dyna
	      [ {c > t} (ssigma-sol-approx R t S t-init (best-sol t-init
								  AS
								  (list c))) ] ;; c is to big to be a solution but can be an approximation 
	      ;; c < t at this point
	      ;; c is part of the solution or his approximation
	      ;; or c is not part of solution or his approximation
	      [ else (best-sol3 t-init AS
				
				       (begin
					 ;;(display "append c=") (display c) (newline)

					 (append (cons c S)
						 (start-ssigma-sol-approx R {t - c}))) ;; we have to find a solution for t-c now

				       (ssigma-sol-approx R t S t-init AS))])))) ;;  we must save the best in dyna (TODO : where? verify)




