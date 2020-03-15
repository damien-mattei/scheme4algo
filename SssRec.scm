;; Sub Set Sum problem
;; Recursive solution
;; Guile compatible

;;  for curly infix notation
;; (read-enable 'curly-infix)

;; export GUILE_AUTO_COMPILE=0

;; (load "SssRec.scm")




(include "../library-FunctProg/first-and-rest.scm")

(define L-init '(1 3 4 16 17 64 256 275 723 889 1040 1041 1093 1111 1284 1344 1520 2027 2734 3000 4285 5027))

;;(define t-init 19836)

(define (ssigma L t)
  ;; (display L)
  ;; (newline)
  ;; (display t)
  ;; (newline)
  ;; (newline)
  (if (null? L)
      #f
      (let [ (c (first L))
	     (R (rest L)) ]
	(cond [ {c = t} #t ] ;; c is the solution
	      [ {c > t} (ssigma R t) ]
	      ;; c < t at this point
	      ;; c is part of the solution or his approximation
	      ;; or c is not part of solution
	      [ else {(ssigma R {t - c}) or (ssigma R t)} ] ))))



;; (ssigma-sol L-init 603 '())
;; (275 256 64 4 3 1)


;;(ssigma-sol L-init 601 '())
;; #f

;; (ssigma-sol L-init t-init '())
;; (5027 4285 3000 1520 1344 1284 1041 1040 723 275 256 17 16 4 3 1)


(define (ssigma-sol L t S)

  (if (null? L)
      (begin
	(display "null L")
	(newline)
	(display S)
	(newline)
	#f)
      (let [ (c (first L))
	     (R (rest L)) ]
	(cond [ {c = t} (cons c S) ] ;; c is the solution
	      [ {c > t} (ssigma-sol R t S) ] ;; c is to big to be a solution but can be an approximation
	      ;; c < t at this point
	      ;; c is part of the solution or his approximation
	      ;; or c is not part of solution or his approximation
	      [ else {(ssigma-sol R {t - c} (cons c S)) or (ssigma-sol R t S)} ] ))))


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



;; (start-ssigma-sol-approx '(1 3 10) 5)
;; (1 3)
;; (start-ssigma-sol-approx '(1 3 10) 12)
;; (1 10)

;;(start-ssigma-sol-approx L-init 19836) 
;;(1 3 4 16 17 256 275 723 1040 1041 1284 1344 1520 3000 4285 5027)

;; (start-ssigma-sol-approx L-init 603) 
;; (1 3 4 64 256 275)

;; scheme@(guile-user)>  (start-ssigma-sol-approx L-init 601) 
;; $7 = (1 4 64 256 275)
;; scheme@(guile-user)> (+ 1 4 64 256 275)
;; $8 = 600

(define (start-ssigma-sol-approx L t)
  ;; (display "start-ssigma-sol-approx")
  ;; (newline)
  ;; (display "L=") (display L)
  ;; (newline)
  ;; (display "t=") (display t)
  ;; (newline)
  ;; (newline)
  ;;(if (null? L)
   ;;   L
   ;;   (ssigma-sol-approx L t '() t (list (first L)))))
    (ssigma-sol-approx L t '() t '()))


(define (ssigma-sol-approx L t S t-init AS) ;; AS:approximative solution

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
	(best-sol t-init AS S)) ;; must return S or AS
      
      (let [ (c (first L))
	     (R (rest L)) ]
	(cond [ {c = t} (best-sol t-init AS (cons c S)) ] ;; c is the solution
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

				       (ssigma-sol-approx R t S t-init AS))]))))



