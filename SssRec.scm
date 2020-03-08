;; Sub Set Sum problem
;; Recursive solution
;; Guile compatible

;;  for curly infix notation
;; (read-enable 'curly-infix)

;; export GUILE_AUTO_COMPILE=0

;; (load "SssRec.scm")




(include "../library-FunctProg/first-and-rest.scm")

(define L-init '(1 3 4 16 17 64 256 275 723 889 1040 1041 1093 1111 1284 1344 1520 2027 2734 3000 4285 5027))

(define t-init 19836)

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

(define (ssigma-sol L t S)
  ;; (display L)
  ;; (newline)
  ;; (display t)
  ;; (newline)
  ;; (newline)
  (if (null? L)
      (begin
	(display "null L")
	(newline)
	(display S)
	(newline)
	#f)
      (let [ (c (first L))
	     (R (rest L)) ]
	(cond [ {c = t} (begin
			  (display (cons c S))
			  (newline)
			  #t) ] ;; c is the solution
	      [ {c > t} (ssigma-sol R t S) ]
	      ;; c < t at this point
	      ;; c is part of the solution or his approximation
	      ;; or c is not part of solution
	      [ else {(ssigma-sol R {t - c} (cons c S)) or (ssigma-sol R t S)} ] ))))


(ssigma-sol L-init t-init '())
