;; Sub Set Sum problem
;; Exponential solution
;; Guile compatible

;;  for curly infix notation
;; (read-enable 'curly-infix)
;; set GUILE_AUTO_COMPILE=0
;; or:
;; export GUILE_AUTO_COMPILE=0

;; (load "SssExpo.scm")

;; chicken scheme:
;;(define path-to-library "../library-FunctProg/")

;;(define library-file (string-append path-to-library "first-and-rest.scm"))

;;(load-relative library-file)

;; for remove
(use-modules (srfi srfi-1))

;; for time
(use-modules (srfi srfi-19))

(include "../library-FunctProg/first-and-rest.scm")

(define L-init '(1 3 4 16 17 64 256 275 723 889 1040 1041 1093 1111 1284 1344 1520 2027 2734 3000 4285 5027))

(define t 19836)

(define (fusion L1 L2)
  (merge L1 L2 <))

(define (addN lst n)
  (map (lambda (k) {k + n} ) lst))

(define p (lambda(k) { k > t }))

;; accumulator acc-L0 updates like this:
;; (0)
;; (0 1 )
;; (0 1 3 4 )
;; (0 1 3 4 4 5 7 8 )
;; (0 1 3 4 4 5 7 8 16 17 19 20 20 21 23 24 )
;; (0 1 3 4 4 5 7 8 16 17 17 18 19 20 20 20 21 21 21 22 23 24 24 25 33 34 36 37 37 38 40 41 )
(define (sss-expo acc-L0 L)
  (if (null? L)	
       acc-L0
       (sss-expo (remove p ;; remove all number > t
			 (fusion acc-L0 ;; merge accumulator list with 
				 (addN acc-L0 (first L)))) ;; list composed of itself + first number of current L
		 (rest L))))


;; variante
(define (sss-expo2 acc-L0 L)
  (cond [(null? L) (apply max acc-L0)]
	[(member t acc-L0) t]
	[else (sss-expo2 (remove p
				 (fusion acc-L0
					 (addN acc-L0 (first L))))
			 (rest L))]
	))



;;; note: source file /home/mattei/Dropbox/git/scheme4algo/SssExpo.scm
;;;       newer than compiled /home/mattei/.cache/guile/ccache/2.2-LE-8-3.A/home/mattei/Dropbox/git/scheme4algo/SssExpo.scm.go
;;$7 = 19836


;; return max of acc-L0 because it is the nearest number solution

(define t1 (current-time))

(display (apply max (sss-expo '(0) L-init)))
(newline)

(define t2 (current-time))

(display (time-difference t2 t1))
(newline)



(define t3 (current-time))

(display (sss-expo2 '(0) L-init))
(newline)

(define t4 (current-time))

(time-difference t4 t3)
