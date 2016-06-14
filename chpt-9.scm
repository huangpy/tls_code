(load "chpt-4.scm")
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

;;my solution
(define keep-looking
  (lambda (a sorn lat)
    (cond ((eq? a sorn) #t)
	  (else
	   (and (number? sorn)
		(keep-looking a (pick sorn lat) lat))))))

;;book solutin
(define keep-looking
  (lambda (a sorn lat)
    (cond ((number? sorn)
	   (keep-looking a (pick sorn lat) lat))
	  (else (eq? a sorn)))))

(looking 'caviar '(6 2 4 caviar 5 7 3))
(looking 'caviar '(6 2 helo caviar 5 7 3))

(define eternity
  (Lambda (x)
    (eternity x)))

(load "chpt-7.scm")
(define shift
  (lambda (pair)
    (build (first (first pair))
	   (build (second (first pair)) (second pair)))))

(shift '((a b) c))
(shift '((a b) (c d)))

(define align
  (lambda (pora)
    (cond ((atom? pora) pora)
	  ((a-pair? (first pora))
	   (align (shift pora)))
	  (else (build (first pora)
		       (align (second pora)))))))

(align '(a (b c)))

(define length*
  (lambda (pora)
    (cond ((atom? pora) 1)
	  (else
	   (m+ (length* (first pora))
	       (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond ((atom? pora) 1)
	  (else
	   ( m+ (m* (weight* (first pora)) 2)
		(weight* (second pora)))))))

(weight* '(a b))
(weight* '((a b) c))
(weight* '(a (b c)))


(define shuffle
  (lambda (pora)
    (cond ((atom? pora) pora)
	  ((a-pair? (first pora))
	   (shuffle (revpair pora)))
	  (else (build (first pora) (shuffle (second pora)))))))


(define length
  (lambda (l)
    (cond ((null? l) 0)
	  (else ( add1 (length (cdr l)))))))

;;length0
(lambda (l)
    (cond ((null? l) 0)
	  (else (add1 (eternity (cdr l))))))

;;length<=1
(lambda (l)
  (cond ((null? l) 0)
	(else
	 (add1 ((lambda (l)
		  (cond ((null? l) 0)
			(else (add1 (eternity (cdr l))))))
		(cdr l))))))

;;length<=2, just replace eternity in length<=1 with length0
(lambda (l)
  (cond ((null? l) 0)
	(else
	 (add1 ((lambda (l)
		  (cond ((null? l) 0)
			(else (add1 ((lambda (l)
				       (cond ((null? l) 0)
					     (else (add1 (eternity (cdr l))))))(cdr l))))))
		(cdr l))))))


;;using the nineth commanddent: abstract the commmon patterns
;;length0
((lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (add1 (length (cdr l)))))))
 eternity)

;;length<=1
((lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
	    (else (add1 (length (cdr l)))))))
 ((lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (add1 (length (cdr l)))))))
 eternity))


;;length<=2
((lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
	    (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
	    (else (add1 (length (cdr l)))))))
 ((lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (add1 (length (cdr l)))))))
 eternity)))

;;get rid of repetition
;;length0
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (add1 (length (cdr l))))))))

;;length<=1
((lambda (mk-length)
   (mk-length (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (add1 (length (cdr l))))))))

;;length<=2
((lambda (mk-length)
   (mk-length (mk-length (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (add1 (length (cdr l))))))))

;;all use the same name
;;length0
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (add1 (mk-length (cdr l))))))))

;;length<=1
(((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond ((null? l) 0)
	     (else (add1 ((mk-length eternity) (cdr l))))))))
 '(apples))

;;=================================
;;length
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (add1 ((mk-length mk-length) (cdr l))))))))

;;extract (mk-length mk-length) to (length) to make it look like length
;;this has infinite recursion problem
;; ((lambda (mk-length)
;;    (mk-length mk-length))
;;  (lambda (mk-length)
;;    ((lambda (length)
;;       (lambda (l)
;; 	(cond ((null? l) 0)
;; 	      (else (add1 (length (cdr l)))))))
;;     (mk-length mk-length))))

;;fix the infinite recursion problem
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
	(cond ((null? l) 0)
	      (else (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))

;;extract length and gieve it a name:le
((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
	    ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (add1 (length (cdr l))))))))

;;=================================


;;the applicative-order Y-combinator, is the top parter of above procedure
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))


;;normal length using define
(define length
  (lambda (l)
    (cond ((null? l) 0)
	  (else ( add1 (length (cdr l)))))))

;;length use Y-combinator
((Y (lambda (length)
       (lambda (l)
	 (cond ((null? l) 0)
	       (else (add1 (length (cdr l))))))))
 '(1 2 3))

