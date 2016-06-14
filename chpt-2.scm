(load "preface.scm")

(define lat?
  (lambda (l)
    (cond ((null? l) true)
	  ((atom? (car lat?)) (lat? (cdr lat)))
	  (else #f))))



;;my solution
(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
	  ((eq? a (car lat)) #t)
	  (else (member? a (cdr lat))))))

;;book solution
(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
	  (else (or (eq? a (car lat))
		     (member? a (cdr lat)))))))

