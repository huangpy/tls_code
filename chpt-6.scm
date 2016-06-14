(load "chpt-4.scm")
(define numbered?
  (lambda (aexp)
    (cond ((atom? aexp) (number? exp))
	  (else (and (number? (car aexp))
		     (number? (caddr aexp)))))))

;;represent like (1 + 2)
(define value
  (lambda (nexp) ;;number exp
    (cond ((atom? nexp) nexp)
	  ((eq? (cadr nexp) '+)
	   (m+ (value (car nexp)) (value (caddr nexp))))
	  ((eq? (cadr nexp) '*)
	   (m* (value (car nexp)) (value (caddr nexp))))
	  ((eq? (cadr nexp) '**)
	   (** (value (car nexp)) (value (caddr nexp)))))))

(value '(1 + 2))
(value '(2 * 3))
(value '(3 ** 3))

;;reprsent like (+ 1 2)
(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
	  ((eq? (car nexp) '+)
	   (m+ (value (cadr nexp)) (value (caddr nexp))))
	  ((eq? (car nexp) '*)
	   (m* (value (cadr nexp)) (value (caddr nexp))))
	  ((eq? (car nexp) '**)
	   (** (value (cadr nexp)) (value (caddr nexp)))))))

(value '(+ 1 2))
(value '(* 2 3))
(value '(** 3 3))

(define 1st-sub-exp
  (lambda (aexp)
    (cadr aexp)))

(define 2st-sub-exp
  (lambda (aexp)
    (caddr aexp)))

(define operator
  (lambda (aexp)
    (car aexp)))

;;write (+ 1 2) representation again in terms of 1st-sub-exp 2st-sub-exp operator
(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
	  ((eq? (operator nexp) '+)
	   (m+ (value (1st-sub-exp nexp)) (value (2st-sub-exp nexp))))
	  ((eq? (operator nexp) '*)
	   (m* (value (1st-sub-exp nexp)) (value (2st-sub-exp nexp))))
	  ((eq? (operator nexp) **)
	   (** (value (1st-sub-exp nexp)) (value (2st-sub-exp nexp)))))))

;;write (1 + 2) representation again in terms of 1st-sub-exp 2st-sub-exp operator
(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define 2st-sub-exp
  (lambda (aexp)
    (caddr aexp)))

(define operator
  (lambda (aexp)
    (cadr aexp)))


;;How is one represented? (()). How is two represented? (() ()).
(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define n+
  (lambda (n m)
    (cond ((sero? m) n)
	  (else (edd1 (n+ n (sub1 m)))))))
