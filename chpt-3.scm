(load "chpt-2.scm")
(define (rember a lat)
  (cond ((null? lat) '())
	((eq? a (car lat)) (cdr lat))
	(else (cons (car lat) (rember a (cdr lat))))))

(rember 2 '(1 2 3 4 2 5))


(define firsts
  (lambda (l)
    (cond ((null? l) '())
	  (else (cons (caar l) (firsts (cdr l)))))))

(firsts '((1 2) (3 4)))
(firsts '(((1 2) 3) (4 5)))


(define insertR
  (lambda (new old lat)
    (cond ((null? lat) '())
	  ((eq? old (car lat))
	   (cons old (cons new (cdr lat))))
	  (else (cons (car lat)
		      (insertR new old (cdr lat)))))))

(insertR 'e 'd '(a b c d f))

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) '())
	  ((eq? old (car lat))
	   (cons new (cons old (cdr lat))))
	  (else (cons (car lat)
		      (insertL new old (cdr lat)))))))

(insertL 'e 'd '(a b c d f))


(define subst
  (lambda (new old lat)
    (cond ((null? lat) '())
	  ((eq? old (car lat))
	   (cons new (cdr lat)))
	  (else (cons (car lat)
		      (subst new old (cdr lat)))))))

(subst 'e 'd '(a b c d f))

;;my solution
(define subst2
  (lambda (new o1 o2 lat)
    (subst new o2 (subst new o1 lat))))

(subst2 'e 'b 'd '(a b c d f))

;;book solution
(define subst2
  (lambda (new o1 o2 lat)
    (cond ((null? lat) '())
	  ((or (eq? o1 (car lat))
	       (eq? o2 (car lat)))
	   (cons new (cdr lat)))
	  (else (cons (car lat)
		      (subst2 new o1 o2 (cdr lat)))))))


(subst2 'e 'b 'd '(a b c d f))

(define (multirember a lat)
  (cond ((null? lat) '())
	((eq? a (car lat)) (multirember a (cdr lat)))
	(else (cons (car lat) (multirember a (cdr lat))))))
(multirember 2 '(1 2 3 4 2 5))

;;rewrite multirember using euqal? instead of eq?
(load "chpt-5.scm")
(define (multirember a lat)
  (cond ((null? lat) '())
	((equal? a (car lat)) (multirember a (cdr lat)))
	(else (cons (car lat) (multirember a (cdr lat))))))
(multirember 2 '(1 2 3 4 2 5))




(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) '())
	  ((eq? old (car lat))
	   (cons old (cons new 
			   (multiinsertR new old (cdr lat)))))
	  (else (cons (car lat)
		      (multiinsertR new old (cdr lat)))))))

(multiinsertR 'z 'd '(a b c d f g d h))

(define multiinsertL
  (lambda (new old lat)
    (cond ((null? lat) '())
	  ((eq? old (car lat))
	   (cons new
		 (cons old
		       (multiinsertL new old (cdr lat)))))
	  (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(multiinsertL 'z 'd '(a b c d f g d h))
