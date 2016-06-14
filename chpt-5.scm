(load "chpt-4.scm")
(load "chpt-2.scm")
(define rember*
  (lambda (a l)
    (cond ((null? l) '())
	  ((atom? (car l))
	   (cond ((eq? a (car l)) (rember* a (cdr l)))
		 (else (cons (car l) (rember* a (cdr l))))))
	  (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))

(define insertR*
  (lambda (new old l)
    (cond ((null? l) '())
	  ((atom? (car l))
	   (cond ((eq? old (car l))
		  (cons old (cons new
				  (insertR* new old (cdr l)))))
		 (else (cons (car l) (insertR* new old (cdr l))))))
	  (else (cons (insertR* new old (car l))
		      (insertR* new old (cdr l)))))))

(insertR* 'roast 'chuck
	  '((how much (wood))
	    could ((a (wood) chuck))
	    (((chuck)))
	    (if (a) ((wood chuck))) could chuck wood))

(define occur*
  (lambda (a l)
    (cond ((null? l) 0)
	  ((atom? (car l))
	   (cond ((eq? a (car l)) (add1 (occur* a (cdr l))))
		 (else (occur* a (cdr l)))))
	  (else (m+ (occur* a (car l))
		    (occur* a (cdr l)))))))

(occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))

(define subst*
  (lambda (new old l)
    (cond ((null? l) '())
	  ((atom? (car l))
	   (cond ((eq? old (car l)) (cons new (subst* new old (cdr l))))
		 (else (cons (car l) (subst* new old (cdr l))))))
	  (else (cons (subst* new old (car l))
		      (subst* new old (cdr l)))))))

(subst* 'orange 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))

(define insertL*
  (lambda (new old l)
    (cond ((null? l) '())
	  ((atom? (car l))
	   (cond ((eq? old (car l))
		  (cons new (cons old (insertL* new old (cdr l)))))
		 (else (cons (car l) (insertL* new old (cdr l))))))
	  (else (cons (insertL* new old (car l))
		      (insertL* new old (cdr l)))))))

(insertL* 'pecker 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))

(define member*
  (lambda (a l)
    (cond ((null? l) #f)
	  ((atom? (car l))
	   (or (eq? a (car l))
	       (member* a (cdr l))))
	  (else (or (member* a (car l))
		    (member* a (cdr l)))))))

(member* 'chips
	 '((potato) (chips ((with) fish) (chips))))

(define leftmost
  (lambda (l)
    (cond ((atom? (car l)) (car l))
	  (else (leftmost (car l))))))

(leftmost '((potato) (chips ((with) fish) (chips))))
(leftmost '(((hot) (tuna (and))) cheese))

(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
	  ((or (null? l1) (null? l2)) #f)
	  ((and (atom? (car l1)) (atom? (car l2)))
	   (and (eq? (car l1) (car l2))
		(eqlist? (cdr l1) (cdr l2))))
	  ((or (atom? (car l1)) (atom? (car l2)))
	   #f)
	  (else (and (eqlist? (car l1) (car l2))
		     (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? '( strawberry ice cream ) '( strawberry ice cream ))
(eqlist? '( strawberry ice cream ) '(strawberry cream ice))
(eqlist? '(banana ((split))) '((banana) (split)))
(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))))
(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))

(define equal?
  (lambda (s1 s2)
    (cond ((and (atom? s1) (atom? s2))
	   (eq? s1 s2))
	  ((atom? s1) #f)
	  ((atom? s2) #f)
	  (else (eqlist? s1 s2)))))

;;simplify
(define equal?
  (lambda (s1 s2)
    (cond ((and (atom? s1) (atom? s2))
	   (eq? s1 s2))
	  ((or (atom? s1) (atom? s2))
	   #f)
	  (else (eqlist? s1 s2)))))

;;simplify eqlist? using equal?
(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
	  ((or (null? l1) (null? l2)) #f)
	  (else (and (equal? (car l1) (car l2))
		     (eqlist? (cdr l1) (cdr l2)))))))

;;Here is rember after we replace lat by a list l of S-expressions and a by any S-expression.
(define rember
  (lambda (s l)
    (cond ((null? l) '())
	  ((atom? (car l))
	   (cond ((equal? s (car l)) (cdr l))
		 (else (cons (car l) (rember s (cdr l))))))
	  (else (cond ((equal? s (car l)) (cdr l))
		      (else (cons (car l) (rember s (cdr l)))))))))

;;simplify above
(define rember
  (lambda (s l)
    (cond ((null? l) '())
	  (else (cond ((equal? s (car l)) (cdr l))
		      (else (cons (car l) (rember s (cdr l)))))))))

;;further simplified which the inner ( cond . . . ) asks questions that the outer ( cond . . . ) could ask!
(define rember
  (lambda (s l)
    (cond ((null? l) '())
	  ((equal? s (car l)) (cdr l))
	  (else (cons (car l) (rember s (cdr l)))))))

;;define member? in chpt-2.scm using equal?
(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
	  (else (or (equal? a (car lat))
		     (member? a (cdr lat)))))))

(member? 'apple '(1 apple 2))
(member? 'meal '(1 apple 2))

;;rewrite insertL using equal? instead of eq?
(define insertL
  (lambda (new old lat)
    (cond ((null? lat) '())
	  ((equal? old (car lat))
	   (cons new (cons old (cdr lat))))
	  (else (cons (car lat)
		      (insertL new old (cdr lat)))))))
