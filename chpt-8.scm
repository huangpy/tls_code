(load "chpt-4.scm")
(define rember-f
  (lambda (test? a f)
    (cond ((null? f) '())
	  ((test? a (car f)) (cdr f))
	  (else (cons (car f) (rember-f test? a (cdr f)))))))

(rember-f eq? 'jelly '(jelly beans are good))
(rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))
(rember-f eq? 5 '(6 2 5 3))

(define rember-f
  (lambda (test?)
    (lambda (a f)
      (cond ((null? f) '())
	    ((test? a (car f)) (cdr f))
	    (else (cons (car f) ((rember-f test?) a (cdr f))))))))

(define rember-eq? (rember-f eq?))

(rember-eq? 'jelly '(jelly beans are good))

((rember-f eq?) 'jelly '(jelly beans are good))

(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond ((null? lat) '())
	    ((test? old (car lat))
	     (cons new (cons old (cdr lat))))
	    (else (cons (car lat)
			((insertL-f test?) new old (cdr lat)) ))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) (quote ()))
	    ((test? (car l) old)
	     (cons old (cons new (cdr l))))
	    (else (cons (car l)
			((insertR-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond ((null? l) (quote ()))
	    ((eq? (car l) old)
	     (seq new old (cdr l)))
	    (else (cons (car l)
			((insert-g seq) new old (cdr l))))))))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

(insertL 'z 'd '(a b c d e))

(define insertL (insert-g (lambda (new old l)
			      (cons new (cons old l)))))

(insertL 'z 'd '(a b c d e))

;;define subst using insert-g
(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))

(define subst (insert-g (lambda (new old l)
			  (cons new l))))

(subst 'e 'd '(a b c d f))

;;define rember using insert-g
(define seqrem
  (lambda (new old l) l))

(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(rember 'a '(b a c))


(load "chpt-6.scm")
;;simplify value using abstractoin common patterns with a new function
;;my solutoin
(define calcu
  (lambda (opera)
    (lambda (f 1st 2st)
      (opera (f 1st) (f 2st)))))

(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
	  ((eq? (operator nexp) '+)
	   ((calcu m+) value (1st-sub-exp nexp) (2st-sub-exp nexp)))
	  ((eq? (operator nexp) '*)
	   ((calcu m*) value (1st-sub-exp nexp) (2st-sub-exp nexp)))
	  ((eq? (operator nexp) **)
	   ((calcu **) value (1st-sub-exp nexp) (2st-sub-exp nexp))))))

(value '(2 + 3))

;;my solution only abstract part, we can also abstract the (eq? .. ..) part!
(define atom-to-function
  (lambda (x)
    (cond ((eq? x '+) m+)
	  ((eq? x '*) m*)
	  ((eq? x '**) **))))

(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
	  (else ((atom-to-function (operator nexp))
		 (value (1st-sub-exp nexp)) (value (2st-sub-exp nexp)))))))

(value '(2 * 3))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) '())
	    ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
	    (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))

(define multirember-eq? (multirember-f eq?))

;;combine test? and a
(define eq?-c
  (lambda (a)
    (lambda (x) (eq? x a))))

(define eq?-tuna
  (eq?-c 'tuna))

;;my solutoin
(define test?-eq-tuna
  (lambda (a b)
    (eq?-tuna b)))

(define multirember-tuna
  (lambda (lat)
    ((multirember-f test?-eq-tuna) #f lat)))

(multirember-tuna '(a b tuna c d tuna e f))

;;book solution
(define multiremberT
  (lambda (test? lat)
    (cond ((null? lat) '())
	  ((test? (car lat)) (multiremberT test? (cdr lat)))
	  (else (cons (car lat) (multiremberT test? (cdr lat)))))))

(multiremberT eq?-tuna '(a b tuna c d tuna e f))

(define multirember-tuna
  (lambda (lat)
    (multiremberT eq?-tuna lat)))

(multirember-tuna '(a b tuna c d tuna e f))

(define multirember&co
  (lambda (a lat col)
    (cond ((null? lat)
	   (col '() '()))
	  ((eq? (car lat) a)
	   (multirember&co
	    a
	    (cdr lat)
	    (lambda (newlat seen)
	      (col newlat
		   (cons (car lat) seen)))))
	  (else (multirember&co
		 a
		 (cdr lat)
		 (lambda (newlat seen)
		   (col (cons (car lat) newlat)
			seen)))))))
(define a-friend
  (lambda (x y)
    (null? y)))

(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)
(multirember&co 'tuna '(and tuna) a-friend)

(load "chpt-4.scm")
(define last-friend
  (lambda (x y)
    (length x)))

(multirember&co 'tuna '(strawberries tuna and swordfish) last-friend)


;;combine multiinsertL and multiinsertR
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) '())
	  ((eq? (car lat) oldL)
	   (cons new (cons oldL
			   (multiinsertLR new oldL oldR (cdr lat)))))
	  ((eq? (car lat) oldR)
	   (cons oldR (cons new
			    (multiinsertLR new oldL oldR (cdr lat)))))
	  (else (cons (car lat)
		      (multiinsertLR new oldL oldR (cdr lat)))))))

(multiinsertLR 'z 'a 'b '(1 a 2 b 3 4))

;;multiinsertLR&co is to multiinsertLR what multirember&co is to multirember
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat)
	   (col '() 0 0))
	  ((eq? (car lat) oldL)
	   (multiinsertLR&co
	    new oldL oldR (cdr lat)
	    (lambda (newlat L R)
	      (col (cons new (cons oldL newlat))
		   (add1 L)
		   R))))
	  ((eq? (car lat) oldR)
	   (multiinsertLR&co
	    new oldL oldR (cdr lat)
	    (lambda (newlat L R)
	      (col (cons oldR (cons new newlat))
		   L
		   (add1 R)))))
	  (else (multiinsertLR&co
		 new oldL oldR (cdr lat)
		 (lambda (newlat L R)
		   (col (cons (car lat) newlat)
			L
			R)))))))

(define display-LR
  (lambda (lat L R)
    (display lat)
    (newline)
    (display L)
    (newline)
    (display R)))

(multiinsertLR&co 'z 'a 'b '(1 a 2 b 3 4) display-LR)

;;write evens-only*
(define even?
  (lambda (n)
    (m= (m* (m÷ n 2) 2) n)))

(even? 4)
(even? 5)

(define evens-only*
  (lambda (l)
    (cond ((null? l) '())
	  ((atom? (car l))
	   (cond ((even? (car l))
		  (cons (car l) (evens-only* (cdr l))))
		 (else (evens-only* (cdr l)))))
	  (else (cons (evens-only* (car l))
		      (evens-only* (cdr l)))))))

(evens-only* '((9 1 2 8) 3 10 ((9 9 ) 7 6) 2))

(define evens-only*&co
  (lambda (l col)
    (cond ((null? l) (col '() 1 0))
	  ((atom? (car l))
	   (cond ((even? (car l))
		  (evens-only*&co
		   (cdr l)
		   (lambda (newl p s)
		     (col (cons (car l) newl)
			  (m* (car l) p)
			  s))))
		 (else
		  (evens-only*&co
		   (cdr l)
		   (lambda (newl p s)
		     (col newl
			  p
			  (m+ (car l) s)))))))
	  (else
	   (evens-only*&co
	    (car l)
	    (lambda (al ap as)
	      (evens-only*&co
	       (cdr l)
	       (lambda (dl dp ds)
		 (col (cons al dl)
		      (m* ap dp)
		      (m+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))

(evens-only*&co '(( 9 1 2 8) 3 10 (( 9 9 ) 7 6) 2)
		the-last-friend)

