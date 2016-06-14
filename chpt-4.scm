(load "preface.scm")
;; my solution
(define m+
  (lambda (n m)
    (cond ((zero? n) m)
	  (else (m+ (sub1 n) (add1 m))))))

;;book solution
(define m+
  (lambda (n m)
    (cond ((zero? m) n)
	  (else (add1 (m+ n (sub1 m)))))))

(m+ 2 3)

;;my solution
(define m-
  (lambda (n m)
    (cond ((zero? m) n)
	  (else (m- (sub1 n) (sub1 m))))))

(define m-
  (lambda (n m)
    (cond ((zero? m) n)
	  (else (sub1 (m- n (sub1 m)))))))

(m- 4 2)

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
	  (else (m+ (car tup) (addtup (cdr tup)))))))

(addtup '(2 3 4))

(define m*
  (lambda (n m)
    (cond ((zero? m) 0)
	  (else (m+ n (m* n (sub1 m)))))))

(m* 2 3)

(define tup+
  (lambda (tup1 tup2)
    (cond ((and (null? tup1) (null? tup2)) '())
	  ((null? tup1) tup2)
	  ((null? tup2) tup1)
	  (else (cons (m+ (car tup1) (car tup2))
		      (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(1 2) '(3 4))
(tup+ '(1 2 5) '(3 4))

(define m>
  (lambda (n m)
    (cond ((zero? n ) #f )
	  ((zero? m ) #t )
	  (else (m> (sub1 n) (sub1 m))))))

(m> 2 3)

(define m<
  (lambda (n m)
    (cond ((zero? m) #f)
	  ((zero? n) #t)
	  (else (m< (sub1 n) (sub1 m))))))

(m< 2 3)

;;slution1
(define m=
  (lambda (n m)
    (cond ((zero? m) (zero? n))
	  ((zero? n) #f)
	  (else (m= (sub1 n) (sub1 m))))))
(zero? 1)

;;solution2
(define m=
  (lambda (n m)
    (cond ((or (m> n m) (m< n m)) #f)
	  (else #t))))

(m= 2 3)
(m= 2 2)

(define **
  (lambda (n m)
    (cond ((m= m 0) 1)
	  (else (m* n (** n (sub1 m)))))))

(** 2 3)
(** 2 0)

(define m÷
  (lambda (n m)
    (cond ((< n m) 0)
	  (else (add1 (m÷ (m- n m) m))))))

(m÷ 2 2)
(m÷ 4 2)
(m÷ 7 2)

(define length
  (lambda (lat)
    (cond ((null? lat) 0)
	  (else (add1 (length (cdr lat)))))))

(length '(1 2 3))

(define pick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (car lat))
	  (else (pick (sub1 n) (cdr lat))))))

(pick 2 '(1 3 5 7))

(define rempick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (cdr lat))
	  (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 2 '(1 3 4 5))

(define no-nums
  (lambda (lat)
    (cond ((null? lat) '())
	  ((number? (car lat)) (no-nums (cdr lat)))
	  (else (cons (car lat) (no-nums (cdr lat)))))))

(no-nums '(a 1 b 2 c))

(define all-nums
  (lambda (lat)
    (cond ((null? lat) '())
	  ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
	  (else (all-nums (cdr lat))))))

(all-nums '(a 1 b 2 c))

(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2))
	   (m= a1 a2))
	  ((or (number? a1) (number? a2))
	   #f)
	  (else (eq? a1 a2)))))

(eqan? 1 1)
(eqan? 1 'a)
(eqan? 'a 'a)
(eq? 1 1)
(eq? 1 'a)
(eq? 'a 'a)

(define occur
  (lambda (a lat)
    (cond ((null? lat) 0)
	  ((eqan? a (car lat))
	   (add1 (occur a (cdr lat))))
	  (else (occur a (cdr lat))))))

(occur 'a '(a b a c d a 1 ))

(define one?
  (lambda (n)
    (m= n 1)))

(one? 1)
(one? 2)

(define rempick
  (lambda (n lat)
    (cond ((one? n) (cdr lat))
	  (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 2 '(1 3 5))


