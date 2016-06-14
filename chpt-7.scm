(load "chpt-5.scm")
;;member is in chpt-5.scm using equal?
(define set?
  (lambda (lat)
    (cond ((null? lat) #t)
	  ((member? (car lat) (cdr lat)) #f)
	  (else (set? (cdr lat))))))


(set? '(apple 3 pear 4 9 apple 3 4))
(set? '(apple 3 pear 4 9))

(define makeset
  (lambda (lat)
    (cond ((null? lat) '())
	  ((member? (car lat) (cdr lat))
	   (makeset (cdr lat)))
	  (else (cons (car lat) (makeset (cdr lat)))))))

(makeset '(apple peach pear peach plum apple lemon peach))

;;define makeset using multirember in chpt-3.scm
(load "chpt-3.scm")
;;my solution
(define makeset
  (lambda (lat)
    (cond ((null? lat) '())
	  ((member? (car lat) (cdr lat))
	   (cons (car lat) (multirember (car lat) (makeset (cdr lat)))))
	  (else (cons (car lat) (makeset (cdr lat)))))))

;;book solution
(define makeset
  (lambda (lat)
    (cond ((null? lat) '())
	  (else (cons (car lat)
		      (makeset (multirember (car lat) (cdr lat))))))))

(makeset '(apple peach pear peach plum apple lemon peach))
(makeset '(apple 3 pear 4 9 apple 3 4))

(define subset?
  (lambda (set1 set2)
    (cond ((null? set1) #t)
	  (else (and (member? (car set1) set2)
		     (subset? (cdr set1) set2))))))

(subset? '(5 chicken wings)
	 '(5 hamburgers 2 pieces fried chicken and light duckling wings))
(subset? '(4 pounds of horseradish)
	 '(four pounds chicken and 5 ounces horseradish))

;;mu solution
(define eqset?
  (lambda (set1 set2)
    (cond ((and (null? set1) (null? set2)) #t)
	  ((or (null? set1) (null? set2)) #f)
	  (else (and  (member? (car set1) set2)
		      (eqset? (cdr set1) (rember (car set1) set2)))))))

;;book solutoin, so smart...
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
	 (subset? set2 set1))))

(eqset? '(6 large chickens with wings)
	'(6 chickens with large wings))

(define intersect?
  (lambda (set1 set2)
    (cond ((null? set1) #f)
	  (else (or ((member? (car set1) set2) #t)
		    (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond ((null? set1) '())
	  ((member? (car set1) set2)
	   (cons (car set1) (intersect (cdr set1) set2)))
	  (else (intersect (cdr set1) set2)))))

(intersect '(stewed tomatoes and macaroni)
	   '(macaroni and cheese))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
	  ((member? (car set1) set2)
	   (union (cdr set1) set2))
	  (else (cons (car set1) (union (cdr set1) set2))))))

(union '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))

(define difference
  (lambda (set1 set2)
    (cond ((null? set1) '())
	  ((member? (car set1) set2)
	   (difference (cdr set1) set2))
	  (else (cons (car set1) (difference (cdr set1) set2))))))

(difference '(stewed tomatoes and macaroni casserole)
	    '(macaroni and cheese))


(define intersectall
  (lambda (l-set)
    (cond ((null? (cdr l-set)) (car l-set))
	  (else (intersect (car l-set)
			   (intersectall (cdr l-set)))))))

(intersectall '( ( a b c) ( c a d e) ( e f g h a b)))
(intersectall '( ( 6 pears and)
		 (3 peaches and 6 peppers)
		 (8 pears and 6 plums)
		 (and 6 prunes with some apples)))

(define a-pair?
  (lambda (x)
    (cond ((atom? x) #f)
	  ((null? x) #f)
	  (((null?) (cdr x)) #f)
	  ((null? (cddr x)) #t))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (cadr p)))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define third
  (lambda (l)
    (caddr l)))

;;rel stands for a set of pairs
(load "chpt-3.scm")
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))

(define revrel
  (lambda (rel)
    (cond ((null? rel) '())
	  (else (cons (build (second (car rel)) (first (car rel)))
		      (revrel (cdr rel)))))))

(revrel '((8 a) (pumpkin pie) (got sick)))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair) )))

;;rewrite revrel using revpair
(define revrel
  (lambda (rel)
    (cond ((null? rel) '())
	  (else (cons (revpair (car rel))
		      (revrel (cdr rel)))))))

(revrel '((8 a) (pumpkin pie) (got sick)))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define seconds
  (lambda (l)
    (cond ((null? l) '())
	  (else (cons (cadar l) (seconds (cdr l)))))))

(seconds '((8 a) (pumpkin pie) (got sick)))
(fullfun? '((8 a) (pumpkin pie) (got sick)))
(fullfun? '((grape raisin) (plum prune) (stewed prune)))

;;fullfun? is one-to-one?
(define one-to-one?
  (lambda (fun)
    (fun (revrel fun))))


