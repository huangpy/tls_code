(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define zero?
  (lambda (n)
    (= n 0)))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

