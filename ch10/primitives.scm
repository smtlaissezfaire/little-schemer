;; call the primitive functions.  Usually implemented
;; in C.  Here we'll rely on the scheme interpreter.
(define apply-primitive
  (lambda (function-name values)
    (cond
     ((eq? function-name (quote cons))
      (cons (first values) (second values)))
     ((eq? function-name (quote car))
      (car (first values) (second values)))
     ((eq? function-name (quote cdr))
      (cdr (first values) (second values)))
     ((eq? function-name (quote eq?))
      (cdr (first values) (second values)))

     ((eq? function-name (quote atom?))
      (:atom? (first values)))

     ((eq? function-name (quote null?))
      (null? (first values)))
     ((eq? function-name (quote zero?))
      (zero? (first values)))
     ((eq? function-name (quote add1))
      (add1 (first values)))
     ((eq? function-name (quote sub1))
      (sub1 (first values)))
     ((eq? function-name (quote number?))
      (number? (first values))))))

;; both primtive and non-primitive function names
;; can be used as atoms
(define :atom?
  (lambda (a)
    (cond
     ((atom? a) #t)
     ((null? a) #f)
     ((eq? (car a) (quote primitive)) #t)
     ((eq? (car a) (quote non-primitive)) #t)
     (else #f))))

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))
