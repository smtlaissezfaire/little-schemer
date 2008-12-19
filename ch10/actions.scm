(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else
      (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define *lambda
  (lambda (e table)
    (build
     (quote non-primitive)
     (cons table (arguments-and-body-of-function e)))))

(define *cond
  (lambda (e table)
    (evcond (cond-lines-of e) table)))

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (eval-list (arguments-of e) table))))
