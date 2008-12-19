(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
     ((or
       (number? e)
       (eq? e #t)
       (eq? e #f)
       (eq? e (quote cons))
       (eq? e (quote car))
       (eq? e (quote cdr))
       (eq? e (quote null?))
       (eq? e (quote eq?))
       (eq? e (quote atom?))
       (eq? e (quote zero?))
       (eq? e (quote add1))
       (eq? e (quote sub1))
       (eq? e (quote number?)))
      *const)
     (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) (quote quote))  *quote)
       ((eq? (car e) (quote lambda)) *lambda)
       ((eq? (car e) (quote cond))   *cond)
       (else *applicaion)))
     (else *applicaion))))

(define eval-list
  (lambda (args table)
    (cond
     ((null? args) (quote ()))
     (else
      (cons (meaning (car args) table)
            (eval-list (cdr args) table))))))

