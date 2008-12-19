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

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (e)
    (car (quote ()))))

(define *lambda
  (lambda (e table)
    (build
     (quote non-primitive)
     (cons table (arguments-and-body-of-function e)))))

(define arguments-and-body-of-function cdr)
(define table-of                       first)
(define arguments-of                   second)
(define body-of                        third)

(define evcond
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines))
               table))
     ((meaning (question-of (car lines))  table)
      (meaning (answer-of   (car lines))) table)
     (else
      (evcond
       (cdr lines)
       table)))))

(define else?
  (lambda (x)
    (cond
     ((and (atom? x) (eq? x (quote else))) #t)
     (else #f))))

(define question-of first)
(define answer-of   second)

(define *cond
  (lambda (e table)
    (evcond (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define eval-list
  (lambda (args table)
    (cond
     ((null? args) (quote ()))
     (else
      (cons (meaning (car args) table)
            (eval-list (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (eval-list (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

