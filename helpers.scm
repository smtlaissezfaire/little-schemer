
(define first
  (lambda (list)
    (car list)))

(define second
  (lambda (list)
    (car (cdr list))))

(define build
  (lambda (a b)
    (cons a
          (cons b
                (cons (quote ()))))))

(define text-of second)

(define initial-table
  (lambda (e)
    (car (quote ()))))

(define arguments-and-body-of-function cdr)
(define table-of                       first)
(define arguments-of                   second)
(define body-of                        third)
(define cond-lines-of                  cdr)
(define function-of                    car)
(define arguments-of                   cdr)

