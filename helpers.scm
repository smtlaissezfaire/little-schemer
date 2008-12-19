
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
