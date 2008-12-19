
;; helpers
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

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help
     name
     (first entry)
     (second entry)
     entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name)
      (car values))
     (else
      (lookup-in-entry-help
       name
       (cdr names)
       (cdr values)
       entry-f)))))

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else
      (lookup-in-entry
       name
       (car table)
       (lambda (name)
         (lookup-in-entry
          name
          (cdr table)
          table-f)))))))

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

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

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
     (cons table (arguments-and-body-of-function e))))))

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

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (not (primitive? (l)))))

(define apply
  (lambda (function values)
    (cond
     ((primitive? function)
      (apply-primitive (second function)
                       values))
     ((non-primitive? function)
      (apply-closure (second function)
                     values)))))

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

(define apply-closure
  (lambda (closure values)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               values)
              (table-of closure)))))