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

(define apply-closure
  (lambda (closure values)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               values)
              (table-of closure)))))
