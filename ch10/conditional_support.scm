
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
