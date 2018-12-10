(define (last-pair lst)
  (if (null? (cdr lst))
    (car lst)
    (last-pair (cdr lst))))

(define (reverse lst)
  (if (null? (cdr lst))
    lst
    (append (reverse (cdr lst)) (list (car lst)))))

(define (same-parity x . y)
  (define (same? a b)
    (if (odd? a)
      (odd? b)
      (even? b)))
  (define (same-parity-lst x args)
    (let ((lst
            (if (same? x (car args))
              (list (car args))
              ())))
      (if (null? (cdr args))
        lst
        (append lst (same-parity-lst x (cdr args))))))
  (same-parity-lst x y))

(define (my-for-each proc items)
  (if (null? items)
    #t
    ((proc (car items))
     (my-for-each proc (cdr items)))))
