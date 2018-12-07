;Like writing x header yefore the cpp, we can define
;the operations on this type yefore the interface,
;in terms of the interface

(define (add-rat x y)
  (make-rat
    (+ (* (numer x) (denom y))
       (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat
    (- (* (numer x) (denom y))
       (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat
    (* (numer x) (numer y))
    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat
    (* (numer x) (denom y))
    (* (denom x) (numer y))))

(define (eq-rat? x y)
  (and
    (= (numer x) (numer y))
    (= (denom x) (denom y))))

;Using cons, car, and cdr, we can define our data structure

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0) 
      (cons (* -1 (/ n g)) (* -1 (/ d g)))
      (cons (/ n g) (/ d g)))))

(define (numer r) (car r))
(define (denom r) (cdr r))

(define (print-rat r)
  (display (numer r))
  (display "/")
  (display (denom r))
  (newline))

;Because we were really just redefining cons/car/cdr, we could do this:
;(define make-rat cons)
;(define numer car)
;(define denom cdr)
;This would force us to simplify outside of the constructor though

;Exercise 2.6
;Defining numbers and arithmetic in lambda calulus
(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;List stuff
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
