(define (average a b) (/ (+ a b) 2.0))

(define (search-zeroes f neg-point pos-point tolerance)
  (let ((midpoint (average neg-point pos-point)))
    (if (< (abs (- neg-point pos-point)) tolerance)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value)
               (search-zeroes f neg-point midpoint tolerance))
              ((negative? test-value)
               (search-zeroes f midpoint pos-point tolerance))
              (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search-zeroes f a b 0.001))
          ((and (positive? a-value) (negative? b-value))
           (search-zeroes f b a 0.001))
          (else 
            (error "Values must be of opposite sign:" a b)))))

(define (fixed-point f guess)
  (define tolerance 0.0001)
  (define (good-enough? a b)
    (< (abs (- a b)) tolerance))
  (let ((y (f guess)))
    (if (good-enough? guess y)
      guess
      (fixed-point f y))))

(define (square-root x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;Let a = x, b = 1. The fixed point of x -> 1 + 1/x = (x + 1) / x. Substitution yields
;a = (a + b) / a. Since b = 1, a = a / b. This is now the golden ratio: a / b = (a + b) / a
(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(define (fixed-point-display f guess n)
  (define tolerance 0.0001)
  (define (good-enough? a b)
    (< (abs (- a b)) tolerance))
  (let ((y (average guess (f guess))))
    (display "Approximation number ")
    (display n)
    (display ": ")
    (display y)
    (newline)
    (if (good-enough? guess y)
      guess
      (fixed-point-display f y (inc n)))))

