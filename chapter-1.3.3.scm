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
