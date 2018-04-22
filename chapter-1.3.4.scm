(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (derivative f)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x)) 
       dx)))

(define (newton-transform f)
  (lambda (x)
    (- x (/ (f x) ((derivative f) x)))))

(define (newtons-method f guess)
  (fixed-point (newton-transform f) guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (dec n)))))

(define (smooth f dx)
  (lambda (x) (/
                (+ (f x) (f (+ x dx)) (f (- x dx)))
                3.0)))
