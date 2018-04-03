(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (cube x) (* x x x))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-integers a b)
  (sum identity a inc b))

(define (compute-pi b)
  (define (pi-term a)
    (/ 1.0 (* a (+ a 2))))
  (define (pi-next a)
    (+ a 4))
  (* 8 (sum pi-term 1 pi-next b)))

; Computes the definite integral using Simpson's method
(define (simpson func a b n)
  (define h (/ (- b a) n) )
  (define (y k)
    (func (+ a (* k h))))
  (define (coefficient k)
    (if (odd? k) 4 2))
  (define (term k)
    (* (coefficient k) (y k)))
  (* (/ h 3.0) (+ (y 0) (sum term 1 inc n))))
