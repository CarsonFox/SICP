(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (identity x) x)

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

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

(define (product-recursive term a next b)
  (if (> a b)
    1
    (* (term a)
       (product-recursive term (next a) next b))))

(define (factorial-recursive n)
    (product-recursive identity 2 inc n))

(define (product-iter term a next b p)
  (if (> a b)
    p
    (product-iter term (next a) next b (* p (term a)))))

(define (factorial-iter n)
  (product-iter identity 2 inc n 1))

(define (accumulate-r combiner identity term a next b)
  (if (> a b)
    identity
    (combiner (term a) (accumulate-r combiner identity term (next a) next b))))

(define (sum-acc term a next b)
  (accumulate-iter + 0 term a next b))

; Don't have to provide the starting value, since it should just be the same as identity
(define (accumulate-iter combiner identity term a next b)
  (define (helper a b acc)
    (if (> a b)
      identity
      (helper (next a) b (combiner acc (term a)))))
  (helper a b identity))


(define (sum-acc-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (integral f a b dx)
  (* (sum f (+ a (/ dx 2.0)) (lambda (x) (+ x dx)) b) dx))

(define (wallis-pi n)
  (define (prev-even x)
    (if (even? x) x (- x 1)))
  (define (prev-odd x)
    (if (odd? x) x (- x 1)))
  (* 4 (/
         (product-iter prev-even 3 inc n)
         (product-iter prev-odd 3 inc n))))
