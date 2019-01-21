;Symbolic differentiation of an expression
;with respect to some variable.
;Assumes the existence of many predicates, accessors and
;constructors, which will need to be implemented.
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknown expression type -- DERIV" exp))))

;Using symbol? builtin
(define (variable? x) (symbol? x))

;Using eq? symbol equality builtin
(define (same-variable? a b)
  (and
    (variable? a)
    (variable? b)
    (eq? a b)))

;Assume we only want to ask about literals
(define (=number? exp number)
  (and (number? exp) (= exp number)))

;a1 and a2 will be variables, whose values are symbols.
(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        (else (list `+ a1 a2))))

(define (make-product a1 a2)
  (cond ((and (number? a1) (number? a2)) (* a1 a2))
        ((=number? a1 0) 0)
        ((=number? a1 1) a2)
        ((=number? a2 0) 0)
        ((=number? a2 1) a1)
        (else (list `* a1 a2))))

(define (sum? x)
  (and
    (pair? x)
    (eq? `+ (car x))))

;Selectors using the cadr and caddr procedures:
;portmanteaus of (car (cdr x)) and (car (cdr (cdr x))).
;Seems like there are lots of these, but not infinite.
(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and
    (pair? x)
    (eq? `* (car x))))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))

(define p1 `(+ x 3))
(define p3 '(* (* x y) (+ x 3)))
