;Ordered list representation of a set
;
;(define (adjoin-set x set)
;  (if (element-of-set? x set)
;      (set)
;      (cond ((null? set) (list x))
;            ((< x (car set)) (cons x set))
;            (else (cons (car set) (adjoin-set x (cdr set)))))))
;
;(define (element-of-set? x set)
;  (cond ((null? set) #f)
;        ((= x (car set)) #t)
;        ((< x (car set)) #f)
;        (else (element-of-set? x (cdr set)))))
;
;(define (intersection-set a b)
;  (if (or (null? a) (null? b))
;      ()
;      (cond ((= (car a) (car b))
;             (cons (car a) (intersection-set (cdr a) (cdr b))))
;            ((< (car a) (car b)) (intersection-set (cdr a) b))
;            (else (intersection-set a (cdr b))))))
;Binary tree set

;Selectors:
(define (left-subtree t) (cadr t))
(define (right-subtree t) (caddr t))
(define (element t) (car t))

;Constructor
(define (make-tree e left right)
  (list e left right))

;Predicates
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (element set)) true)
        ((< x (element set)) (element-of-set? x (left-subtree set)))
        (else (element-of-set? x (right-subtree set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x (list) (list)))
        ((= x (element set)) set)
        ((< x (element set))
         (make-tree
           (element set)
           (adjoin-set x (left-subtree set))
           (right-subtree set)))
        (else
         (make-tree
           (element set)
           (left-subtree set)
           (adjoin-set x (right-subtree set))))))
