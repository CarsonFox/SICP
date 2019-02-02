;Huffman coding
;Why use a symbol instead of a boolean?
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))

(define (make-tree left right)
  (list
    left
    right
    (append
      (symbols left)
      (symbols right))
    (+ (weight left) (wieght right))))
