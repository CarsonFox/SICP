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

;Selectors
(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (symbol-leaf tree)
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;Now for the actual decoder
(define (decode bits tree)
  (define (decode-recursive bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch
                  (car bits)
                  current-branch)))
          (if (leaf? next-branch)
              (cons
                (symbol-leaf next-branch)
                (decode-recursive (cdr bits) tree))
              (decode-recursive (cdr bits) next-branch)))))
  (decode-recursive bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))
