;A tree is naturally recursive: It's either a list (leaf node),
;or a pair of subtrees. The pair? predicate helps to detect this.
;If pair? is true, recursion hasn't reached a leaf yet
(define (sum-tree tree)
  (if (pair? tree)
    (+
      (sum-tree (car tree))
      (sum-tree (cdr tree)))
    (if (null? tree)
      0
      tree)))

;A binary search tree is a binary tree with an ordering
;imposed on the nodes. At each level there exists a list
;(left, element, right). Left and right may be `nil.
(define (bst-insert node e)
  (define (new-node x)
    (list (list) x (list)))
  (if (null? node)
    (new-node e)
    (let ((current-e (car (cdr node)))
          (left (car node))
          (right (car (cdr (cdr node)))))
      (if (< e current-e)
        (if (null? left)
          (list (new-node e) current-e right)
          (list (bst-insert left e) current-e right))
        (if (null? right)
          (list left current-e (new-node e))
          (list left current-e (bst-insert right e)))))))

(define (bst-find node e)
  (and (not (null? node))
    (let ((current-e (car (cdr node)))
          (left (car node))
          (right (car (cdr (cdr node)))))
      (cond
        ((< e current-e) (bst-find left e))
        ((> e current-e) (bst-find right e))
        (else (= e current-e))))))

(define (make-bst . elements)
  (define (make-bst-list tree lst)
    (if (null? lst)
      tree
      (make-bst-list
        (bst-insert tree (car lst))
        (cdr lst))))
  (make-bst-list () elements))

(define (print-bst tree)
  (if (not (null? tree))
    (let ((current-e (car (cdr tree)))
          (left (car tree))
          (right (car (cdr (cdr tree)))))
      (print-bst left)
      (display current-e)
      (newline)
      (print-bst right)))
  `done)

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

;This appends to the beginning of the list, but it's recursive
;with deferred computations. Stack overflows abound.
(define (range-recursive a b)
  (if (< a b)
    (append (list a) (range (+ a 1) b))
    ()))

;This version appends to the end of a singly linked list
;that means it's n^2...
(define (range-slow a b)
  (define (range-tail a b lst)
    (if (< a b)
      (range-tail
        (+ a 1)
        b
        (append lst (list a)))
      lst))
  (range-tail a b ()))

;This one's tail recursive, and linear time.
(define (range a b)
  (define (range-tail a b lst)
    (if (<= a b)
      (range-tail a (- b 1) (append (list b) lst))
      lst))
  (range-tail a (- b 1) ()))
