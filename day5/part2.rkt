;; Solution: sort ranges. merge if in '(a b) '((c d) ...) (b > c). merge-down
;; keep merging up util b < c or b < d. merge-up
;; repeat few times with repeating merge-lines
;; sum all ranges

#lang racket
(require test-engine/racket-tests)
(require math)
(require racket/list/grouping)
(require math/number-theory)


(check-expect (n-in-ranges 1 '((2 4) (0 3))) #t)
(check-expect (n-in-ranges 5 '((2 4) (0 3))) #f)
(check-expect (n-in-ranges -1 '((2 4) (0 3))) #f)
(define (n-in-ranges n ranges)
  (local ((define (n-in-range r)
            (and (>= n (car r)) (<= n (cadr r)))))
    (ormap n-in-range ranges)))

(check-expect (sum-range '(10 20)) 11)
(define (sum-range x)
  (add1 (- (cadr x) (car x))))

(check-expect (merge-down '(1 2) '((3 4))) '((1 2) (3 4)))
(check-expect (merge-down '(1 2) '((2 4))) '((1 4)))
(check-expect (merge-down '(1 10) '((3 4) (5 6) (22 30))) '((1 10) (22 30)))
(define (merge-down x l)
  (cond [(empty? l) `(,x)]
        [else
         (if (<= (caar l) (cadr x))
             (local ((define a (car x))
                     (define b (max (cadr x) (cadar l))))
               (merge-up (list a b) (rest l)))
             (cons x l))]))

(check-expect (merge-up '(1 10) '((3 4) (5 6))) '((1 10)))
(check-expect (merge-up '(1 2) '((3 4) (5 6))) '((1 2) (3 4) (5 6)))
(define (merge-up x l)
  (cond [(empty? l) `(,x)]
        [else
         (if (>= (cadr x) (cadar l)) (merge-up x (rest l))
             (cons x l))]))

(check-expect (merge-lines (merge-lines '((1 10) (1 2) (3 4) (5 24) (22 30)))) '((1 30)))
(define (merge-lines lines)
  (foldr merge-down '() lines))


(define (main lines)
  (local ((define ranges (sort (map (lambda (x) (map string->number x))
                                    (map (lambda (x) (string-split x "-")) lines))
                               (lambda (x y) (< (car x) (car y))))))
    ;; run few merge-lines to remove possible created overlaps
    (sum (map sum-range (merge-lines (merge-lines (merge-lines ranges)))))))


;; -----------------------------
(check-expect (main (file->lines "test2.txt")) 14)
(test)
(main (file->lines "input2.txt"))
