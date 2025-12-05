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

(define (main lines)
  (local ((define empty-line-pos (index-where lines (lambda (x) (equal? "" x))))
          (define ranges (map (lambda (x) (map string->number x))
                              (map (lambda (x) (string-split x "-"))
                                   (take lines empty-line-pos))))
          (define ids (map string->number (drop lines (add1 empty-line-pos)))))
    (count (lambda (x) (n-in-ranges x ranges)) ids)))


;; -----------------------------
(check-expect (main (file->lines "test.txt")) 3)
(test)
(main (file->lines "input.txt"))
