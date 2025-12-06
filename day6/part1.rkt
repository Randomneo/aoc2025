#lang racket
(require test-engine/racket-tests)
(require math)
(require racket/list/grouping)
(require math/number-theory)

(define (transpose matrix)
  (apply map list matrix))

(check-expect (opp '(1 2 3 4) "*") (* 1 2 3 4))
(check-expect (opp '(1 2 3 4) "+") (+ 1 2 3 4))
(define (opp numbers op)
  (eval `(,(op->proc op) ,@numbers)))

(define (op->proc op)
  (cond [(equal? op "*") *]
        [(equal? op "+") +]))

(define (process-lines numbers ops)
  (cond [(empty? numbers) '()]
        [else
         (cons (opp (car numbers) (car ops)) (process-lines (cdr numbers) (cdr ops)))]))

(define (main lines)
  (local ((define (splitter x) (string-split x " "))
          (define numbers (map (lambda (x) (filter number? x))
                               (map (lambda (x) (map string->number x))
                                    (map splitter
                                         (drop-right lines 1)))))
          (define ops (filter (lambda (x) (not (equal? "" x))) (splitter (last lines)))))
    (sum (process-lines (transpose numbers) ops))))



;; -----------------------------
(check-expect (main (file->lines "test.txt")) 4277556)
(test)
(main (file->lines "input.txt"))
