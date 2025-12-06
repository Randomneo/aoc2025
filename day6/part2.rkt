#lang racket
(require test-engine/racket-tests)
(require math)
(require racket/list/grouping)
(require math/number-theory)

(define (transpose matrix)
  (apply map list matrix))

(define (add-spaces line dest-len)
  (append line (make-list dest-len #\space)))

(define (op->proc op)
  (cond [(equal? op "*") *]
        [(equal? op "+") +]))

(check-expect (make-groups '("123 " " 23" "12 *" "  1" "22  +") '())
              '(("*" "123 " " 23" "12 ") ("+" "  1" "22  ")))
(define (make-groups lines acc)
  (cond [(empty? lines) '()]
        [else (local ((define cur-line (car lines))
                      (define rest-lines (cdr lines)))
                (if (or (string-suffix? cur-line "*") (string-suffix? cur-line "+"))
                    (local ((define chars (string->list cur-line))
                            (define number (list->string (drop-right chars 1)))
                            (define op (list->string (list (last chars)))))
                      (cons `(,op ,@acc ,number) (make-groups rest-lines '())))
                    (make-groups rest-lines `(,@acc ,cur-line))))]))

(check-expect (process-group '("*" "123 " " 23" "12 ")) (* 123 23 12))
(check-expect (process-group '("+" "  1" "22  ")) (+ 1 22))
(define (process-group group)
  (local ((define op (op->proc (car group)))
          (define numbers (map string->number (map string-trim (cdr group)))))
    (eval `(,op ,@numbers))))


(define (main lines)
  (local ((define string-lines (map string->list lines))
          (define aligned (map (lambda (x) (add-spaces x (- (length (argmax length string-lines)) (length x))))
                               string-lines)))
    (sum (map process-group
              (make-groups (filter (lambda (x) (not (equal? (string-trim x) "")))
                                   (reverse (map list->string (transpose aligned))))
                           '())))))



;; -----------------------------
(check-expect (main (file->lines "test.txt")) 3263827)
(test)
(main (file->lines "input.txt"))
