#lang racket
(require test-engine/racket-tests)
(require math)
(require racket/list/grouping)
(require math/number-theory)

(check-expect (number->digits 123) '(1 2 3))
(define (number->digits num)
  (cond [(equal? 0 num) '()]
        [else `(,@(number->digits (quotient num 10)) ,(modulo num 10))]))

(check-expect (bank-biggest (number->digits 818181911112111)) '(9 2))
(check-expect (bank-biggest (number->digits 987654321111111)) '(9 8))
(check-expect (bank-biggest (number->digits 811111111111119)) '(8 9))
(check-expect (bank-biggest (number->digits 234234234234278)) '(7 8))
(define (bank-biggest bank)
  (local ((define (mknum n1 n2) (+ (* 10 n1) n2)))
    (cond [(empty? (cddr bank)) bank]
          [else (local ((define biggest (bank-biggest (cdr bank)))
                        (define n1 (car biggest))
                        (define n2 (cadr biggest))
                        (define biggest-num (mknum n1 n2))
                        (define cand1 (mknum (car bank) n1))
                        (define cand2 (mknum (car bank) n2))
                        (define res1 `(,(car bank) ,n1))
                        (define res2 `(,(car bank) ,n2)))
                  (cond [(and (> cand1 biggest-num) (> cand1 cand2) res1)]
                        [(> cand2 biggest-num) res2]
                        [else biggest]))])))



(define (main banks)
  (sum (map (lambda (x) (+ (* (car x) 10) (cadr x)))
            (map bank-biggest
                 (map number->digits (map string->number banks))))))

;; ------------------------
(check-expect (main '("987654321111111" "811111111111119" "234234234234278" "818181911112111"))
              357)
(main (file->lines "input.txt"))
(test)
