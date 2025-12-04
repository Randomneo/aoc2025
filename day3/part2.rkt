;;; Solution take first 12 digits. Iterate over rest. On each digit remove one that less than next
;;; one from current list, and append new digit.

#lang racket
(require test-engine/racket-tests)
(require math)
(require racket/list/grouping)
(require math/number-theory)

(check-expect (number->digits 123) '(1 2 3))
(define (number->digits num)
  (cond [(equal? 0 num) '()]
        [else `(,@(number->digits (quotient num 10)) ,(modulo num 10))]))

(check-expect (digits->number '(1 2 3)) 123)
(define (digits->number digits)
  (foldl (lambda (x num) (+ (* num 10) x)) 0 digits))


(check-expect (drop-digit '(1 2 3)) '(2 3))
(check-expect (drop-digit '(3 2 1)) '(3 2))
(check-expect (drop-digit '(4 2 3 1)) '(4 3 1))
(check-expect (drop-digit '(1)) '())
(check-expect (drop-digit '(1 2)) '(2))
(define (drop-digit digits)
  (cond [(empty? (cdr digits)) '()]
        [else (if (< (car digits) (cadr digits))
                  (cdr digits)
                  (cons (car digits) (drop-digit (cdr digits))))]))

(check-expect (bank-biggest (number->digits 987654321111111) 12) 987654321111)
(check-expect (bank-biggest (number->digits 811111111111119) 12) 811111111119)
(check-expect (bank-biggest (number->digits 234234234234278) 12) 434234234278)
(check-expect (bank-biggest (number->digits 818181911112111) 12) 888911112111)
(check-expect (bank-biggest (number->digits 818181911122111) 12) 888911122111)
(check-expect (bank-biggest (number->digits 9876512341234) 5) 98765)
(define (bank-biggest bank size)
  (local ((define (next-digits next current)
            (drop-digit `(,@current ,next))))
    (digits->number (foldl next-digits (take bank size) (list-tail bank size)))))



(define (main banks)
  (sum (map (lambda (x) (bank-biggest x 12))
            (map number->digits (map string->number banks)))))

;; ------------------------
(check-expect (main '("987654321111111" "811111111111119" "234234234234278" "818181911112111"))
              3121910778619)
(test)
(main (file->lines "input.txt"))
