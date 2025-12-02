#lang racket
(require test-engine/racket-tests)
(require math)
(require racket/list/grouping)
(require math/number-theory)


(check-expect (ranges 11 22) '(11 22))
(check-expect (ranges 10 20) '(11))
(check-expect (ranges 1000 1234) '(1010 1111 1212))
(check-expect (ranges 1023 1234) '(1111 1212))
(check-expect (ranges 1000 1200) '(1010 1111))
(check-expect (ranges 100 120) '(111))
(check-expect (ranges 121210 121220) '(121212))

(define (ranges from to)
  (filter elf? (build-list (add1 (- to from)) (lambda (x) (+ x from)))))

(define (elf? num)
  (elf-chars? (string->list (number->string num))))

(define (elf-chars? chars)
  (ormap list-equal (sublists chars)))

(check-expect (list-equal '((1 2) (1 2) (1 2))) #t)
(check-expect (list-equal '((1 2 3) (1 2 3) (1 2 3))) #t)
(check-expect (list-equal '((1 2) (2 3))) #f)
(define (list-equal l)
  (andmap (lambda (x) (equal? (car x) (cadr x))) (windows 2 1 l)))

(define (range-to-list r)
  (local ((define r-pair (string-split r "-"))
          (define from (max (string->number (car r-pair)) 10))
          (define to (string->number (cadr r-pair))))
    (ranges from to)))

(define (string-to-lines s)
  (string-split (string-trim s) ","))

(define (main ids)
  (sum (map (lambda (x) (sum (range-to-list x))) (string-to-lines ids))))

(define (sublists l)
  (map (lambda (x) (windows x x l)) (drop-right (divisors (length l)) 1)))


;; ---------------------------
(check-expect (main "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124") 4174379265)
(main (file->string "input.txt"))
(test)
