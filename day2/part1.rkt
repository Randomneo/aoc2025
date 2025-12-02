#lang racket
(require test-engine/racket-tests)
(require math)


(check-expect (ranges 11 22) '(11 22))
(check-expect (ranges 10 20) '(11))
(check-expect (ranges 1000 1234) '(1010 1111 1212))
(check-expect (ranges 1023 1234) '(1111 1212))
(check-expect (ranges 1000 1200) '(1010 1111))

(define (ranges from to)
  (local ((define low (string->number (car (split (number->string from)))))
          (define high (string->number (car (splitc (number->string to)))))
          (define (make-elf i)
            (local ((define base (number->string (+ low i))))
              (string->number (string-append base base))))
          (define (between value) (and (>= value from) (<= value to))))
    (filter between (build-list (add1 (- high low)) make-elf))))

(check-expect (split "123") '("1" "23"))
(define (split str) (split-f str quotient))
(check-expect (splitc "123") '("12" "3"))
(define (splitc str) (split-f str (lambda (a b) (ceiling (/ a b)))))
(define (split-f str at-f)
  (let-values ([(a b) (split-at (string->list str) (at-f (string-length str) 2))])
    `(,(list->string a) ,(list->string b))))


; (sum (ranges 11 22))
; (string-split (string-trim (file->string "input.txt")) ",")
(define (range-to-list r)
  (local ((define r-pair (string-split r "-"))
          (define from (max (string->number (car r-pair)) 10))
          (define to (string->number (cadr r-pair))))
    (ranges from to)))

(define (main file-name)
  (local ((define lines (string-split (string-trim (file->string "input.txt")) ",")))
    (sum (map (lambda (x) (sum (range-to-list x))) lines))))
(main "input.txt")
(test)
