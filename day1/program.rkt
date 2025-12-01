#lang racket
(require test-engine/racket-tests)

(check-expect (rotate 50 -50) 1)
(check-expect (rotate 0 -5) 0)
(check-expect (rotate 0 5) 0)
(check-expect (rotate 50 50) 1)

(define (rotate cur rot)
  (cond [(= cur 0) 0]
        [(<= (+ cur rot) 0) 1]
        [(>= (+ cur rot) 100) 1]
        [else 0]))

(check-expect (count '("L20") 50) 0)
(check-expect (count '("R50") 50) 1)
(check-expect (count '("L50") 50) 1)
(check-expect (count '("R1000") 50) 10)
(check-expect (count '("R1000" "L50") 50) 11)
(check-expect (count '("L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14" "L82") 50) 6)

(define (count lines cur)
  (cond [(empty? lines) 0]
        [else
         (local ((define line (car lines))
                 (define chars (string->list line))
                 (define dir (car chars))
                 (define rest (list->string (cdr chars)))
                 (define operation (if (char=? dir #\R) + -))
                 (define num (operation 0 (string->number rest)))
                 (define full-rotations (abs (quotient num 100)))
                 (define rotation (remainder num 100))
                 (define val (+ cur num))
                 (define r (count (cdr lines) (modulo val 100))))
           (+ r (rotate cur rotation) full-rotations))]))

(test)
(count (file->lines "./input.txt") 50)
