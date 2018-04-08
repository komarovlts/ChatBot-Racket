#lang r6rs
(import (rnrs lists (6))
        (rnrs base (6))
        (rnrs r5rs (6))
        (rnrs io simple (6))
        (srfi :19)
        )
;(define date (current-date 7) )
;(display "This is a string")
;(make-date 0 19 10 10 14 "bogus" "bogus" 0)
;(display (date-day date))
(newline)
;(display (current-time))

(lax-date? (make-date 0 19 10 10 14 "bogus" "bogus" 0))

(lax-date? (string->date "10:21:00" "~H:~M:~S"))

(define (concatena Lista1 Lista2)
     (if (null? Lista1)
          ;do
          Lista2
     ;else
     (cons (car Lista1) (concatena (cdr Lista1) Lista2))
     )
)
;(display (concatena '("hola") '(0 2 1)))
;(display (find even? '(3 1 4 1 5 9)))

;(let ((date (make-date (current-time))))
;  (display (date-month date)))
(display (date? (current-date)))
(newline)
(display (current-date ))
(newline)
(display (date-second (current-date)))

