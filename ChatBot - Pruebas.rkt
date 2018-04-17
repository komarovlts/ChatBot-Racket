#lang r6rs
(import (rnrs lists (6))
        (rnrs base (6))
        (rnrs r5rs (6))
        (rnrs io simple (6))
        (rnrs files (6))
        (rnrs mutable-strings (6))
        (srfi :19)
        )
;(define date (current-date 7) )
;(display "This is a string")
;(make-date 0 19 10 10 14 "bogus" "bogus" 0)
;(display (date-day date))
;(newline)
;(display (current-time))

#|(lax-date? (make-date 0 19 10 10 14 "bogus" "bogus" 0))

(lax-date? (string->date "10:21:00" "~H:~M:~S"))

(define (concatena Lista1 Lista2)
     (if (null? Lista1)
          ;do
          Lista2
     ;else
     (cons (car Lista1) (concatena (cdr Lista1) Lista2))
     )
)
|#
;(display (concatena '("hola") '(0 2 1)))
;(display (find even? '(3 1 4 1 5 9)))


;(let ((date (make-date (current-time))))
;(display (date-month date)))
;(display (date? (current-date)))
;(newline)
;(display (current-date ))
;(newline)
;(display (date-day (current-date)))
;(newline)
;(display (date-month (current-date)))
;(newline)
;(display (date-year (current-date)))
;(newline)
;(display (string-append "a" "b" "c" "d" ))
;(newline)
;(display (string-append (number->string (date-day (current-date))) "/" (number->string (date-month (current-date))) "/" (number->string (date-year (current-date)))))

;(newline)
#|
(define chat 0)
(define log 0)
(define seed 0)
(define (sendMassage chat log seed nombre edad)
  (display (string-append "Hola " nombre ", ¿Cómo estás?"))
  (display (string-append "así que tienes" (number->string edad)", aún eres ilegal."))
  )
|#

;(display (find even? '(3 1 4 1 5 9)))
;(sendMassage chat log seed Omar 17)

#|
(define (unirListas lista1 lista2)
     (cond
          ((null? lista1) lista2)
          ((null? lista2) lista1)
          (else (cons (car lista1) (cons (car lista2) (unirListas (cdr lista1) (cdr lista2)))))
          )
)
|#

#|
(define (obtenerElemento posicionObjetivo lista)
     (cond
          ((or (< posicionObjetivo 0) (> posicionObjetivo (length lista))))
          ((> posicionObjetivo 0) (obtenerElemento (- posicionObjetivo 1) (cdr lista)))
          (else (car lista))
          )
)

(define (separarEvaluaciones  listaChatbot)
     (define listaEvaluacion (cdr listaChatbot))
     listaEvaluacion
)

(define (adquirirPersonalidad listaChatbot)
  (define personalidadChatbot (car listaChatbot))
     personalidadChatbot
)

(define (log? log)
     (if (list? log)
          ;do
          (if (> (length log) 0)
               ;do
               (if (null? (filter (lambda (x) (not(string? x))) log))
                    ;do
                    #t
               ;else
               #f
               )
          ;else
          #t
          )
     ;else
     #f
     )
)
|#

(define (adquirirPersonalidad listaChatbot)
     (define personalidadChatbot '())
     (if (not (= (length listaChatbot) 0))
          ;do
          (append personalidadChatbot (car listaChatbot))
     ;else
     listaChatbot
     )
)

(define (chatBot? chatBot)
     (if (list? chatBot)
          ;do
          (if (> (length chatBot) 0)
               ;do
               (if (null? (filter (lambda (x) (not(number? x))) chatBot))
                    ;do
                    (if (or (= (adquirirPersonalidad chatBot) 1) (= (adquirirPersonalidad chatBot) 0))
                         ;do
                         #t
                    ;else
                    #f
                    )
               ;else
               #f
               )
          ;else
          #t
          )
     ;else
     #f
     )
)
;(list 1 4 3 5 7 1)
;'(4 3 5 7 1)

;#| |#   ---> comentarios
