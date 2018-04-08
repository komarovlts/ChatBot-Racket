#lang r6rs
(import (rnrs lists (6))
        (rnrs base (6))
        (rnrs r5rs (6))
        (rnrs io simple (6))
        (srfi :19)
        )
;Función numeroRandom
;Esta función random tuma un xn y obtiene el xn+1 de la secuencia de números aleatorios.
(define numeroRandom
     (lambda (xn)
     (define a 1103515245)
     (define c 12345)
     (define m 2147483648)
          (remainder (+ (* a xn) c) m)
     )
)
;Cada vez que pedimos un random, debemos pasar como argumento el random anterior.
;Acá un ejemplo que permite generar una lista de números aleatorios.
;Parámetros:
;* "cantidadNumerosLista" indica el largo de la lista a generar.
;* "numeroActual" valor actual del random, se pasa en cada nivel de recursión de forma actualizada
;* "maximoLista" Los números generados van desde 0 hasta maximoLista-1
(define obtenerListaRandom (lambda (cantidadNumerosLista numeroActual maximoLista)
     (if (= 0 cantidadNumerosLista)
          ;do
          '()
     ;else
     (let ((xNvo (numeroRandom numeroActual)))
          (cons (remainder xNvo maximoLista)
               (obtenerListaRandom (- cantidadNumerosLista 1) xNvo maximoLista)
               )
          )
     )
   )
)

(define (seed maximoAleatorio)
     (car (obtenerListaRandom 3 (numeroRandom (date-second (current-date))) maximoAleatorio))
)

;Fecha
(display
(string-append (number->string (date-day (current-date))) "/"
               (number->string (date-month (current-date))) "/"
               (number->string (date-year (current-date))))
               )
(newline)
;Hora
(display
(string-append (number->string (date-hour (current-date))) ":"
               (number->string (date-minute (current-date))) ":"
               (number->string (date-second (current-date))))
               )
