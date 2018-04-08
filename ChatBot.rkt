#lang r6rs
(import (rnrs lists (6))
        (rnrs base (6))
        (rnrs r5rs (6))
        (rnrs io simple (6))
        (srfi :19)
        )

;Esta función random tuma un xn y obtiene el xn+1 de la secuencia de números aleatorios.
(define myRandom
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
;* "cuantos" indica el largo de la lista a generar.
;* "xActual" valor actual del random, se pasa en cada nivel de recursión de forma actualizada
;* "maximo" Los números generados van desde 0 hasta maximo-1
(define getListaRandom (lambda (cuantos xActual maximo)
     (if (= 0 cuantos)
          ;do
          '()
     ;else
     (let ((xNvo (myRandom xActual)))
          (cons (remainder xNvo maximo)
               (getListaRandom (- cuantos 1) xNvo maximo)
               )
          )
     )
   )
)

(define (seed maximoAleatorio)
     (car (getListaRandom 3 (myRandom (date-second (current-date))) maximoAleatorio))
)
