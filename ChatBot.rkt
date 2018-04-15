#lang r6rs
(import (rnrs io simple (6))
        (rnrs files (6))
        (rnrs lists (6))
        (rnrs base (6))
        (rnrs r5rs (6))
        (srfi :19))

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

(define (seed segundoActual)
     (car (obtenerListaRandom 1 segundoActual 3))
)

(define (unirListas lista1 lista2)
     (cond
          ((null? lista1) lista2)
          ((null? lista2) lista1)
          (else (cons (car lista1) (cons (car lista2) (unirListas (cdr lista1) (cdr lista2)))))
          )
)

(define (obtenerElemento posicionObjetivo lista)
     (cond
          ((or (< posicionObjetivo 0) (> posicionObjetivo (length lista))))
          ((> posicionObjetivo 0) (obtenerElemento (- posicionObjetivo 1) (cdr lista)))
          (else (car lista))
          )
)

(define (separarEvaluaciones largoLista listaChatbot nuevaLista)
     (if (and (> largoLista 0)(< largoLista (length listaChatbot)))
          ;do
          (and (cons (obtenerElemento largoLista listaChatbot) nuevaLista ) (separarEvaluaciones (+ largoLista 1) listaChatbot nuevaLista))
     ;else
     nuevaLista
     )
)

;LLamada: (separarEvaluaciones (- (length '(8 4 2 3 5)) 1) '(8 4 2 3 5) '())

;Chatbot.
;Dominio:
;    personalidad: Se ingresa un string que determina la personalidad que tenga el chatbot.
;    vocabulario: Se ingresa un string que determina el tipo de vocabulario que tenga el chatbot.
;    evaluaciones: Se ingresa una lista con las evaluaciones anteriores.
;Tipo 1: Coloquial de vocabulario Alto.
;Tipo 2: Coloquial de vocabulario Pobre.
;Tipo 3: Formal de vocabulario Alto.
;Tipo 4: Formal de vocabulario Pobre.
;Se devulve una lista en donde el primer elemento de esta es el tipo
;de chatbot, el resto equivalen a las evaluaciones que tenga el chatbot.
(define (chatBot personalidad vocabulario #|evaluaciones|#)
     (cond
          ((and (eqv? personalidad "coloquial" ) (eqv? vocabulario "alto")) 1)
          ((and (eqv? personalidad "coloquial" ) (eqv? vocabulario "pobre")) 2)
          ((and (eqv? personalidad "formal" ) (eqv? vocabulario "alto")) 3)
          ((and (eqv? personalidad "formal" ) (eqv? vocabulario "pobre")) 4)
          )


)

;(define (log logAnterior)

;)

(define (mostrarHora&Fecha Null)
;Fecha
(display
(string-append (number->string (date-day (current-date))) "/"
               (number->string (date-month (current-date))) "/"
               (number->string (date-year (current-date))))
               )
(display "  ")
;Hora
(display
(string-append (number->string (date-hour (current-date))) ":"
               (number->string (date-minute (current-date))) ":"
               (number->string (date-second (current-date))))
               )
(display "  ")
)


(define log 0)
;(define seed 0)
;(define (sendMassage chatBot log seed nombre edad)
     ;(display (string-append "Hola " nombre ", ¿Cómo estás?"))
     ;(display (string-append "así que tienes" (number->string edad)", aún eres ilegal."))
     ;)

(define (begingDialog chatBot log seed)
     (cond
          ((and (> (date-hour (current-date)) 6) (< (date-hour (current-date)) 12))
          ;do
          (and (mostrarHora&Fecha 0) (display "Hola, Buenos Días, ¿Cómo te llamas?")))
          ((and (>= (date-hour (current-date)) 12) (< (date-hour (current-date)) 20))
          ;do
          (and (mostrarHora&Fecha 0) (display "Hola, Buenas Tardes, ¿Cómo te llamas?")))
          ((and (>= (date-hour (current-date)) 20) (< (date-hour (current-date)) 0))
          ;do
          (and (mostrarHora&Fecha 0) (display "Hola, buenos días, ¿Cómo te llamas?")))
          (else (display "Hola, ¿Cómo te llamas?"))
          )
     )

(define (endDialog chatBot log seed)
(cond
     ((and (> (date-hour (current-date)) 6) (< (date-hour (current-date)) 12))
     ;do
     (and (mostrarHora&Fecha 0) (display "Espero haberte sido de utilidad, que tengas un buen día,¡Adiós!")))
     ((and (>= (date-hour (current-date)) 12) (< (date-hour (current-date)) 20))
     ;do
     (and (mostrarHora&Fecha 0) (display "Espero haberte sido de utilidad, que tengas una buena tarde,¡Adiós!")))
     ((and (>= (date-hour (current-date)) 20) (< (date-hour (current-date)) 0))
     ;do
     (and (mostrarHora&Fecha 0) (display "Espero haberte sido de utilidad, que tengas un buena noche,¡Adiós!")))
     (else (display "Espero haberte sido de utilidad, ¡Hasta luego!"))
     )
)

;(define f 0)
;(define (rate chatbot score f log)
;)

;LLamadas:
;begingDialog: (begingDialog chatBot log (seed (date-second (current-date)) 3))
;endDialog: (endDialog chatBot log (seed (date-second (current-date)) 3))
