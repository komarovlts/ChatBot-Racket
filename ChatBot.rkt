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
;Cada vez que se pide un random, debemos pasar como argumento el random anterior.
;Acá un ejemplo que permite generar una lista de números aleatorios.
;Parámetros:
;* "cantidadNumerosLista" indica el largo de la lista a generar.
;* "numeroActual" valor actual del random, se pasa en cada nivel de recursión de forma actualizada
;* "maximoLista" Los números generados van desde 0 hasta maximoLista-1
(define obtenerListaRandom
     (lambda (cantidadNumerosLista numeroActual maximoLista)
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

;Código de numeroRandom y obtenerListaRandom se han obtenido desde:
;http://www.udesantiagovirtual.cl/moodle2/pluginfile.php?file=%2F93558%2
;Fmod_folder%2Fcontent%2F0%2Fvflores%2FrandomScheme.rkt&forcedownload=1

(define (seed valor)
     (car (obtenerListaRandom 1 valor 3))
)
;(seed (date-second (current-date))) = pseudoaleatoreo.
;(seed 1) = 0
;(seed 2) = 1
;(seed 4) = 2

;CONSTRUCTOR
(define log '() )

;FUNCION DE PERTENENCIA
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

;LLamadas: (separarEvaluaciones (list 1 4 3 5 7 1))
;         (adquirirPersonalidad (list 1 4 3 5 7 1))

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

;CONSTRUCTOR
(define (chatBot personalidad anteriorListaChatbot)
     (define listaChatbot '())
     (cond
          ((eqv? personalidad "informal" ) (append listaChatbot '(0) (separarEvaluaciones anteriorListaChatbot)))
          ((eqv? personalidad "formal" ) (append listaChatbot '(1) (separarEvaluaciones anteriorListaChatbot)))
          )
)
;Llamada: (chatBot "informal" '())

;FUNCION DE PERTENENCIA
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

;Llamada: (chatBot? '(2 3 4 5))

;SELECTORES
(define (separarEvaluaciones listaChatbot)
     (define listaEvaluacion '())
     (if (not (= (length listaChatbot) 0))
          ;do
          (append listaEvaluacion (cdr listaChatbot))
     ;else
     listaChatbot
          )
     )

(define (adquirirPersonalidad listaChatbot)
     (define personalidadChatbot '())
     (if (not (= (length listaChatbot) 0))
          ;do
          (append personalidadChatbot (car listaChatbot))
     ;else
     listaChatbot
     )
)

(define (Hora&Fecha momentoActual listaHora&Fecha)
     (append listaHora&Fecha
     ;Fecha
          (list (string-append (number->string (date-day (current-date))) "/"
                         (number->string (date-month (current-date))) "/"
                         (number->string (date-year (current-date)))
                    )
               )
     ;Hora
          (list (string-append (number->string (date-hour (current-date))) ":"
                         (number->string (date-minute (current-date))) ":"
                         (number->string (date-second (current-date)))
                    )
               )
          )
     )
;Llamada: (Hora&Fecha current-date '())

(define (obtenerElemento posicionObjetivo lista)
     (cond
          ((or (< posicionObjetivo 0) (> posicionObjetivo (length lista))))
          ((> posicionObjetivo 0) (obtenerElemento (- posicionObjetivo 1) (cdr lista)))
          (else (car lista))
          )
)

(define (adquirirUltimo log)
     (if (< (length log) 1)
          ;do
          (car log)
     ;else
     (adquirirUltimo log)
     )
)
;Llamada: (adquirirUltimo log)

(define (displayLog log)
     (display log)
)
;Llamada: (displayLog log)

(define (begingDialog chatBot log seed)
     (if (= (adquirirPersonalidad chatBot) 1) ;Personalidad: Formal.
          ;do
          (cond
               ((and (> (date-hour (current-date)) 6) (< (date-hour (current-date)) 12) ())
               ;do
               (append log (Hora&Fecha current-date '()) (list "Chatbot:" "Buenos Días, ¿Cúal es tu nombre?" "|Nombre|"))
               )
               ((and (>= (date-hour (current-date)) 12) (< (date-hour (current-date)) 20))
               ;do
               (append log (Hora&Fecha current-date '()) (list "Chatbot:" "Buenas Tardes, ¿Cúal es tu nombre?" "|Nombre|"))
               )
               ((>= (date-hour (current-date)) 20)
               ;do
               (append log  (Hora&Fecha current-date '()) (list "Chatbot:" "Buenas Noches, ¿Cúal es tu nombre?" "|Nombre|"))
               )
               (else (append log (Hora&Fecha current-date '()) (list "Chatbot:" "Buenas, ¿Cómo te llamas?" "|Nombre|")))
               )
          ;else Personalidad: Informal.
          (cond
               ((= seed 0) (append log (Hora&Fecha current-date '()) (list "Chatbot:" "¡Hola!, Cómo te llamai?" "|Nombre|")))
               ((= seed 1) (append log (Hora&Fecha current-date '()) (list "Chatbot:" "¡Buena!, Cómo te llamas?" "|Nombre|")))
               ((= seed 2) (append log (Hora&Fecha current-date '()) (list "Chatbot:" "¡Hola compa!, Cómo te llamai?" "|Nombre|")))
               )
          )
     )
;Llamada: (define log1 (begingDialog cb1 log (seed 1)))

;Se realiza la venta de homocinéticas de los siguientes vehículos:
;    Toyota Rav4
;    Renault Duster
;    Hyundai Tucson
;    Nissan Qashqai
;    Nissan Kicks
(define (sendMessage msg chatBot log seed)
     (cond
          ((eqv? (adquirirUltimo log) "|Nombre|")
               (if (= (adquirirPersonalidad chatBot) 1) ;Personalidad: Formal.
                    ;do
                    (cond
                         ((= seed 0) (append log (list msg) (list "Usuario:" msg ) (Hora&Fecha current-date '()) (list "Chatbot:" "¿Cómo estás?" "|Respuesta1|")))
                         ((= seed 1) (append log (list msg) (list "Usuario:" msg ) (Hora&Fecha current-date '()) (list "Chatbot:" "¿Qué tal estás?" "|Respuesta1|")))
                         ((= seed 2) (append log (list msg) (list "Usuario:" msg ) (Hora&Fecha current-date '()) (list "Chatbot:" "Bienvenido " msg "¿Cómo estás?" "|Respuesta1|")))
                         )
                    ;else Personalidad: Informal.
                    (cond
                         ((= seed 0) (append log (list msg) (list "Usuario:" msg ) (Hora&Fecha current-date '()) (list "Chatbot:" "¿Cómo estay?" "|Respuesta1|")))
                         ((= seed 1) (append log (list msg) (list "Usuario:" msg ) (Hora&Fecha current-date '()) (list "Chatbot:" "¡Buena po" msg "! ¿Cómo estai?" "|Respuesta1|")))
                         ((= seed 2) (append log (list msg) (list "Usuario:" msg ) (Hora&Fecha current-date '()) (list "Chatbot:" "¿Cómo estai" msg "?" "|Respuesta1|")))
                         )
                    )
               )
          )
     (append log (list msg) (list "Usuario:" msg ) (Hora&Fecha current-date '()) (list "Chatbot:" "Bienvenido " msg ", estos son las marcas/modelos de los autos de las homocinéticas que disponemos : 1.-Toyota Rav4, 2.-Renault Duster, 3.-Hyundai Tucson, 4.-Nissan Qashqai, 5.-Nissan Kicks, elija una opción y la cantidad de homocinéticas que desea"))
)
;Llamada: (sendMessage msg cb1 log (seed 1))



(define (endDialog chatBot log seed)
     (if (= (adquirirPersonalidad chatBot) 1)
          ;do
          (cond
               ((and (> (date-hour (current-date)) 6) (< (date-hour (current-date)) 12))
               ;do
               (append log (Hora&Fecha current-date '()) (list "Chatbot: " "Espero haberte sido de utilidad, que tengas un buen día" (adquirirNombre log)",¡Adiós!. ¡No olvides puntuar la aplicación!" "|Fin conversación|"))
               )
               ((and (>= (date-hour (current-date)) 12) (< (date-hour (current-date)) 20))
               ;do
               (append log (Hora&Fecha current-date '()) (list "Chatbot: " "Espero haberte sido de utilidad, que tengas una buena tarde" (adquirirNombre log)",¡Adiós!. ¡No olvides puntuar la aplicación!" "|Fin conversación|"))
               )
               ((>= (date-hour (current-date)) 20)
               ;do
               (append log (Hora&Fecha current-date '()) (list "Chatbot: " "Espero haberte sido de utilidad, que tengas un buena noche "(adquirirNombre log)",¡Adiós!. ¡No olvides puntuar la aplicación!" "|Fin conversación|"))
               )
               (else (append log (Hora&Fecha current-date '()) (list "Chatbot: " "Espero haberte sido de utilidad " (adquirirNombre log)", ¡Hasta luego!. ¡No olvides puntuar la aplicación!" "|Fin conversación|"))
                     )
               )
          ;else
          (cond
               ((= seed 0) (append log (Hora&Fecha current-date '()) (list "Chatbot: " "Chao" (adquirirNombre log)", ¡Nos vimoh!. ¡Acuérdate de puntuar la aplicación!" "|Fin conversación|")))
               ((= seed 1) (append log (Hora&Fecha current-date '()) (list "Chatbot: " "Espero haberte servido" (adquirirNombre log)", ¡Adiós!. ¡Acuérdate de puntuar la aplicación!" "|Fin conversación|")))
               ((= seed 2) (append log (Hora&Fecha current-date '()) (list "Chatbot: " "Dale compa, ¡Nos vemos" (adquirirNombre log)"!. ¡Acuérdate de puntuar la aplicación!" "|Fin conversación|")))
               )
          )
)
;Llamada: (define log1 (endDialog cb1 log (seed 1)))


(define (f log)
     (cond
          ((> (buscadorRepeticiones log "Lo lamento, no logro entenderte, ¿Podrías repetirlo?" 0) 7) 1)
          ((= (buscadorRepeticiones log "Lo lamento, no logro entenderte, ¿Podrías repetirlo?" 0) 5) 2)
          ((= (buscadorRepeticiones log "Lo lamento, no logro entenderte, ¿Podrías repetirlo?" 0) 3) 3)
          ((= (buscadorRepeticiones log "Lo lamento, no logro entenderte, ¿Podrías repetirlo?" 0) 2) 4)
          ((= (buscadorRepeticiones log "Lo lamento, no logro entenderte, ¿Podrías repetirlo?" 0) 0) 5)
          (else 0)
          )
)
;Llamada: (f log)

(define (rate chatbot score f log)
     (if (eqv? (adquirirUltimo log) "|Fin conversación|")
          ;do
          (append chatbot (list score) (list f))
     ;else
     (append log (list "Conversación aún en curso."))
)
;Llamada: (rate cb1 4 (f log))

(define (procesoRecursivoBR log stringObjetivo numeroCreciente)
     (if (not (= (member stringObjetivo log) 0))
          ;do
          (buscadorRepeticiones (cdr (member stringObjetivo log)) stringObjetivo (+ numeroCreciente 1))
     ;else
     numeroCreciente
     )
)

(define (buscadorRepeticiones log stringObjetivo) ; Encapsulación.
     (procesoRecursivoBR log stringObjetivo 0)
)
;Llamada: (buscadorRepeticiones log "" 0)

(define (adquirirNombre log)
     (car (cdr (member "|Nombre|" log)))
)
;Llamada: (adquirirNombre log)
