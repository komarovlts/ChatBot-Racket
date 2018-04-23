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
;Tipo de Recusión: Recursión de Cola.
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
;http://www.udesantiagovirtual.cl/moodle2/pluginfile.php?file=%2F93558%2Fmod_folder%2Fcontent%2F0%2Fvflores%2FrandomScheme.rkt&forcedownload=1
;Se han cambiado algunos nombres de las variables para una mejor comprensión, no obstante en escencia son la misma función.

;Nombre función: seed
;Firma:
;    Dominio: Números enteros.
;    Recorrido: Números enteros entre 0 y 1.
;Descripción: A través de las funciones obtenerListaRandom y numeroRandom, se obtiene un número correspondiente, al aplicar como argumento el tiempo en segundos
;             se puede obtener un numero pseudoaleatoreo.
(define (seed valor)
     (car (obtenerListaRandom 1 valor 2))
)
;Llammadas:
;(seed (date-second (current-date))) = pseudoaleatoreo.
;(seed 1) = 0
;(seed 2) = 1

(define log '())

;Nombre función: log?
;Firma:
;    Dominio: Cualquier tipo de dato.
;    Recorrido: Booleano.
;Descripción: Se verifica que el argumento dado sea una lista de strings o una lista vacía.
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

;Nombre función: log?
;Firma:
;Dominio:
;    personalidad: Se ingresa un string que determina la personalidad que tenga el chatbot.
;    anteriorListaChatbot: Se ingresa una lista chatbot con el fin de obtener las evaluaciones anteriores.
;Tipo 0: Informal.
;Tipo 1: Formal.
;Recorrido:
;Se devulve una lista en donde el primer elemento de esta es el tipo
;de chatbot, el resto equivalen a la acumulación de las evaluaciones que tenga el chatbot.
;Descripción: Se crea una lista de enteros en donda el primer entero identifica la personalidad del bot y toma las evaluaciones de algún chat anterior para añadirlas al actual.
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
;Nombre función: chatBot?
;Firma:
;    Dominio: Cualquier tipo de dato.
;    Recorrido: Booleano.
;Descripción: Se verifica que el argumento dado sea una lista de enteros o una lista vacía.
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

;Nombre función: separarEvaluaciones
;Firma:
;    Dominio: Lista de enteros.
;    Recorrido: Lista de Enteros.
;Descripción: Se toman todos los elementos de la lista exceptuando al primer elemento.
(define (separarEvaluaciones listaChatbot)
     (define listaEvaluacion '())
     (if (not (= (length listaChatbot) 0))
          ;do
          (append listaEvaluacion (cdr listaChatbot))
     ;else
     listaChatbot
          )
     )

;Nombre función: adquirirPersonalidad
;Firma:
;    Dominio: Lista de enteros.
;    Recorrido: Lista de Enteros con un sólo elemento.
;Descripción: Se toman el primer elemento de la lista y se añade a una lista vacía.
(define (adquirirPersonalidad listaChatbot)
     (define personalidadChatbot '())
     (if (not (= (length listaChatbot) 0))
          ;do
          (append personalidadChatbot (car listaChatbot))
     ;else
     listaChatbot
     )
)

;Nombre función: Hora&FechaActual
;Firma:
;    Dominio: Current-date, que es una estructura que contiene varios datos sobre la fecha y la hora actual, además se recibe una lista vacía.
;    Recorrido: Lista con metadatos de hora y fecha.
;Descripción: Se toman el primer elemento de la lista y se añade a una lista vacía.
(define (Hora&FechaActual momentoActual listaHora&Fecha)
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

(define (Hora&Fecha momentoActual) ;Encapsulación
     (Hora&FechaActual momentoActual '())
)
;Llamada: (Hora&Fecha current-date)

;Nombre función: adquirirUltimo
;Firma:
;    Dominio: Lista de strings.
;    Recorrido: String.
;Descripción: Se recorre la lista de strings hasta obtener el último string de la lista.
;Tipo de Recursión: Recursión de cola.
(define (adquirirUltimo log)
     (if (= (length log) 1)
          ;do
          (car log)
     ;else
     (adquirirUltimo (cdr log))
     )
)
;Llamada: (adquirirUltimo log)

;Nombre función: displayLog
;Firma:
;    Dominio: Lista de strings.
;    Recorrido:
;Descripción: Se muestra por pantalla la lista dada por argumento.
(define (displayLog log)
     (display log)
)
;Llamada: (displayLog log)

;Nombre función: begingDialog
;Firma:
;    Dominio: Lista de enteros, lista de strings y un número entero.
;    Recorrido: Lista de strings.
;Descripción: Se añaden mensajes de bienvenida a la lista de strings.
(define (begingDialog chatBot log seed)
     (if (= (adquirirPersonalidad chatBot) 1) ;Personalidad: Formal.
          ;do
          (cond
               ((and (> (date-hour (current-date)) 6) (< (date-hour (current-date)) 12) ())
               ;do
               (append log (Hora&Fecha current-date) (list "Chatbot:" "Buenos Días, ¿Cúal es tu nombre?" "|Nombre|"))
               )
               ((and (>= (date-hour (current-date)) 12) (< (date-hour (current-date)) 20))
               ;do
               (append log (Hora&Fecha current-date) (list "Chatbot:" "Buenas Tardes, ¿Cúal es tu nombre?" "|Nombre|"))
               )
               ((>= (date-hour (current-date)) 20)
               ;do
               (append log  (Hora&Fecha current-date) (list "Chatbot:" "Buenas Noches, ¿Cúal es tu nombre?" "|Nombre|"))
               )
               (else (append log (Hora&Fecha current-date) (list "Chatbot:" "Buenas, ¿Cómo te llamas?" "|Nombre|")))
               )
          ;else Personalidad: Informal.
          (cond
               ((= seed 0) (append log (Hora&Fecha current-date) (list "Chatbot:" "¡Hola!, Cómo te llamai?" "|Nombre|")))
               ((= seed 1) (append log (Hora&Fecha current-date) (list "Chatbot:" "¡Buena!, Cómo te llamas?" "|Nombre|")))
               )
          )
     )
;Llamada: (define log1 (begingDialog cb1 log (seed 1)))

;Nombre función: begingDialog
;Firma:
;    Dominio: String, lista de enteros, lista de strings y un número entero.
;    Recorrido: Lista de strings.
;Descripción: Se añaden mensajes a la lista de strings según el string dado por el usuario.
(define (sendMessage msg chatBot log seed)
     (if (eqv? msg "¿Qué pasó con el stock?")
          ;do
          (and (respuesta3 log) (append log (list "|Respuesta4|")))
     ;else
     (cond
          ((eqv? msg "¿Qué pasó con el stock?")
               (and (respuesta3 log) (append log (list "|Respuesta4|")))
               )
          ((eqv? (adquirirUltimo log) "|Nombre|")
               (if (= (adquirirPersonalidad chatBot) 1) ;Personalidad: Formal.
                    ;do
                    (cond
                         ((= seed 0) (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "¿Cómo estás?" "|Respuesta1|")))
                         ((= seed 1) (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "¿Qué tal estás?" "|Respuesta1|")))
                         )
                    ;else Personalidad: Informal.
                    (cond
                         ((= seed 0) (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "¿Cómo estay?" "|Respuesta1|")))
                         ((= seed 1) (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "¿Cómo estai" msg "?" "|Respuesta1|")))
                         )
                    )
               )
          ((eqv? (adquirirUltimo log) "|Respuesta1|")
               (and (respuesta1 msg log seed) (append log (list "Estos son las marcas/modelos de autos de las homocinéticas que disponemos: 1.-Toyota Rav4, 2.-Renault Duster, 3.-Hyundai Tucson, 4.-Nissan Qashqai, 5.-Nissan Kicks, elija la opción que desea." "|Respuesta2|")))
               )
          ((eqv? (adquirirUltimo log) "|Respuesta2|")
               (and (respuesta2 msg log) (append log (list "Perfecto, ¿Cuántas querrá?" "|Respuesta3|")))
               )
          ((eqv? (adquirirUltimo log) "|Respuesta3|")
               (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "Excelente, déjeme verificar el stock, luego le confirmo." "¿Desea comprar algo más?" "|Respuesta4|"))
               )
          ((eqv? (adquirirUltimo log) "|Respuesta4|")
               (and (respuesta4 msg log) (append log (list "Perfecto, ¿Cuántas querrá?" "|Respuesta3|")))
               )
          )
     )
)
;Llamada: (sendMessage msg cb1 log (seed 1))

;Las funciones de respuesta (respuesta1, respuesta2, respuesta3, respuesta4) son funciones sólo para acotar la función sendMessage.
(define (respuesta1 msg log seed)
     (cond
          ((eqv? msg "bien")
               (if (= (adquirirPersonalidad chatBot) 1) ;Personalidad: Formal.
                    ;do
                    (cond
                         ((= seed 0) (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "Oh ¡Perfecto!")))
                         ((= seed 1) (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "¡Que bien!")))
                         )
                    ;else Personalidad: Informal.
                    (cond
                         ((= seed 0) (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "¡Buena!" )))
                         ((= seed 1) (append log (list "Usuario:" msg ) (Hora&Fecha current-date '()) (list "Chatbot:" "¡Buena po!")))
                         )
                    )
               )
          ((eqv? msg "mal")
               (if (= (adquirirPersonalidad chatBot) 1) ;Personalidad: Formal.
                    ;do
                    (cond
                         ((= seed 0) (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "Oh ¡Que mal, espero poder ayudarte!")))
                         ((= seed 1) (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "Espero poder subirte el ánimo cuando lleves lo que buscabas.")))
                         )
                    ;else Personalidad: Informal.
                    (cond
                         ((= seed 0) (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "¡Oh que mala!" )))
                         ((= seed 1) (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "¡de perro!")))
                         )
                    )
               )
          (else (append log  (list "Usuario:" msg ) (list "Chatbot:" "Lo lamento, no logro entenderte, ¿Podrías repetirlo?" "|Respuesta1|")))
     )
)

(define (respuesta2 msg log)
     (cond
          ((eqv? msg "1") (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "Tomo nota...")))
          ((eqv? msg "2") (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "Tomo nota...")))
          ((eqv? msg "3") (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "Tomo nota...")))
          ((eqv? msg "4") (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "Tomo nota...")))
          ((eqv? msg "5") (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "Tomo nota...")))
          (else (append log (list "Usuario:" msg ) (list "Chatbot:" "Lo lamento, no logro entenderte, ¿Podrías repetirlo?" "|Respuesta2|")))
          )
)

(define (respuesta3 log)
    (append log (Hora&Fecha current-date) (list "Chatbot:" "Se ha confirmado el stock y sus items han sido agregados a su carro."))
)

(define (respuesta4 msg log)
     (cond
          ((or(eqv? msg "Sí") (eqv? msg "Si"))(append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "¡Okey!" "Estos son las marcas/modelos de autos de las homocinéticas que disponemos: 1.-Toyota Rav4, 2.-Renault Duster, 3.-Hyundai Tucson, 4.-Nissan Qashqai, 5.-Nissan Kicks, elija la opción que desea." "|Respuesta2|")))
          ((or(eqv? msg "Sí, me gustaría comprar algo más") (eqv? msg "Si, me gustaria comprar algo más")) (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "¡Okey!" "Estos son las marcas/modelos de autos de las homocinéticas que disponemos: 1.-Toyota Rav4, 2.-Renault Duster, 3.-Hyundai Tucson, 4.-Nissan Qashqai, 5.-Nissan Kicks, elija la opción que desea." "|Respuesta2|")))
          ((eqv? msg "No, gracias") (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "¡Está bien! Se ha confirmado el stock y todos sus items han sido agregados a su carro.")))
          ((eqv? msg "No") (append log (list "Usuario:" msg ) (Hora&Fecha current-date) (list "Chatbot:" "¡Está bien! Se ha confirmado el stock y todos sus items han sido agregados a su carro.")))
          (else (append log (list "Chatbot:" "Lo lamento, no logro entenderte, ¿Podrías repetirlo?" "|Respuesta4|")))
          )
)

;Nombre función: endDialog
;Firma:
;    Dominio: Lista de enteros, lista de strings y un número entero.
;    Recorrido: Lista de strings.
;Descripción: Se añaden mensajes de despedida a la lista de strings.
(define (endDialog chatBot log seed)
     (if (= (adquirirPersonalidad chatBot) 1)
          ;do
          (cond
               ((and (> (date-hour (current-date)) 6) (< (date-hour (current-date)) 12))
               ;do
               (append log (Hora&Fecha current-date) (list "Chatbot: " "Espero haberte sido de utilidad, que tengas un buen día, ¡Adiós!. ¡No olvides puntuar la aplicación!" "|Fin conversación|"))
               )
               ((and (>= (date-hour (current-date)) 12) (< (date-hour (current-date)) 20))
               ;do
               (append log (Hora&Fecha current-date) (list "Chatbot: " "Espero haberte sido de utilidad, que tengas una buena tarde, ¡Adiós!. ¡No olvides puntuar la aplicación!" "|Fin conversación|"))
               )
               ((>= (date-hour (current-date)) 20)
               ;do
               (append log (Hora&Fecha current-date) (list "Chatbot: " "Espero haberte sido de utilidad, que tengas un buena noche, ¡Adiós!. ¡No olvides puntuar la aplicación!" "|Fin conversación|"))
               )
               (else (append log (Hora&Fecha current-date) (list "Chatbot: " "Espero haberte sido de utilidad, ¡Hasta luego!. ¡No olvides puntuar la aplicación!" "|Fin conversación|"))
                     )
               )
          ;else
          (cond
               ((= seed 0) (append log (Hora&Fecha current-date) (list "Chatbot: " "Chao, ¡Nos vimoh!. ¡Acuérdate de puntuar la aplicación!" "|Fin conversación|")))
               ((= seed 1) (append log (Hora&Fecha current-date) (list "Chatbot: " "Espero haberte servido, ¡Adiós!. ¡Acuérdate de puntuar la aplicación!" "|Fin conversación|")))
               ((= seed 2) (append log (Hora&Fecha current-date) (list "Chatbot: " "Dale compa, ¡Nos vemos!. ¡Acuérdate de puntuar la aplicación!" "|Fin conversación|")))
               )
          )
)
;Llamada: (define log1 (endDialog cb1 log (seed 1)))

;Nombre función: f
;Firma:
;    Dominio: Lista de strings.
;    Recorrido: Entero.
;Descripción: Se busca la cantidad de veces que se repite la frase, lo cual determina el número a retornar.
(define (f log) ;Métrica
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

;Nombre función: rate
;Firma:
;    Dominio: Lista de enteros, dos números enteros y lista de strings.
;    Recorrido: Lista de enteros.
;Descripción: Se verifica que la conversación ya ha concluido para luego añadir las evaluaciones a la lista de enteros del chatBot.
(define (rate chatbot score f log)
     (if (eqv? (adquirirUltimo log) "|Fin conversación|")
          ;do
          (append chatbot (list score) (list f))
     ;else
     (append log (list "Conversación aún en curso."))
     )
)
;Llamada: (rate cb1 4 (f log))

;Nombre función: buscadorRepeticiones
;Firma:
;    Dominio: Lista de strings, string.
;    Recorrido: Entero.
;Descripción: Se recorre la lista de strings contando las coincidencias del string obtenido como argumento.
;Tipo de Recursión: Recursión de cola.
(define (procesoRecursivoBR log stringObjetivo numeroCreciente)
     (if (not (= (member stringObjetivo log) 0))
          ;do
          (buscadorRepeticiones (cdr (member stringObjetivo log)) stringObjetivo (+ numeroCreciente 1))
     ;else
     numeroCreciente
     )
)

(define (buscadorRepeticiones log stringObjetivo) ;Encapsulación.
     (procesoRecursivoBR log stringObjetivo 0)
)
;Llamada: (buscadorRepeticiones log "" 0)
