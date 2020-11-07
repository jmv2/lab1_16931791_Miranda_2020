#lang racket

(require "User.rkt" "Stack.rkt" "Question.rkt" "Answer.rkt" "otrasFunciones.rkt")


(define userExists?
  (lambda (stack username)
    (memberOf
     (get-users-stack stack) ; Lista de usuarios del stack
     (get-user-by-name stack username))))


(define (register stack username password)
  (if (null? stack)
      (Stack (User 1 username password 0) stack)
      (if (userExists? stack username)
          stack
          (Stack stack (User (+ 1 (length (get-users-stack stack))) username password 0 #f)))))


(define login
  (lambda (stack username password operation)
    (if (and (userExists? stack username)
             (equal? (get-user-password stack username) password)
             (userActive? username (set-user-active stack username #t)))
             ; forzar a que se inicie la sesión si se cumple que usuario existe
             (operation stack)
             operation)))

;((((login stackCompleto "usuario1" "pass1" answer)(date 3 11 2020))5)"mi respuesta")
;(((login stackCompleto "usuario1" "pass1" ask)(date 3 11 2020))"pregunta de prueba")



(define ask
  (lambda (stack-actualizado)
    (lambda (fecha-pregunta)
      (lambda (pregunta e1 e2 e3) ; Para respetar el formato del ejemplo
        (list stack-actualizado fecha-pregunta pregunta (list e1 e2 e3))))))


;Aquí como se usa la función "ask"
;(((login "user" "pass" "stack" ask) (date 5 11 2020))"pregunta 1" "etiqueta 1" "etiqueta 2" "etiqueta 3")


;Aquí lo haré con la notacón normal

(define (reward stack) ; Este es el argumento principal, con el cual es ejecutado desde login
  (lambda (id-pregunta) ; Este es el primer argumento curry, que se coloca fuera el parentesis
    (lambda (recompensa) ; El segundo argumento fuera del segundo parentesis
      (list stack id-pregunta recompensa))))

; En acción la función reward
;(((login "user" "pass" "stack" reward) 60) 1000)


(define answer
  (lambda (stack-actualizado)
    (lambda (fecha-respuesta)
      (lambda (id-pregunta) ; es el ID de la pregunta que se quiere responder
        (lambda (texto-respuesta e1 e2 e3) ; el cuerpo de la respuesta ;)
          (list stack-actualizado fecha-respuesta id-pregunta texto-respuesta (list e1 e2 e3)))))))

;prueba de función answer



;Aqui la escribi con el formato λ

(define accept
  (λ (stack-actualizado)
    (λ (id-pregunta)
      (λ (id-respuesta)
        (list stack-actualizado id-pregunta id-respuesta)))))