#lang racket

(require "User.rkt" "Stack.rkt" "Question.rkt" "Answer.rkt" "otrasFunciones.rkt")
(define default-reward 0);  al momento de crear un nueva pregunta su recomensa es 0
(define default-active-session #f); al momento de crear un usuario el estado de su sesión en inactivo: #f

(define logout
  (lambda (stack)
    (set-user-active stack (get-username (user-active stack)) #f )))
    

(define userExists?
  (lambda (stack username)
    (memberOf
     (get-users-stack stack) ; Lista de usuarios del stack
     (get-user-by-name stack username))))


(define (register stack username password)
  (if (null? stack)
      (Stack stack (User 1 username password 0 default-active-session))
      (if (userExists? stack username)
          stack
          (Stack stack (User (+ 1 (length (get-users-stack stack))) username password 0 #f)))))


(define login
  (lambda (stack username password operation)
    (if (and (userExists? stack username)
             (equal? (get-user-by-password stack username) password))
        ; forzar a que se inicie la sesión si se cumple que usuario existe
        (operation (set-user-active stack username #t))
        operation )))



(define ask
  (lambda (stack)
    (lambda (fecha-pregunta)
      (lambda (pregunta . labels)
        (logout
         (Stack stack
            (Question
              (+ 1 (length (get-questions-stack stack)))
              (get-userid (user-active stack))
              fecha-pregunta
              pregunta
              labels
              default-reward)))))))




(define reward
  (lambda (stack)
    (lambda (id-question)
      (lambda (reward-question)
        (if (<= reward-question (get-user-reputation (user-active stack)))
            (logout
             (Stack
              (remove-from-stack
               (Stack
                (remove-from-stack stack (user-active stack))
                (set-reward-offer (user-active stack) reward-question))
               (get-question-by-id stack id-question))
               (set-question-reward (get-question-by-id stack id-question) reward-question)))
            stack)))))
            
            ;(Stack
             ;(set-user-offer stack (get-userid (user-active stack)) reward-question))
            ;(remove-from-stack stack (user-active stack)))))))
             ;(Stack
              ;(remove-from-stack stack (get-user-by-id stack (user-active stack)))
              ;(set-user-offer stack (get-user-by-id stack (user-active stack)) reward-question))
             ;null)))))

;(remove-from-stack stack (get-question-by-id stack id-question) ;Eliminar pregunta
; ; Eliminar usuario
;)
;(set-question-reward (get-question-by-id stack id-question) reward-question)

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