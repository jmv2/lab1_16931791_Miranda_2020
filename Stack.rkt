#lang racket

(require "User.rkt" "Question.rkt" "Answer.rkt" "otrasFunciones.rkt")
(provide Stack Stack? get-users-stack get-user-by-id get-user-by-name get-user-by-password userActive? get-questions-stack get-answers-stack)
(provide remove-user-stack set-user-active full-stack user-active)

; TDA Stack
(define voidStack null)
(define voidStack? null?)


; constructor

(define Stack
  (lambda (stack stackElement)
    (if (or (User? stackElement)(Question? stackElement)(Answer? stackElement))
        (add-tail stack stackElement)
        stack)))
        
            
;Pertenencia

(define (Stack? stack)
  (list? stack))


; FUNCIONES SELECTORAS

;Selectores de usuarios

;Función que retorna todos los usuarios registrados dentro del Stack
(define get-users-stack
  (lambda (stack)
    (filter User? stack)))

;Función que retorna un usuario del stack dado el ID del mismo
(define get-user-by-id
  (lambda (stack userId)
      (filter
        (lambda (lista) (= (get-userid lista) userId))
        (get-users-stack stack))))


;retorna el usuario solicitado 

(define get-user-by-name
  (lambda (stack username)
    (if (voidStack? stack)
      voidStack
      (if (equal? (get-username (car (get-users-stack stack))) username) ;Comparar primer elemento de la lista de usuarios con "username"
          (car (get-users-stack stack))
          (get-user-by-name (cdr (get-users-stack stack)) username)))))
    

; Retorna un usuario activo
(define user-active
    (lambda (stack)
      (if (voidStack? stack)
          voidStack
          (if (userActive? stack (get-username (car (get-users-stack stack))))
              (car (get-users-stack stack))
              ;(cons (get-username (car (get-users-stack stack)) users))
              (user-active (cdr (get-users-stack stack)))))))


;Función que retorna la contraseña de un usuario dentro de un stack
(define (get-user-by-password stack username)
  (get-user-password (get-user-by-name stack username)))

;retorna la reputación de un usuario en el Stack dado su nombre de usuario
(define (get-reputation stack username)
  (get-user-reputation (get-user-by-name stack username)))


;Función que retorna si un usuario tiene sesión activa en el stack
(define userActive?
  (lambda (stack username)
    (get-user-active-session (get-user-by-name stack username))))


;Selectores de preguntas

(define get-questions-stack
  (lambda (stack)
    (filter Question? stack)))


; Función que con un id de pregunta, restorna la pregunta completa
(define get-question-by-id
  (lambda (stack question-id)
    (if (voidStack? stack)
        voidStack
        (if (equal?
             (get-question-id (car (get-questions-stack stack)))
             question-id)
             (car (get-questions-stack stack))
             (get-question-by-id  (cdr (get-questions-stack stack)) question-id)))))
            





;selectores de respuestas

(define get-answers-stack
  (lambda (stack)
    (filter Answer? stack)))

; Modificadores

(define remove-user-stack
  (lambda (stack username)
    (quitarElementoLista stack (get-user-by-name stack username))))

(define set-user-active
  (lambda (stack username active)
    (Stack
     (remove-user-stack stack username)
     (set-session (get-user-by-name stack username) active) )))
  

; ==========Definiciones para prueba ========================

;usuarios de prueba
(define u1 (User 1 "usuario1" "pass1" 10 #f))
(define u2 (User 2 "usuario2" "pass2" 20 #f))
(define u3 (User 3 "usuario3" "pass3" 30 #f))
;pregutas de prueba
(define q1 (Question 1 1 (date 1 11 2020) "Pregunta 1" (tag  "tag1" "tag2" "tag3") 0))
(define q2 (Question 2 2 (date 2 11 2020) "Pregunta 2" (tag  "tag1" "tag2" "tag3") 0))
(define q3 (Question 3 3 (date 3 11 2020) "Pregunta 3" (tag  "tag1" "tag2" "tag3") 0))
;respuestas de prueba
(define a1 (Answer 1 1 1 (date 1 12 2020) (tag  "tag1" "tag2" "tag3") "Respuesta 1"))
(define a2 (Answer 2 2 2 (date 2 12 2020) (tag  "tag1" "tag2" "tag3") "Respuesta 2"))
(define a3 (Answer 3 3 3 (date 3 12 2020) (tag  "tag1" "tag2" "tag3") "Respuesta 3"))
;stack de prueba
(define full-stack
  (Stack(Stack(Stack(Stack(Stack(Stack(Stack(Stack(Stack voidStack u1)u2)u3) q1)q2)q3)a1)a2)a3))

  
"StackOver Flow"