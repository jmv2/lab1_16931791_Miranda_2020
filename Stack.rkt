#lang racket

(require "User.rkt" "Question.rkt" "Answer.rkt" "otrasFunciones.rkt")
(provide Stack Stack? get-users-stack get-user-by-id get-user-by-name get-user-by-password userActive? get-questions-stack get-answers-stack)
(provide remove-user-stack set-user-active full-stack)

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


;Selectores

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
      (if (equal? (get-username (car (get-users-stack stack))) username)
          (car (get-users-stack stack))
          (get-user-by-name (cdr (get-users-stack stack)) username)))))
    

;Función que retorna la contraseña de un usuario dentro de un stack
(define (get-user-by-password stack username)
  (get-user-password (get-user-by-name stack username)))


;Función que retorna si un usuario tiene sesión activa en el stack
(define userActive?
  (lambda (stack username)
    (get-user-active-session (get-user-by-name stack username))))

(define get-questions-stack
  (lambda (stack)
    (filter Question? stack)))

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
      (set-session (get-user-by-name username stack) active) 
      (remove-user-stack username stack))))
  

; ==========Definiciones para prueba ========================

;usuarios de prueba
(define u1 (User 1 "usuario1" "pass1" 10 #f))
(define u2 (User 2 "usuario2" "pass2" 20 #f))
(define u3 (User 3 "usuario3" "pass3" 30 #t))
;pregutas de prueba
(define q1 (Question 1 1 (date 1 11 2020) "Pregunta 1" (tag  "tag1" "tag2" "tag3")))
(define q2 (Question 2 2 (date 2 11 2020) "Pregunta 2" (tag  "tag1" "tag2" "tag3")))
(define q3 (Question 3 3 (date 3 11 2020) "Pregunta 3" (tag  "tag1" "tag2" "tag3")))
;respuestas de prueba
(define a1 (Answer 1 1 1 (date 1 12 2020) (tag  "tag1" "tag2" "tag3") "Respuesta 1"))
(define a2 (Answer 2 2 2 (date 2 12 2020) (tag  "tag1" "tag2" "tag3") "Respuesta 2"))
(define a3 (Answer 3 3 3 (date 3 12 2020) (tag  "tag1" "tag2" "tag3") "Respuesta 3"))
;stacks de prueba
(define stackUsuarios
  (Stack u1 (Stack u2 (Stack u3 voidStack))))

(define stackPreguntas
  (Stack q1 (Stack q2 (Stack q3 voidStack))))

(define stackRespuestas
  (Stack a1 (Stack a2 (Stack a3 voidStack))))

(define full-stack
  (Stack(Stack(Stack(Stack(Stack(Stack(Stack(Stack(Stack voidStack u1)u2)u3) q1)q2)q3)a1)a2)a3))

  
"StackOver Flow"