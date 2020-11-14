#lang racket

(require "User_16931791_MirandaCardenas.rkt")
(require "Answer_16931791_MirandaCardenas.rkt")
(require "Question_16931791_MirandaCardenas.rkt")
(require "otrasFunciones_16931791_MirandaCardenas.rkt")

(provide Stack get-users-stack get-user-by-id get-user-by-name get-user-by-password userActive? get-questions-stack get-answers-stack)
(provide set-user-active full-stack user-active set-user-offer )
(provide get-question-by-id remove-from-stack)
(provide voidStack) 

; Definiciones estáticas

(define voidStack null)
(define voidStack? null?)

; TDA Stack

; constructor

;Desc: Constructor del TDA stack
;Dom: stack x (User, Question, Answer)
;Rec: Stack
(define Stack
  (lambda (stack stackElement)
    (if (or (User? stackElement)(Question? stackElement)(Answer? stackElement))
        (add-tail stack stackElement)
        stack)))
        
            
; FUNCIONES SELECTORAS

;Selectores de usuarios

;Desc: Función que retorna todos los usuarios registrados dentro del Stack
;Dom: Stack
;Rec: Stack
(define get-users-stack
  (lambda (stack)
    (filter User? stack)))


;Desc: Función que retorna un usuario del stack dado el ID del mismo
;Dom: stack x number
;Rec: stack
(define get-user-by-id
  (lambda (stack userId)
      (filter
        (lambda (lista) (= (get-userid lista) userId))
        (get-users-stack stack))))


;Desc: retorna el usuario solicitado 
;Dom: stack x string
;Rec: stack

(define get-user-by-name
  (lambda (stack username)
    (if (voidStack? stack)
      voidStack
      (if (equal? (get-username (car (get-users-stack stack))) username) ;Comparar primer elemento de la lista de usuarios con "username"
          (car (get-users-stack stack))
          (get-user-by-name (cdr (get-users-stack stack)) username)))))
    

;Desc: Retorna un usuario activo
;Dom: stack
;Rec: stack
(define user-active
    (lambda (stack)
      (if (voidStack? stack)
          voidStack
          (if (userActive? stack (get-username (car (get-users-stack stack))))
              (car (get-users-stack stack))
              (user-active (cdr (get-users-stack stack)))))))


;Desc: Función que retorna la contraseña de un usuario dentro de un stack
;Dom: stack x string
;Rec: stack
(define (get-user-by-password stack username)
  (get-user-password (get-user-by-name stack username)))


;Desc: retorna la reputación de un usuario en el Stack dado su nombre de usuario
;Dom: stack x string
;Rec: stack
(define (get-reputation stack username)
  (get-user-reputation (get-user-by-name stack username)))


;Desc: Función que retorna si un usuario tiene sesión activa en el stack
;Dom: stack x string
;Rec: stack
(define userActive?
  (lambda (stack username)
    (get-user-active-session (get-user-by-name stack username))))


;Selectores de preguntas

;Desc: Función que obtiene un stack con solamente las preguntas dentro de un stack  
;Dom: stack
;Rec: stack
(define get-questions-stack
  (lambda (stack)
    (filter Question? stack)))


;Desc: Función que con un id de pregunta, retorna la pregunta completa
;Dom: stack x number
;Rec: Question
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

;Desc: Elimina un elemento del Stack
;Dom: stack x stack-element
;Rec: stack
(define remove-from-stack
  (lambda (stack stack-element)
    (quitarElementoLista stack stack-element)))

;Desc: Función que cambia el estado de un usuario en el stack
;Dom: string x User
;Rec: stack
(define set-user-active
  (lambda (stack username active)
    (Stack
     (remove-from-stack stack (get-user-by-name stack username))
     (set-session (get-user-by-name stack username) active))))


;Desc: Función que configura la oferta de un usuario en el stack
;Dom: stack x number
;Rec: stack
(define set-user-offer
  (lambda (stack user-id new-offer)
    (Stack
     (remove-from-stack stack (car (get-user-by-id stack user-id)))
     (set-reward-offer (car(get-user-by-id stack user-id)) new-offer))))





; ==========Definiciones para prueba ========================

;usuarios de prueba
(define u1 (User 1 "usuario1" "pass1" 10 0 #f))
(define u2 (User 2 "usuario2" "pass2" 20 0 #f))
(define u3 (User 3 "usuario3" "pass3" 30 0 #f))
;pregutas de prueba
(define q1 (Question 1 "usuario1" (date 1 11 2020) "Pregunta 1" (tag  "tag1" ) 15 #t))
(define q2 (Question 2 "usuario2" (date 2 11 2020) "Pregunta 2" (tag  "tag1" "tag2" ) 0 #t))
(define q3 (Question 3 "usuario3" (date 3 11 2020) "Pregunta 3" (tag  "tag1" "tag2" "tag3") 45 #t))
;respuestas de prueba
(define a1 (Answer 1 1 "usuario1" (date 1 12 2020) "Respuesta 1" (tag  "tag1" "tag2" "tag3")))
(define a2 (Answer 2 2 "usuario2" (date 2 12 2020) "Respuesta 2" (tag  "tag1" "tag2" "tag3")))
(define a3 (Answer 3 3 "usuario3" (date 3 12 2020) "Respuesta 3" (tag  "tag1" "tag2" "tag3")))
;stack de prueba
(define full-stack
  (Stack(Stack(Stack(Stack(Stack(Stack(Stack(Stack(Stack voidStack u1)u2)u3) q1)q2)q3)a1)a2)a3))

  
;"Stack de pruebas"
;full-stack