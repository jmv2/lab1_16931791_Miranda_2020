#lang racket

(require "User.rkt" "Question.rkt" "Answer.rkt" "otrasFunciones.rkt")
(provide Stack Stack? get-users-stack getUserById getUserByName)
(provide stackCompleto)

; TDA Stack
(define voidStack null)
; constructor
(define Stack
  (lambda (stackElement stack)
    (if (or (User? stackElement)(Question? stackElement)(Answer? stackElement))
        (add-tail stack stackElement)
        stack )))

            
            
;Pertenencia

(define (Stack? stack)
  (list? stack))


;Selectores
  
(define get-users-stack
  (lambda (stack)
    (generic-filter User? stack)))

(define (getUserById userId stack)
    (filter
     (lambda (lista) (= (getUserId lista) userId))
     (get-users-stack stack)))

(define (getUserByName userName stack)
    (filter
     (lambda (lista) (equal? (getUserName lista) userName))
     (get-users-stack stack)))


(define get-questions-stack
  (lambda (stack)
    (generic-filter Question? stack)))

(define get-answers-stack
  (lambda (stack)
    (generic-filter Answer? stack)))



; ==========Definiciones para prueba ========================

;usuarios de prueba
(define u1 (User 1 "usuario1" "pass1" 10))
(define u2 (User 2 "usuario2" "pass2" 20))
(define u3 (User 3 "usuario2" "pass2" 30))
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

(define stackCompleto
  (Stack u1 (Stack q1 (Stack a1 (Stack u2 (Stack q2 (Stack a2 (Stack u3 (Stack q3 (Stack a3 voidStack))))))))))

;"StackOver Flow"
;stackCompleto