#lang racket

(require "User.rkt" "Main.rkt" "Question.rkt" "otrasFunciones.rkt")

; TDA Stack

; constructor
(define Stack
  (λ (user stack)
    (cons user (cons stack '()))))

;Pertenencia

(define (Stack? stack)
    (map
     (λ (L)
       (or (User? L)(Question? L)))
   stack))
  




(define userPrueba1 (User 10 "jmiranda" "robotech" 10))
(define userPrueba2 (User 11 "mportnoy" "dt2020" 500))

(define testStack (Stack userPrueba1 userPrueba2))