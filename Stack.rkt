#lang racket

(require "User.rkt" "Main.rkt" "Question.rkt" "otrasFunciones.rkt")

; TDA Stack

; constructor
(define Stack
  (λ (stackElement stack)
    (if (or
         (User? stackElement)
         (Question? stackElement))
        (cons stackElement stack)
        '())))

;Pertenencia

(define (Stack? stack) (map (λ (L) (or (User? L)(Question? L))) stack))
  






;(define userPrueba1 (User 10 "jmiranda" "robotech" 10))
;(define userPrueba2 (User 11 "mportnoy" "dt2020" 500))
;(define Qtest1 (Question 1 10 (list 27 10 2020) "¿Qué son las funciones anónimas?" (list "lambda" "anon")))
;(define Qtest2 (Question 2 20 (list 17 06 2020) "¿Qué es la recursividad?" (list "recursividad")))
;(Stack (Question 2 20 (date 17 06 2020) "¿Qué es la recursividad?" (tag "recursividad")) (Stack (Question 1 10 (date 27 10 2020) "¿Qué son las funciones anónimas?" (tag "lambda" "anon")) (Stack (User 10 "jmiranda" "robotech" 10) (Stack (User 11 "mportnoy" "dt2020" 500) null))))