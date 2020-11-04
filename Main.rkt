#lang racket

(require "User.rkt" "Stack.rkt" "Question.rkt" "Answer.rkt" "otrasFunciones.rkt")


(define userExists?
  (lambda (username stack)
    (memberOf (getUserByName username stack) (get-users-stack stack))))


(define (register stack username password)
  (if (null? stack)
      (Stack (User 1 username password 0) stack)
      (if (userExists? stack username)
          stack
          (Stack (User (+ 1 (length (get-users-stack stack))) username password 0 #f) stack))))


(define ask
  (lambda (Date question stack)
    ;(Question questionId ownerId date questionBody tag)
    (Stack (Question 2 70 Date question (tag 1 1 1)) stack)
    )
  )
    

(define answer
  (lambda (Date questionId stack newAnswer)
    ;(idAnswer idQuestion idUser dateAnswer tagAnswer bodyAnswer)
    (Stack (Answer 1 questionId 60 Date (tag 1 2 3) newAnswer) stack)
    )
  )



(define login
  (lambda (stack username password operation)
    (if (and (userExists? username stack)
             (equal? (getUsernamePassword stack username) password)
             (userActive? username (set-user-active stack username #t))) ; forzar a que se inicie la sesión si se cumple que usuario existe
        (lambda (arg1)
          (lambda (arg2)
            (lambda (arg3)
              (operation arg1 arg2 stack arg3 ))))
        null
    )
  )
)



;((((login stackCompleto "usuario1" "pass1" answer)(date 3 11 2020))5)"mi respuesta")
;(((login stackCompleto "usuario1" "pass1" ask)(date 3 11 2020))"pregunta de prueba")