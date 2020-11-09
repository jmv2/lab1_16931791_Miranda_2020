#lang racket

(require "User.rkt" "Stack.rkt" "Question.rkt" "Answer.rkt" "otrasFunciones.rkt")
(define default-reward 0);  al momento de crear un nueva pregunta su recomensa es 0
(define default-active-session #f); al momento de crear un usuario el estado de su sesión en inactivo: #f
(define question-open #t)
(define question-close #f)
(define no-offer 0)

(define logout
  (lambda (stack)
    (set-user-active stack (get-username (user-active stack)) #f )))
    

(define userExists?
  (lambda (stack username)
    (memberOf
     (get-users-stack stack) ; Lista de usuarios del stack
     (get-user-by-name stack username))))

(define questionExists?
  (lambda (stack id-question)
    (memberOf
     (get-questions-stack stack)
     (get-question-by-id stack id-question))))


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
              (get-username (user-active stack))
              fecha-pregunta
              pregunta
              labels
              default-reward
              question-open)))))))




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
            (logout stack))))))
            
       

(define answer
  (lambda (stack)
    (lambda (answer-date)
      (lambda (id-question) ; es el ID de la pregunta que se quiere responder
        (lambda (text-answer . labels) ; Texto de la respuesta 
         (if (questionExists? stack id-question)
          (logout
           (Stack
            stack
            (Answer
             (+ 1 (length (get-answers-stack stack)))
             id-question
             (get-username (user-active stack))
             answer-date
             text-answer
             labels)))
          (logout stack))))))) ; realiza logout del usuario en caso de que el id de preguta no exista


(define payment 
  (lambda (stack username)
    (let
      ([user (get-user-by-name stack username)])
      (Stack
       (remove-from-stack stack user) ; Stack sin el usuario
       (User
        (get-userid user)
        (get-username user)
        (get-user-password user)
        (- (get-user-reward user)
           (get-user-reputation user))
        no-offer
        (get-user-active-session user))))))


(define accept
  (lambda (stack)
    (lambda (id-question)
      (lambda (id-answer)
        (if (equal? (get-username (user-active stack)) (get-question-author(get-question-by-id stack id-question)))
            (logout
             (Stack
              (Stack
               (Stack
                (remove-from-stack stack (get-question-by-id stack id-question))
                (set-question-state (get-question-by-id stack id-question) question-close)) ;Aqui se da por solucionada la pregunta
               (payment stack  (get-question-author (get-question-by-id stack id-question))))
              (remove-from-stack stack (user-active stack))))
            (logout stack))))))



;Función que descontará los puntos del usuario y retornará un stack modificado.

     

;(set-reward-offer(set-user-reputation (get-user-by-name stack (get-question-author (get-question-by-id stack id-question))) ; Usuario de la recompensa
 ;                                                    ( - (get-user-reputation (get-user-by-name stack (get-question-author (get-question-by-id stack id-question))))
  ;                                                       (get-user-reward (get-user-by-name stack (get-question-author (get-question-by-id stack id-question))))))no-offer