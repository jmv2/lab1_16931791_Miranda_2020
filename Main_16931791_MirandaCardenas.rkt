#lang racket

(require "Stack_16931791_MirandaCardenas.rkt")
(require "User_16931791_MirandaCardenas.rkt")
(require "Answer_16931791_MirandaCardenas.rkt")
(require "Question_16931791_MirandaCardenas.rkt")
(require "otrasFunciones_16931791_MirandaCardenas.rkt")

(define default-reward 0);  al momento de crear un nueva pregunta su recomensa es 0
(define default-active-session #f); al momento de crear un usuario el estado de su sesión en inactivo: #f
(define question-open #t)
(define question-close #f)
(define no-offer 0)


; Función que deslogue al usuario
; Dom : stack
; Rec : stack
(define logout
  (lambda (stack)
    (set-user-active stack (get-username (user-active stack)) #f )))
    

;Desc: Función que indica si existe un usuario dentro de un stack, dado un nombre de usuario
;Dom: stack x string
;Rec: boolean
(define userExists?
  (lambda (stack username)
    (memberOf
     (get-users-stack stack) ; Lista de usuarios del stack
     (get-user-by-name stack username))))


;Desc: Función que indica si existe una pregunta en un stack, dado un nombre de usuario
;Dom: stack x number
;Rec: stack
(define questionExists?
  (lambda (stack id-question)
    (memberOf
     (get-questions-stack stack)
     (get-question-by-id stack id-question))))


;Desc: Función que permite crear un nuevo usuario dentro de un stack
;Dom: stack x string x string
;Rec: stack
(define (register stack username password)
  (if (null? stack)
      (Stack stack (User 1 username password 0 0 default-active-session))
      (if (userExists? stack username)
          stack
          (Stack stack (User (+ 1 (length (get-users-stack stack))) username password 0 0 #f)))))

;Desc: Función que permite a un usuario iniciar sesión dentro de un stack
;Dom: stack x string x string x function
;Rec: function
;Rec final: stack
(define login
  (lambda (stack username password operation)
    (if (and (userExists? stack username)
             (equal? (get-user-by-password stack username) password))
        (operation (set-user-active stack username #t))
        operation )))


;Desc:  Función que permite crear una nueva pregunta dentro del stack
;Dom: stack 
;Rec: function: list ->stack X date X string X string list
;Rec final: stack
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



;Desc: Función que permite a un usuario ofrecer una recompensa a una pregunta
;Dom: stack
;Rec: 
;Rec final:
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
            
;Desc: Función que permite a un usurio crear una nueva pregunta en un stack
;Dom: stack
;Rec: function: list ->stack X date X string X string list
;Rec final: stack
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

;Desc: Función que resta el puntaje a la reputación 
;Dom: stack x string
;Rec: stack
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
        (- (get-user-reputation user)
           (get-user-reward user))
        no-offer
        (get-user-active-session user))))))

;Desc: Función que busca un usuario que haya ofrecido una recompesa 
;Dom: stack x number
;Rec: user
(define debtor
  (lambda (stack id-question)
    (if (null? stack)
        null
        (if (equal?
             (get-user-reward (car ( get-users-stack stack)))
             id-question)
            (car (get-users-stack stack))
            (debtor (cdr (get-users-stack stack)) id-question)))))


;Desc: Función que permite al dueño de la pregunta, aceptar una respuesta, cambiar de estado de la pregunta y pagar  la recompensa
;Dom: stack
;Rec: function: integer (IDPregunta) X integer (IDRespuesta)
;Rec final: stack
(define accept
  (lambda (stack)
    (lambda (id-question)
      (lambda (id-answer)
        (if (equal? (get-username (user-active stack)) (get-question-author(get-question-by-id stack id-question))) ; Sólo el autor de la pregunta puede continuar
            (logout(payment  
                (Stack
                    (Stack
                       (remove-from-stack stack (get-question-by-id stack id-question))
                       (set-question-state (get-question-by-id stack id-question) question-close)
                    )
                    (remove-from-stack stack (user-active stack))
                )
               (get-username (debtor stack (get-question-reward (get-question-by-id stack id-question))))))
              (logout stack))
             ))))
            
            


#| Funciones de prueba para el informe

(define stack-informe
  (register
   (register voidStack "jmiranda" "pass") "acardenas" "pass2"))

(define stack-con-reward
  (((login full-stack "usuario1" "pass1" reward)2)5))

|#
  