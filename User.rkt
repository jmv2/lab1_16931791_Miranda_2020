#lang racket

(require "otrasFunciones.rkt")
(provide User User? get-userid get-username get-user-password get-user-reputation get-user-active-session setUserActiveSession)


; Constructor de TDA Usuario

(define (User id username password reputation activeSession)
  (if (and
       (string? username)
       (and (number? id) (> id 0))
       (and (string? password) (not (equal? username password)))
       (and (number? reputation) (>= reputation 0))
       (and (boolean? activeSession)))
       (cons "U" (cons id (cons username (cons password (cons reputation (cons activeSession null))))))
       null))
      

; Función de pertencia

(define (User? user)
  (and (list? user)(= 6 (length user))(equal? "U" (car user))))

; Funciones selectoras

(define (get-userid user)
  (cadr user))

(define (get-username user)
  (caddr user))

(define (get-user-password user)
  (cadddr user))

(define (get-user-reputation user)
  (cadr (reverse user)))
 
(define (get-user-active-session user)
  (car (myReverse user)))

; Funciones modificadoras

;Modifica el id del usuario 
(define set-id 
  (lambda (user new-id))
    (User
      new-id
      (get-username user)
      (get-user-password user)
      (get-user-reputation user)
      (get-user-active-session user)))

;Modifica el nombre de usuario
(define set-username
  (lambda (user new-username)
    (User
      (get-userid user) 
      new-username
      (get-user-password user)
      (get-user-reputation user)
      (get-user-active-session user))))

;Modifica la contraseña del usuario
(define set-password
  (lambda (user new-password)
    (User 
      (getUserId user)
      (getUserName user)
      new-password
      (getUserReputation user)
      (get-user-active-session user))))


(define set-reputation
  (lambda (user new-reputation)
    (User 
      (getUserId user)
      (getUserName user)
      (getUserPassword user) 
      new-reputation
      (get-user-active-session user)))
    
;Función que cambia el estado de sesión del usuario en el Stack

(define set-session
  (lambda (user new-state)
    (User 
      (getUserId user)
      (getUserName user)
      (getUserPassword user)
      (getUserReputation user)
      new-state)))