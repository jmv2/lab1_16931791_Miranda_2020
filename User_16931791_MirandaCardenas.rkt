#lang racket

(require "otrasFunciones_16931791_MirandaCardenas.rkt")

(provide User User? get-userid get-username get-user-password get-user-reputation get-user-reward get-user-active-session)
(provide set-id set-password set-user-reputation set-session set-username set-reward-offer)


; Constructor de TDA Usuario

;Desc: Constructor del TDA User
;Dom: number x string x string x number x number x boolean
;Rec: list

(define User
  (lambda (id username password reputation reward-offer activeSession)
    (if (and
        (string? username)
        (and (number? id) (> id 0))
        (and (string? password) (not (equal? username password)))
        (and (number? reputation))
        (and (boolean? activeSession)))
        (cons "U" (cons id (cons username (cons password (cons reputation (cons reward-offer (cons activeSession null)))))))
        null )))

; Función de pertencia
;Desc: Función que indica si el tipo de dato dado es un usuario 
;Dom:User
;Rec:Boolean
(define (User? user)
  (and (list? user)(= 7 (length user))(equal? "U" (car user))))

; Funciones selectoras

;Desc: Función que retorna el ID de un usuario 
;Dom: User
;Rec: number
(define get-userid
  (lambda (user)
    (cadr user)))

;Desc: Función que retorna el nombre de usuario de un usuario
;Dom: User
;Rec: string
(define get-username
  (lambda (user)
    (caddr user)))

;Desc: Función que retorna la constraseña de un usuario dado 
;Dom: User
;Rec: string
(define get-user-password
  (lambda (user)
    (cadddr user)))


;Desc: Función que retorna la reputación de un usuario dado 
;Dom: User
;Rec: number
(define get-user-reputation
  (lambda (user)
    (caddr (reverse user))))

;Desc: Función que retorna lo ofertado a una pregunta. También se entiende como el puntaje retenido 
;Dom: User
;Rec: number
(define get-user-reward
  (lambda (user)
    (cadr (reverse user))))

;Desc: Función que retorna el estado del usuario dentro del stack
;Dom: User
;Rec: Boolean
(define get-user-active-session
  (lambda (user)
    (car (myReverse user))))


; Funciones modificadoras
 
;Desc: Modifica el id del usuario 
;Dom: User
;Rec: User
(define set-id 
  (lambda (user new-id)
    (User
      new-id
      (get-username user)
      (get-user-password user)
      (get-user-reputation user)
      (get-user-reward user)
      (get-user-active-session user))))

;Desc: Modifica el nombre de usuario
;Dom: User
;Rec: User
(define set-username
  (lambda (user new-username)
    (User
      (get-userid user) 
      new-username
      (get-user-password user)
      (get-user-reputation user)
      (get-user-reward user)
      (get-user-active-session user))))

;Desc: Función que modifica la contraseña del usuario
;Dom: User
;Rec: User
(define set-password
  (lambda (user new-password)
    (User 
      (get-userid user)
      (get-username user)
      new-password
      (get-user-reputation user)
      (get-user-reward user)
      (get-user-active-session user))))


;Desc: Función que modifica la reputación de un usuario
;Dom: User
;Rec: User
(define set-user-reputation
  (lambda (user new-reputation)
    (User 
      (get-userid user)
      (get-username user)
      (get-user-password user)
      (get-user-reward user)
      new-reputation
      (get-user-active-session user))))
    

;Desc: Función que cambia el estado de sesión del usuario en el Stack
;Dom: User
;Rec: User
(define set-session
  (lambda (user new-state)
    (User 
      (get-userid user)
      (get-username user)
      (get-user-password user)
      (get-user-reputation user)
      (get-user-reward user)
      new-state)))


;Desc: Función que cambia la recompensa ofertada
;Dom: User
;Rec: User
(define set-reward-offer
  (lambda (user new-offer)
    (User
      (get-userid user)
      (get-username user)
      (get-user-password user)
      (get-user-reputation user)
      new-offer
      (get-user-active-session user))))