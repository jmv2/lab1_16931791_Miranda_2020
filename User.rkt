#lang racket

(require "otrasFunciones.rkt")
(provide User User? getUserId getUserName getUserPassword getUserReputation)


; Constructor de TDA Usuario

(define (User id username password reputation)
  (if (and
       (string? username)
       (and (number? id) (> id 0))
       (and (string? password) (not (equal? username password)))
       (and (number? reputation) (>= reputation 0)))
       (cons "U" (cons id (cons username (cons password (cons reputation null)))))
       '()))
      

; Función de pertencia

(define (User? user)
  (and (list? user)(= 5 (length user))(equal? "U" (car user))))

; Funciones selectoras

(define (getUserId user)
  (car user))

(define (getUserName user)
  (cadr user))

(define (getUserPassword user)
  (caddr user))

(define (getUserReputation user)
  (cadddr user))


; Funciones modificadoras

(define (setUserId user newId)
  (User
   newId (getUserName user)(getUserPassword user)(getUserReputation user)))


(define (setUserName user newUserName)
  (User  (getUserId user) newUserName (getUserPassword user)(getUserReputation user)))


(define (setUserPassword user newUserPassword)
  (User (getUserId user)(getUserName user)newUserPassword (getUserReputation user)))


(define (setUserReputation user newUserReputation)
  (User (getUserId user)(getUserName user) (getUserPassword user) newUserReputation))

; Sección de usuarios de prueba (predefinidos).

(define pruebaUsuario01
  (User 1 "user01" "pass01" 10))
