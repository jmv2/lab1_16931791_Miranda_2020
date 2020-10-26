#lang racket

(provide User User? getUserId getUserName getUserPassword getUserReputation)


; Constructor de TDA Usuario

(define (User id username password reputation)
  (list id username password reputation))


; Función de pertencia

(define (User? user)
    (if (and
         (list? user)
         (number? (getUserId user))
         (string? (getUserName user))
         (string? (getUserPassword user))
         (number? (getUserReputation user)))
        #t
        #f))


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
  (list newId (getUserName user)(getUserPassword user)(getUserReputation user)))


(define (setUserName user newUserName)
  (list  (getUserId user) newUserName (getUserPassword user)(getUserReputation user)))


(define (setUserPassword user newUserPassword)
  (list (getUserId user)(getUserName user)newUserPassword (getUserReputation user)))


(define (setUserReputation user newUserReputation)
  (list (getUserId user)(getUserName user) (getUserPassword user) newUserReputation))

; Sección de usuarios de prueba (predefinidos).

(define pruebaUsuario01
  (User 1 "user01" "pass01" 10))
