#lang racket

(provide User User?)


; Constructor de TDA Usuario

(define (User id username password reputation)
  (list id username password reputation))


; Función de pertencia

(define (User? user)
    (if (and
         (list? user)
         (number? (getUserId user))
         (string? (getUsername user))
         (string? (getUserPassword user))
         (number? (getUserReputation user)))
        #t
        #f))


; Funciones selectoras

(define (getUserId user)
  (car user))

(define (getUsername user)
  (cadr user))

(define (getUserPassword user)
  (caddr user))

(define (getUserReputation user)
  (cadddr user))


; Funciones modificadoras



; Sección de usuarios de prueba (predefinidos).

(define pruebaUsuario01
  (User 1 "user01" "pass01" 10))
