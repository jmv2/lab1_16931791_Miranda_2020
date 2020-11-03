#lang racket

(require "User.rkt" "Stack.rkt")


(define (userExists? stack username)
  (if (< 0(length (getUserByName username stack)))
      #t
      #f))


(define (register stack username password)
  (if (null? stack)
      (Stack (User 1 username password 0) stack)
      (if (userExists? stack username)
          stack
          (Stack (User (length (get-users-stack stack)) username password 0) stack))))