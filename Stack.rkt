#lang racket

(require "Usuario.rkt" "main.rkt")

; TDA Stack

(define (Stack stack)
  (lambda (user)
    (list stack user)))





(define testStack (Stack null))