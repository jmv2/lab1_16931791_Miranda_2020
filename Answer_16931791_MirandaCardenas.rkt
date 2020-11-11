#lang racket

(require "otrasFunciones_16931791_MirandaCardenas.rkt")
(provide Answer Answer?)

; TDA Answer

; Constructor

(define Answer
         (lambda (id-answer id-question author answer-date answer labels)
           (if (and
                (and (number? id-answer)(> id-answer 0))
                (and (number? id-question)(> id-question 0))
                (and (string? author))
                (list? answer-date)
                (list? labels)
                (string? answer))
               (cons "A" (cons id-answer (cons id-question (cons author (cons answer-date (cons answer (cons labels null)))))))
               '())))


;Pertenecia

(define (Answer? answer)
  (and (list? answer)(= 7 (length answer))(equal? "A" (car answer))))


;Selectores

;retora el id de la pregunta
(define get-answer-questionid
  (lambda (answer)
  (cadr answer)))

;retornna el id de la respuesta
(define (get-answer-id answer)
  (caddr answer))

; retorna el autor de la respuesta
(define get-answer-author
  (lambda (answer)
    (cadddr answer)))

