#lang racket

(require "otrasFunciones.rkt")
(provide Answer Answer?)

; TDA Answer

; Constructor

(define Answer
         (λ (idAnswer idQuestion idUser dateAnswer tagAnswer bodyAnswer)
           (if (and
                (and (number? idAnswer)(> idAnswer 0))
                (and (number? idQuestion)(> idQuestion 0))
                (list? dateAnswer)
                (list? tagAnswer)
                (string? bodyAnswer))
               (cons "A" (cons idAnswer (cons idQuestion (cons idUser (cons dateAnswer (cons tagAnswer (cons bodyAnswer null)))))))
               '())))


;Pertenecia

(define (Answer? answer)
  (and (list? answer)(= 7 (length answer))(equal? "A" (car answer))))


;Selectores

(define (get-idQuestion answer)
  (cadr answer))