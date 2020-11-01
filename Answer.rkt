#lang racket

(require "otrasFunciones.rkt")

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
               (cons "Q" (cons idAnswer (cons idQuestion (cons idUser (cons dateAnswer (cons tagAnswer (cons bodyAnswer null)))))))
               '())))

