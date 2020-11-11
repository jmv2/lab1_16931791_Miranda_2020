#lang racket

(require "otrasFunciones_16931791_MirandaCardenas.rkt")

(provide Question Question?  get-question-id get-question-author get-question-date get-question-body get-question-labels get-question-reward)
(provide set-question-reward set-question-state)
;TDA Question


;Constructor

(define (Question questionId author Date questionBody labels reward state)
  (if (and
       (and (string? author))
       (and (list? Date) (= 3 (length Date)))
       (and (string? questionBody))
       (and (list? labels))
       (and (number? reward)(>= reward 0)))
       (cons "Q" (cons questionId (cons author (cons Date (cons questionBody (cons labels (cons reward (cons state null))))))))
      null))

;Pertenencia

(define (Question? question)
  (and (list? question)(= (length question) 8)(equal? "Q" (car question))))
  

;Selectores

;retorna el id de la pregunta
(define get-question-id
  (lambda (question)
    (cadr question)))

;retorna el id del autor de la pregunta
(define get-question-author
  (lambda (question)
    (caddr question)))


; retorna la fecha de la pregunta
(define get-question-date
  (lambda (question)
    (elementFromList question 4))) ; Obtiene el cuarto elemento de la lista

; funcion que retorna el contenido de la pregunta

(define get-question-body
  (lambda (question)
    (cadddr (reverse question))))

; Función que retorna la lista de los tags de la pregunta

(define get-question-labels
  (lambda (question)
    (caddr (reverse question))))

; retorna la recompensa de la pregunta. Si el valor es cero, no tiene recompensa.
(define get-question-reward
  (lambda (question)
    (cadr(reverse question))))

(define get-question-state
  (lambda (question)
    (car (reverse question))))

;Modificadores

; Modifica el valor de la recompensa de la pregunta

(define set-question-reward
  (lambda (question reward)
    (Question
     (get-question-id question)
     (get-question-author question)
     (get-question-date question)
     (get-question-body question)
     (get-question-labels question)
     reward
     (get-question-state question))))



(define set-question-state
  (lambda (question state)
    (Question
     (get-question-id question)
     (get-question-author question)
     (get-question-date question)
     (get-question-body question)
     (get-question-labels question)
     (get-question-reward question)
     state)))