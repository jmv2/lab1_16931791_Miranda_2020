#lang racket

(require "otrasFunciones_16931791_MirandaCardenas.rkt")

(provide Question Question?  get-question-id get-question-author get-question-date get-question-body get-question-labels get-question-reward)
(provide set-question-reward set-question-state)
;TDA Question


;Constructor

;Desc: Constructor de TDA Question 
;Dom: number x string x list x string x list x number x boolean
;Rec: Question
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

;Desc: Función de pertenencia de Question 
;Dom: Question
;Rec: Boolean
(define (Question? question)
  (and (list? question)(= (length question) 8)(equal? "Q" (car question))))
  

;Selectores

;Desc: retorna el id de la pregunta
;Dom: question
;Rec: number
(define get-question-id
  (lambda (question)
    (cadr question)))

;Desc: retorna el autor de la pregunta
;Dom: question
;Rec: string
(define get-question-author
  (lambda (question)
    (caddr question)))

;Desc: retorna la fecha de la pregunta
;Dom: question
;Rec: list
(define get-question-date
  (lambda (question)
    (elementFromList question 4))) ; Obtiene el cuarto elemento de la lista


;Desc: funcion que retorna el contenido de la pregunta
;Dom: question
;Rec: string
(define get-question-body
  (lambda (question)
    (cadddr (reverse question))))


;Desc: Función que retorna la lista de los tags de la pregunta
;Dom: question
;Rec: list
(define get-question-labels
  (lambda (question)
    (caddr (reverse question))))


;Desc: retorna la recompensa de la pregunta. Si el valor es cero, no tiene recompensa.
;Dom: question
;Rec: number
(define get-question-reward
  (lambda (question)
    (cadr(reverse question))))


;Desc: Función que retrna el estado un pregunta dada
;Dom: question
;Rec: boolean
(define get-question-state
  (lambda (question)
    (car (reverse question))))

;Modificadores

;Desc: Modifica el valor de la recompensa de la pregunta
;Dom: question
;Rec: question
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

;Desc: función de cambia el estado de una pregunta
;Dom: question
;Rec: question
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