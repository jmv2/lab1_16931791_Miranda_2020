#lang racket
(require "otrasFunciones.rkt")
(provide Question Question?  get-question-id get-question-author get-question-date get-question-body get-question-tags get-question-reward set-question-reward)

;TDA Question


;Constructor

(define (Question questionId author-id Date questionBody tag reward)
  (if (and
       (and (number? questionId)(> questionId 0))
       (and (number? author-id) (> author-id 0))
       (and (list? Date) (= 3 (length Date)))
       (and (string? questionBody))
       (and (list? tag))
       (and (number? reward)(>= reward 0)))
       (cons "Q" (cons questionId (cons author-id (cons Date (cons questionBody (cons tag (cons reward null)))))))
      null))

;Pertenencia

(define (Question? question)
  (and (list? question)(= (length question) 7)(equal? "Q" (car question))))
  

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
    (cadddr question)))

; funcion que retorna el contenido de la pregunta

(define get-question-body
  (lambda (question)
    (caddr (reverse question))))

; Función que retorna la lista de los tags de la pregunta

(define get-question-tags
  (lambda (question)
    (cadr (reverse question))))

; retorna la recompensa de la pregunta. Si el valor es cero, no tiene recompensa.
(define get-question-reward
  (lambda (question)
    (car(reverse question))))

;Modificadores

; Modifica el valor de la recompensa de la pregunta

(define set-question-reward
  (lambda (question reward)
    (Question
     (get-question-id question)
     (get-question-author question)
     (get-question-date question)
     (get-question-body question)
     (get-question-tags question)
     reward)))