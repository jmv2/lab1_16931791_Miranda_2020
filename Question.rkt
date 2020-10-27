#lang racket

;TDA Question

(define (Question questionId ownerId date questionBody tag)
  (if (and
       (and (number? questionId)(> questionId 0))
       (and (number? ownerId) (> ownerId 0))
       (and (list? date)(= 3 (length date)))
       (and (string? questionBody))
       (and (list? tag)))
       (list questionId ownerId date questionBody tag)
      '()))

;Pertenencia

(define (Question? question)(list? question))
  

;Selectores

(define (getQuestionId question)
  (car question))

(define (getOwnerQuestionId question)
  (cadr question))

(define (getDateQuestion question)
  (caddr question))

(define (getQuestionBody question)
  (cadddr question))

(define (getQuestionTag question)
  (car (reverse question)))

;Modificadoras

(define (setQuestionId question newId)
  (Question
   newId
   (getOwnerQuestionId question)
   (getDateQuestion question)
   (getQuestionBody question)
   (getQuestionTag question)))



(define (setOwnerQuestionId question newQuestionOwner)
  (Question
   (getQuestionId question)
   newQuestionOwner
   (getDateQuestion question)
   (getQuestionBody question)
   (getQuestionTag question)))


(define (setDateQuestion question newDateQuestion)
  (Question
   (getQuestionId question)
   (getOwnerQuestionId question)
   newDateQuestion
   (getQuestionBody question)
   (getQuestionTag question)))

   
; Preguntas de prueba

(define Qtest1 (Question 1 10 (list 27 10 2020) "¿Qué son las funciones anónimas?" (list "lambda" "anon")))

(define Qtest2 (Question 2 20 (list 17 06 2020) "¿Qué es la recursividad?" (list "recursividad")))

;La siguiente es una lista malformada
(define Qtest3 (Question "1" 20 (list 17 06 2020) "¿Qué es la recursividad?" (list "recursividad")))
