#lang racket

(provide myReverse elementFromList date tag add-tail generic-filter quitarElementoLista memberOf)

;Otras funciones complementarias  transversales

; Se revierte el orden de los elementos de la lista L
(define (myReverse L)
  (define myReverseInterno
    (lambda (L1 L2)
      (if (null? L1)
          L2
          (myReverseInterno (cdr L1) (cons (car L1) L2))
          )
      )
    )
  (myReverseInterno L '()))


; Obtiene el elemento numero E de la lsta L
(define (elementFromList L E)
  (define (elementFromListInternal L E S)
    (if (= E S)
     (car L)
     (elementFromListInternal (cdr L) E (+ 1 S))))
    (elementFromListInternal L E 1)
  )


;Agrega un elemento E al final (cola) de una lista L
(define (add-tail L N)
    (if (null? L)
        (cons N null)
        (cons (car L) (add-tail (cdr L) N))))


; Aplica un criterio f a una lista l, creando nueva lista con los elementos que 

(define generic-filter
  (lambda (f l)
    (if (null? l)
        null
        (if (f (car l))
               (cons (car l) (generic-filter f (cdr l)))
               (generic-filter f (cdr l))))))


;Elimina elemento de lista
(define (quitarElementoLista L E)
    (if (null? L)
        L
        (if (equal? (car L) E)
            (cdr L)
            (cons (car L)(quitarElementoLista (cdr L) E)))))


;Verifica si el elemento E pertenece a la lista L

(define memberOf
  (lambda (E L)
    (if (null? L)
        #f
        (if (equal? (car L) E)
            #t
            (memberOf E (cdr L))))))


(define date
  (lambda (d m a)
    (cons d (cons m (cons a null)))))


(define tag list)