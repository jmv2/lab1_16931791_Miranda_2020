#lang racket

(provide myReverse elementFromList date tag add-tail generic-filter quitarElementoLista memberOf)

;Otras funciones complementarias  transversales. Funciones sin contexto para ser usadas a través del programa.

;Desc: función que dada una lista invierte su orden 
;Dom: lista
;Rec: lista
;Rrecursión: De cola. Motivo: Fue lo mas directo
(define (myReverse L)
  (define myReverseInterno
    (lambda (L1 L2)
      (if (null? L1)
          L2
          (myReverseInterno (cdr L1) (cons (car L1) L2)))))
  (myReverseInterno L '()))

;Desc: Función que retorna un elemento de una lista
;Dom: lista x number
;Rec: lista
;Recursión: De cola. Motivo: Fue lo mas directo, es parecido a un contador con un ciclo for
(define (elementFromList L E)
  (define (elementFromListInternal L E S)
    (if (= E S)
     (car L)
     (elementFromListInternal (cdr L) E (+ 1 S))))
    (elementFromListInternal L E 1))


;Desc: Agrega un elemento E al final (cola) de una lista L
;Dom: lista x elemento
;Rec: lista
;Recursión: Natural. Motivo: Porque fue lo mas directo de realizar.
(define (add-tail L N)
    (if (null? L)
        (cons N null)
        (cons (car L) (add-tail (cdr L) N))))


;Desc: Aplica un criterio f a una lista l, creando nueva lista con los elementos que 
;Dom: funcion x lista
;Rec: lista
;Recursión: Natural. Motivo: Porque fue lo mas directo al momento de pensar la implementación.
(define generic-filter
  (lambda (f l)
    (if (null? l)
        null
        (if (f (car l))
               (cons (car l) (generic-filter f (cdr l)))
               (generic-filter f (cdr l))))))


;Elimina elemento E de lista L
(define (quitarElementoLista L E)
    (if (null? L)
        L
        (if (equal? (car L) E)
            (cdr L)
            (cons (car L) (quitarElementoLista (cdr L) E)))))


;Verifica si el elemento E pertenece a la lista L
;Dom: lista x elemento de la lista
;Rec: Bool
;Recursión: De cola. Motivo: Porque fue lo mas directo al momento de pensar la implementación.
(define memberOf
  (lambda (L E)
    (if (null? L)
        #f
        (if (equal? (car L) E)
            #t
            (memberOf (cdr L) E)))))

;Desc: Función para crear una fecha
;Dom: number x number x number
;Rec: lista
(define date
  (lambda (d m a)
    (cons d (cons m (cons a null)))))

;Desc: Función que crea una lista de tags para unas funciones de prueba
(define tag list)
