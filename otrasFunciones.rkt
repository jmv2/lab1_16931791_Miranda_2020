#lang racket

(provide myReverse elementFromList date tag add-tail generic-filter)

;Otras funciones complementarias  transversales

(define (myReverse L)
  (define myReverseInterno
    (lambda (L1 L2)
      (if (null? L1)
          L2
          (myReverseInterno (cdr L1) (cons (car L1) L2)))))
  (myReverseInterno L '()))

(define date
  (lambda (d m a)
    (cons d (cons m (cons a null)))))


(define tag list)

(define (elementFromList L E)
  (define (elementFromListInternal L E S)
    (if (= E S)
     (car L)
     (elementFromListInternal (cdr L) E (+ 1 S))))
    (elementFromListInternal L E 1)
  )


(define (add-tail L N)
    (if (null? L)
        (cons N null)
        (cons (car L) (add-tail (cdr L) N))))



(define generic-filter
  (lambda (f l)
    (if (null? l)
        null
        (if (f (car l))
               (cons (car l) (generic-filter f (cdr l)))
               (generic-filter f (cdr l))))))
        