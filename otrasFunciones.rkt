#lang racket

(provide myReverse date tag)

;Otras funciones complementarias  transversales

(define (myReverse L)
  (define myReverseInterno
    (λ (L1 L2)
      (if (null? L1)
          L2
          (myReverseInterno (cdr L1) (cons (car L1) L2)))))
  (myReverseInterno L '()))

(define date
  (λ (d m a)
    (cons d (cons m (cons a null)))))


(define tag list)