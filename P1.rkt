#lang scheme

(define (buscador lista elemento)
  (let recursiva ((lista_resultante lista) (x 1))
  (cond
    ((null? lista_resultante) -1)
    ((equal? (car lista_resultante) elemento) x)
    (else (recursiva (cdr lista_resultante) (+ x 1))))))

;(buscador '(1 2 3) 3)

;(buscador '(ABC "ABC" 3.0 1234) "ABC")

;(buscador '(ABC "ABC" 3.0 1234) 'ABC)

;(buscador '(389 (2 4 5.0) (40 here 2)) '(40 here 2))

;(buscador '() 'INF253)