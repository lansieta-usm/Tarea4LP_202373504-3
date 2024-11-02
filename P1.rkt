#lang scheme

(define (buscador lista elemento) ;; Busca al elemento "elemento" dentro de la lista "lista" y retorna su posición... Si el elemento no está, retorna -1
    (let recursiva ((listaResultante lista) (i 1)) ;; Implementa la lógica de la búsqueda recursiva, listaResultante es la lista tras una iteración e i es un contador de iteraciones
        (cond
            ((null? listaResultante) -1)
            ((equal? (car listaResultante) elemento) i)
            (else (recursiva (cdr listaResultante) (+ i 1)))
        )
    )
)

; (buscador '(1 2 3) 3)

; (buscador '(ABC "ABC" 3.0 1234) "ABC")

; (buscador '(ABC "ABC" 3.0 1234) 'ABC)

; (buscador '(389 (2 4 5.0) (40 here 2)) '(40 here 2))

; (buscador '() 'INF253)