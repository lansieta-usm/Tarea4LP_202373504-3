#lang scheme

(define (rotar lista cant_rotaciones) ;; Rota una lista "lista" "cant_rotaciones" veces. En cada rotación, mueve el primer elemento de la lista al final
    (if (= cant_rotaciones 0)
        lista
    ;else
        (rotar (append (cdr lista) (list (car lista))) (- cant_rotaciones 1))
    )
)

(define (evaluarFunciones funciones aEvaluar) ;; Evalua cada función de la lista "funciones" al valor "aEvaluar" en orden
    (if (null? funciones)
        aEvaluar
    ;else 
        (evaluarFunciones (cdr funciones) ((car funciones) aEvaluar))
    )
)

(define (evaluador funciones numeros) ;; Aplica cada rotación y evaluación. Retorna una lista con los resultados de cada evaluación
    (let recursiva ((f funciones) (n numeros) (i 0) (resultado '())) ;; "f" es la lista de funciones, "n" la lista de números a evaluar, "i" el índice actual y "resultado" un acumulador de resultados en orden inverso
        (if (null? n)
            (reverse resultado)
            (let
                ((fnRotada (rotar f i)) (valor (car n))) ;; "fnRotada" es la lista resultante tras rotar la lista de funciones "f" "i" veces. "valor" es el valor actual a evaluar
                (recursiva f (cdr n) (+ i 1) (cons (evaluarFunciones fnRotada valor) resultado)) ;; Llamada recursiva con el siguiente número y acumula el resultado
            )
        )
    )
)

; (evaluador (list (lambda (x) (+ x 1)) (lambda (x) (* x x)) (lambda (x) (- x 2))) '(2 5 7))

; (evaluador '() '())
