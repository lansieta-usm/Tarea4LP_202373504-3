#lang scheme

(define (profundidades arbol) ;; Dado un árbol con la estructura dada en el enunciado, retorna una lista con las profundidades de todos los nodos 'T' que existan... Dicha lista está ordenada crecientemente
    (define (encontrarTesoros subarbol profundidad) ;; Función auxiliar que recorre un subarbol y acumula las profundidades de los tesoros
        (cond
            ((null? subarbol) '())
            ((not (list? subarbol))
                (if (equal? subarbol 'T)
                    (list profundidad)
                ;else
                    '()
                )
            )
            (else
                (let
                    ((depthList
                        (if (equal? (car subarbol) 'T)
                            (list profundidad)
                        ;else
                            '()
                        )
                    ))
                    (append depthList
                        (apply append (map (lambda (subarbolHijo) (encontrarTesoros subarbolHijo (+ profundidad 1))) (cdr subarbol)))
                    )
                )
            )
        )
    )
    (sort (encontrarTesoros arbol 0) <)
)

; (profundidades '(1 (6 (3) (2 (5))) (4 (7 (8) (9)))))

; (profundidades '(1 (2 (3 (T (T) (T)) (T (T) (T))))))

; (profundidades '())
