#lang scheme

(define (exponenciacion base exponente) ;; Recibe una base y un exponente... Retorna base^exponente
    (define (recursivaExp base exponente resultado) ;; Implementa la lógica de la exponenciación de manera recursiva
        (if (= exponente 0)
            resultado
        ;else
            (recursivaExp base (- exponente 1) (* resultado base))
        )
    )
    (recursivaExp base exponente 1)
)

(define (factorial n) ;; Recibe un número y lo convierte en su valor factorial
    (if (= n 0)
        1
    ;else
        (* n (factorial (- n 1)))
    )
)

(define (taylorSenoSimple n x) ;; Implementa la serie de Taylor del seno de "x" desde 0 hasta "n" 
    (let recursivaSin ((i 0)) ;; Implementa la lógica de la sumatoria usando recursión simple, "i" es el número de iteraciones que se incrementa hasta llegar a "n"
        (if (= i n)
            (let* (
                (signo (if (even? i) 1 -1))
                (exponente (+ (* 2 i) 1))
                (actual (/ (* signo (exponenciacion x exponente)) (factorial exponente)))
                )
                actual
            )
        ;else
            (let* (
                (signo (if (even? i) 1 -1))
                (exponente (+ (* 2 i) 1))
                (actual (/ (* signo (exponenciacion x exponente)) (factorial exponente)))
                )
                (+ actual (recursivaSin (+ i 1)))
            )
        )
    )
)

(define (taylorCosenoCola n x) ;; Implementa la serie de Taylor del coseno de "x" desde 0 hasta "n"
    (let recursivaCos ((resultado 0) (i 0)) ;; Implementa la lógica de la sumatoria usando recursión de cola, "i" es el número de iteraciones que se incrementa hasta llegar a "n"
        (let* (
            (signo (if (even? i) 1 -1))
            (exponente (* 2 i))
            (actual (/ (* signo (exponenciacion x exponente)) (factorial exponente)))
            )
            (if (= i n)
                (+ resultado actual)
            ;else
                (recursivaCos (+ resultado actual) (+ i 1))
            )
        )
    )
)

; (taylorSenoSimple 300 3.14)

; (taylorCosenoCola 300 3.14)

; (taylorSenoSimple 1 2.14)

; (taylorCosenoCola 1 2.14)

; (taylorSenoSimple 0 20.3)

; (taylorCosenoCola 0 20.3)
