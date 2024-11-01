#lang scheme

(define (exponenciacion base exponente)
    (define (auxiliar base exponente resultado)
        (if (= exponente 0)
            resultado
        ;else
            (auxiliar base (- exponente 1) (* resultado base))
        )
    )
    (auxiliar base exponente 1)
)

(define (factorial n)
    (if (= n 0)
        1
    ;else
    (* n (factorial (- n 1)))
    )
)

(define (taylorSenoSimple n x)
  (let recursivaSin ((i 0))
    (if (= i n)
        0
        (let* (
            (signo (if (even? i) 1 -1))
            (exponente (+ (* 2 i) 1))
            (actual (/ (* signo (exponenciacion x exponente)) (factorial exponente)))
            )
          (+ actual (recursivaSin (+ i 1)))))))

(taylorSenoSimple 300 3.14)