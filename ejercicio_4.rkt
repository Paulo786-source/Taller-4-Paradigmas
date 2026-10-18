#lang racket

;; 1. doble
(define (doble lst) (map (lambda (x) (* 2 x)) lst))

;; 2. mayores-que-3
(define (mayores-que-3 lst) (filter (lambda (x) (> x 3)) lst))

;; 3. cuadrados-de-pares
(define (cuadrados-de-pares lst) (map (lambda (x) (* x x)) (filter even? lst)))

;; 4. promedio con foldl
(define (promedio lst) (/ (foldl + 0 lst) (length lst)))

;; 5. suma-cuadrados-impares (pipeline filter -> map -> foldl)
(define (suma-cuadrados-impares lst) (foldl + 0 (map (lambda (x) (* x x)) (filter odd? lst))))

;; 6. sumar con recursión explícita
(define (sumar lst) (if (null? lst) 0 (+ (car lst) (sumar (cdr lst)))))

;; ---- Pruebas ----
(doble '(2 5 7))                        ; '(4 10 14)
(mayores-que-3 '(1 2 3 4 5))            ; '(4 5)
(cuadrados-de-pares '(1 2 3 4 5))       ; '(4 16)
(promedio '(2 4 6))                     ; 4
(suma-cuadrados-impares '(1 2 3 4 5))   ; 35
(sumar '(2 3 4))                        ; 9