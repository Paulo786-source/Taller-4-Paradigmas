#lang racket

;; 1. naturales-desde: stream infinito de números naturales desde n
(define (naturales-desde n)
  (stream-cons n (naturales-desde (add1 n))))

;; 2. tomar: obtiene los primeros n elementos de un stream
(define (tomar s n)
  (if (zero? n)
      '()
      (cons (stream-first s)
            (tomar (stream-rest s) (sub1 n)))))

;; 3. multiplos-de: stream infinito de múltiplos de k
(define (multiplos-de k)
  (stream-map (lambda (n) (* k n))
              (naturales-desde 1)))

;; 4. fibs: stream infinito de la sucesión de Fibonacci
(define fibs
  (let loop ([a 0] [b 1])
    (stream-cons a (loop b (+ a b)))))

;; 5. primo?: predicado auxiliar para decidir si un número es primo
(define (primo? n)
  (cond
    [(< n 2) #f]
    [(= n 2) #t]
    [(even? n) #f]
    [else
     (let loop ([d 3])
       (cond
         [(> (* d d) n) #t]
         [(zero? (remainder n d)) #f]
         [else (loop (+ d 2))]))]))

;; 6. primos: stream infinito de números primos
(define primos
  (stream-filter primo? (naturales-desde 2)))

;; 7. pipeline: cuadrados de los impares desde 1
(define pipeline
  (stream-map sqr (stream-filter odd? (naturales-desde 1))))

;; ---- Pruebas ----
(tomar (multiplos-de 3) 4) ; '(3 6 9 12)
(tomar fibs 6)             ; '(0 1 1 2 3 5)
(tomar primos 5)           ; '(2 3 5 7 11)
(tomar pipeline 4)         ; '(1 9 25 49)