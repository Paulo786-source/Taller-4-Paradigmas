#lang racket

;; 1. Struct transparente de un curso
(struct curso (codigo creditos semestre) #:transparent)

;; 2. Lista de cursos para pruebas
(define mis-cursos
  (list (curso "CE1101" 3 1)
        (curso "MA1102" 4 1)
        (curso "CE1103" 4 2)
        (curso "FI1101" 3 2)
        (curso "CE1106" 4 3)
        (curso "MA2105" 4 3)))

;; 3. mi-map: map propio
;; Contrato: (All (A B) (-> (-> A B) (Listof A) (Listof B)))
(define (mi-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (mi-map f (cdr lst)))))

;; 4. mi-foldl: foldl propio con el mismo orden de argumentos que foldl
;; Contrato: (All (A B) (-> (-> A B B) B (Listof A) B))
(define (mi-foldl f acc lst)
  (if (null? lst)
      acc
      (mi-foldl f (f (car lst) acc) (cdr lst))))

;; 5. contar-si: cuenta elementos que cumplen un predicado usando mi-foldl
;; Contrato: (All (A) (-> (-> A Boolean) (Listof A) Natural))
(define (contar-si pred lst)
  (mi-foldl (lambda (x acc)
              (if (pred x)
                  (add1 acc)
                  acc))
            0
            lst))

;; ---- Pruebas ----
;; Enteros
(mi-map add1 '(1 2 3))       ; '(2 3 4)
(mi-foldl + 0 '(1 2 3 4))    ; 10
(contar-si even? '(1 2 3 4)) ; 2

;; Strings
(mi-map string-length '("a" "abc")) ; '(1 3)
(mi-foldl (lambda (s acc) (string-append acc s))
          ""
          '("a" "b" "c")) ; "abc"
(contar-si (lambda (s) (> (string-length s) 0))
           '("" "x" "yz")) ; 2

;; Cursos
(mi-map curso-codigo mis-cursos)
(mi-foldl (lambda (c acc) (+ (curso-creditos c) acc))
          0
          mis-cursos)
(contar-si (lambda (c) (= (curso-semestre c) 1))
           mis-cursos)