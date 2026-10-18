#lang racket

;; 1. Struct transparente de un curso
(struct curso (codigo creditos semestre) #:transparent)

;; 2. Lista de al menos 6 cursos
(define cursos
  (list (curso "CE1101" 3 1)
        (curso "MA1102" 4 1)
        (curso "CE1103" 4 2)
        (curso "FI1101" 3 2)
        (curso "CE1106" 4 3)
        (curso "MA2105" 4 3)))

;; 3. codigo: lista con el código de todos los cursos (map)
(define (codigo lst)
  (map curso-codigo lst))

;; 4. del-semestre: cursos de un semestre dado (filter)
(define (del-semestre lst sem)
  (filter (lambda (c) (= (curso-semestre c) sem)) lst))

;; 5. total-creditos: suma de créditos (foldl)
(define (total-creditos lst)
  (foldl (lambda (c acc) (+ (curso-creditos c) acc)) 0 lst))

;; 6. con-creditos: nueva lista donde el curso con ese código
;;    tiene otros créditos (struct-copy). La original no cambia.
(define (con-creditos lst cod nuevos)
  (map (lambda (c)
         (if (string=? (curso-codigo c) cod)
             (struct-copy curso c [creditos nuevos])
             c))
       lst))

;; ---- Pruebas ----
(codigo cursos)
(del-semestre cursos 2)
(total-creditos cursos)
(define cursos2 (con-creditos cursos "CE1103" 5))
cursos2
cursos   ; la lista original sigue igual