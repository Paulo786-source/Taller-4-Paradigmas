#lang racket

;; 1. Datos base
(define entradas '((sopa 3) (ensalada 4) (nachos 5)))
(define platos '((pollo 8) (pasta 7) (carne 10) (vegetariano 6)))
(define postres '((helado 3) (pastel 4) (fruta 2)))

;; 2. Presupuesto máximo
(define PRESUPUESTO 15)

;; 3. item-nombre / item-costo: accesores para cada ítem
(define (item-nombre it) (first it))
(define (item-costo it) (second it))

;; 4. todos-los-menus: generación completa de combinaciones
(define (todos-los-menus)
  (for*/list ([e entradas]
              [p platos]
              [d postres])
    (list (item-nombre e)
          (item-nombre p)
          (item-nombre d)
          (+ (item-costo e)
             (item-costo p)
             (item-costo d)))))

;; 5. juntar: concatena una lista de listas (apoyo para la búsqueda)
(define (juntar listas)
  (if (null? listas)
      '()
      (append (car listas) (juntar (cdr listas)))))

;; 6. buscar: búsqueda con poda; solo sigue ramas que cumplen criterio
(define (buscar criterio)
  (let loop ([etapas (list entradas platos postres)]
             [parcial '()]
             [costo 0])
    (if (null? etapas)
        (list (append (reverse parcial) (list costo)))
        (juntar
         (map (lambda (item)
                (define nuevo-parcial
                  (cons (item-nombre item) parcial))
                (define nuevo-costo
                  (+ costo (item-costo item)))
                (if (criterio nuevo-costo nuevo-parcial)
                    (loop (cdr etapas) nuevo-parcial nuevo-costo)
                    '()))
              (car etapas))))))

;; 7. menu-costo-ok?: menú completo válido solo por costo
(define (menu-costo-ok? m)
  (<= (fourth m) PRESUPUESTO))

;; 8. costo-ok?: criterio parcial para poda por presupuesto
(define (costo-ok? costo parcial)
  (<= costo PRESUPUESTO))

;; 9. menus-por-costo: punto 1, generación completa filtrada por costo
(define menus-por-costo
  (filter menu-costo-ok? (todos-los-menus)))

;; 10. menus-bt-por-costo: punto 2, backtracking con poda por costo
(define menus-bt-por-costo
  (buscar costo-ok?))

;; 11. incompatible-parcial?: detecta la combinación nachos + pasta
(define (incompatible-parcial? parcial)
  (and (memq 'nachos parcial)
       (memq 'pasta parcial)
       #t))

;; 12. ok?: criterio final de poda: presupuesto + incompatibilidad
(define (ok? costo parcial)
  (and (costo-ok? costo parcial)
       (not (incompatible-parcial? parcial))))

;; 13. menu-final-ok?: menú completo válido con costo e incompatibilidad
(define (menu-final-ok? m)
  (and (menu-costo-ok? m)
       (not (and (eq? (first m) 'nachos)
                 (eq? (second m) 'pasta)))))

;; 14. menus-validos: punto 3, menús válidos con restricción final
(define menus-validos
  (filter menu-final-ok? (todos-los-menus)))

;; 15. menus-validos-bt: backtracking con ok? y restricción final
(define menus-validos-bt
  (buscar ok?))

;; ---- Pruebas ----
;; Generación completa y backtracking solo por costo (deben coincidir)
(length menus-por-costo)                      ; 24
(equal? menus-por-costo menus-bt-por-costo)   ; #t

;; Menús válidos finales con incompatibilidad (deben coincidir)
menus-validos
menus-validos-bt
(length menus-validos)                        ; 22
(equal? menus-validos menus-validos-bt)       ; #t

#|
¿Por qué la versión con poda evalúa menos combinaciones?

La generación completa construye primero todas las combinaciones posibles de
entrada, plato y postre, y después descarta las que superan el presupuesto
o incumplen restricciones. En cambio, la búsqueda con poda revisa el costo
acumulado en cada paso. Como todos los costos son positivos, si una rama ya
supera 15, ninguna extensión futura podrá volverla válida, por lo que se
descarta de inmediato y no se exploran sus extensiones. Además, la
incompatibilidad entre nachos y pasta se detecta apenas ambos ítems aparecen
en la rama parcial, evitando probar postres para esa combinación. Así se
evita construir y evaluar ramas completas que de todos modos serían
rechazadas.
|#