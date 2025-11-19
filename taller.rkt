#lang racket
 
;Ejercicio 1 – Contar elementos positivos en una lista
(define (contar-positivos lst)
  (length (filter (lambda (x) (> x 0)) lst)))

;Ejercicio 2 – Generar lista de cuadrados pares
(define (cuadrados-pares lst)
  (map (lambda (x) (* x x))
       (filter even? lst)))

;Ejercicio 3 – Calcular el factorial de un número
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))


;Ejercicio 4 – Elevar cada número al cubo
(define (cubos lst)
  (map (lambda (x) (* x x x)) lst))


;Ejercicio 5 – Sumar todos los elementos impares
(define (suma-impares lst)
  (foldl + 0 (filter odd? lst)))


;Ejercicio 6 – Determinar si una lista contiene números negativos
(define (tiene-negativos? lst)
  (ormap (lambda (x) (< x 0)) lst))


;Ejercicio 7 – Calcular la suma acumulada de una lista
(define (suma-acumulada lst)
  (reverse
   (foldl (lambda (x acc)
            (cons (+ x (if (null? acc) 0 (car acc))) acc))
          '()
          lst)))


;Ejercicio 8 – Concatenar cadenas de texto en una lista
(define (concatenar lst)
  (foldl string-append "" lst))


;Ejercicio 9 – Generar lista con el doble de los números mayores que 5
(define (dobles-mayores5 lst)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lst)))


;Ejercicio 10 – Invertir el orden de una lista
(define (invertir lst)
  (foldl (lambda (x acc) (cons x acc)) '() lst))


;Ejercicio 11 – Crear una función que reciba una función como parámetro
(define (aplicar-func f lst)
  (map f lst))

(define (cuadrado x) (* x x))


;Ejercicio 12 – Reto integrador: combinar múltiples funciones
(define (promedio-mayores5 lst)
  (let* ([mayores (filter (lambda (x) (> x 5)) lst)]
         [suma (foldl + 0 mayores)]
         [cantidad (length mayores)])
    (exact->inexact (/ suma cantidad))))



(displayln "============================")
(displayln "RESULTADOS")
(displayln "============================")

(displayln "\nEjercicio 1 – Contar positivos:")
(displayln (contar-positivos '(3 -2 7 0 -5 9)))

(displayln "\nEjercicio 2 – Cuadrados pares:")
(displayln (cuadrados-pares '(1 2 3 4 5 6 7 8)))

(displayln "\nEjercicio 3 – Factorial:")
(displayln (factorial 5))

(displayln "\nEjercicio 4 – Cubos:")
(displayln (cubos '(2 3 4)))

(displayln "\nEjercicio 5 – Suma impares:")
(displayln (suma-impares '(1 2 3 4 5 6 7)))

(displayln "\nEjercicio 6 – ¿Contiene negativos?:")
(displayln (tiene-negativos? '(5 9 -3 2)))

(displayln "\nEjercicio 7 – Suma acumulada:")
(displayln (suma-acumulada '(1 2 3 4)))

(displayln "\nEjercicio 8 – Concatenar cadenas:")
(displayln (concatenar '("Hola" " " "Mundo")))

(displayln "\nEjercicio 9 – Dobles mayores de 5:")
(displayln (dobles-mayores5 '(3 6 8 2 10)))

(displayln "\nEjercicio 10 – Invertir lista:")
(displayln (invertir '(1 2 3 4)))

(displayln "\nEjercicio 11 – Función como parámetro:")
(displayln (aplicar-func cuadrado '(1 2 3 4)))

(displayln "\nEjercicio 12 – Promedio mayores a 5:")
(displayln (promedio-mayores5 '(3 8 10 4 9 2 7)))


