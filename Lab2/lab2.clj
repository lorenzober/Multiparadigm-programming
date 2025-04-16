(ns lab2.core
(:require [clojure.string :as str]))
;; === Вхідні дані ===
(def alphabet (vec "abcdefghijklmnopqrstuvwxyz")) ;; Алфавіт із 26
літер
(def alphabet-power 10) ;; Потужність алфавіту
(def num-size 100) ;; Кількість чисел у ряді
(def min-val 1) ;; Мінімальне значення у ряді
(def max-val 1000) ;; Максимальне значення у ряді
;; === Генерація випадкового числового ряду ===
(defn generate-random-list [size min-val max-val]
"Генерує випадковий числовий ряд із заданим розміром та межами"
(repeatedly size #(+ min-val (rand-int (- max-val min-val)))))
;; === Розбиття числового ряду на рівномірні інтервали ===
(defn uniform-segment-ends [min-val max-val count]
"Створює список кінцевих точок інтервалів рівномірного
розподілу"
(let [step (double (/ (- max-val min-val) count))]
(map #(int (+ min-val (* % step))) (range count))))
;; === Визначення індексу інтервалу для числа ===
(defn find-segment-index [num segments]
"Знаходить індекс інтервалу, до якого належить число"
(let [idx (->> segments
(map-indexed vector)
(filter #(<= (second %) num))
last
first)]
(if (nil? idx) 0 idx)))
;; === Перетворення чисел у літери ===
(defn numbers-to-letters [numbers segments alphabet]
"Перетворює числовий ряд у літери відповідно до розподілу
інтервалів"
(map #(nth alphabet (find-segment-index % segments)) numbers))
;; === Побудова матриці передування ===
(defn build-transition-matrix [letters alphabet]
"Будує матрицю передування для лінгвістичного ряду"
(let [matrix (atom (zipmap alphabet (repeat (zipmap alphabet
(repeat 0)))))]
(doseq [[a b] (partition 2 1 letters)]
(swap! matrix update-in [a b] inc))
@matrix))
;; === Основна функція виконання ===
(defn process-sequence []
"Головна функція, що виконує всі етапи обробки даних"
(let [num-list (generate-random-list num-size min-val max-val)
;; Генеруємо випадковий ряд
sorted-list (sort num-list) ;; Сортуємо список
segments (uniform-segment-ends (first sorted-list) (last
sorted-list) alphabet-power) ;; Розбиваємо на інтервали
letters (numbers-to-letters num-list segments alphabet) ;;
Перетворюємо числа у літери
transition-matrix (build-transition-matrix letters
alphabet)] ;; Будуємо матрицю передування
;; Виведення результатів
(println "Числовий ряд:" num-list)
(println "Лінгвістичний ряд:" (str/join " " letters))
(println "Матриця передування:")
(doseq [[key val] transition-matrix]
(println key val))))
;; === Виконання програми ===
(process-sequence)