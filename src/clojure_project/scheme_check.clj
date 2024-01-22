(ns clojure-project.scheme-check
  (:require [clojure-project.base_func :refer :all]))

(defn zip
  "Функция берёт на вход несколько списков и создаёт из них список,
  такой, что первый элемент полученного списка содержит кортеж из первых элементов всех списков-аргументов."
  [a b]
  (map vector a b))

(defn validate
  "Функция проверки документа по схеме. Если схема не пустая,
  то применяется первая функция в схеме(tag-check/sequence-check/string-check/xmlsequence-check)"
  [tree scheme]
  (if (empty? scheme)
    (empty? tree)
    (apply
      (first scheme)
      [tree (rest scheme)])))

(defn tag-check
  "Функция проверки корректности тэга. В тэге дерева:
   1) на первом месте должен быть тэг;
   2) на втором месте должно быть имя тэга, совпадающего с указанным в схеме;
   3) в конце указаны данные, которые также должны пройти проверку."
  [tree scheme]
  (and
    (= (first tree) :clojure-project.base_func/tag)
    (= (second tree) (nm (first scheme)))
    (validate (rest (rest tree)) (second scheme))))

(defn sequence-check
  "Функция проводит строгую проверку последовательности тэгов. Учитывается порядок."
  [tree scheme]
  (every? true? (map
                  (partial apply validate)
                  (zip tree scheme))))

(defn string-check
  "Функция проверки данных по типу string"
  [tree scheme]
  (string? (first tree)))

(defn number-check
  "Функция проверки данных по типу number"
  [tree]
  (number? (first tree)))

(defn xmlsequence-check
  "Функция проводит НЕстрогую проверку последовательности тэгов. НЕ учитывается порядок."
  [tree scheme]
  (every? true?
          (map
            (fn [node]
              (some true? (map
                            (partial validate node)
                            scheme)))
            tree)))


