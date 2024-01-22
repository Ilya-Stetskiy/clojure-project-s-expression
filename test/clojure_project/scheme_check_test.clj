(ns clojure-project.scheme-check-test
  (:require [clojure.test :refer :all]
            [clojure-project.base_func :refer :all]
            [clojure-project.scheme-check :refer :all])
  (:import (java.io FileNotFoundException)))

(defn read-file [filename]
  (try
    (let [file-contents (slurp filename)]
      (load-string file-contents))
    (catch FileNotFoundException e
         (println "Файл не существует")
    )
    (catch RuntimeException e
      (println "Некорректный формат файла")
      )
    ))

;(def invalid_file (read-file "test\\clojure_project\\data-for-test\\data\\i.jpg"))

(def tree1 (read-file "test/clojure_project/data-for-test/data/data1.txt"))
(def scheme1 (read-file "test/clojure_project/data-for-test/scheme/scheme1.txt"))

(def tree2 (read-file "test/clojure_project/data-for-test/data/data2.txt"))

(def tree3 (read-file "test/clojure_project/data-for-test/data/data3.txt"))
(def tree3a (tag (nm :note)
                 (tag (nm :to) "You")
                 (tag (nm :from) "Me")
                 (tag (nm :heading) "Reminder")
                 (tag (nm :body) "Hi! Hello!")
                 (tag (nm :body) "Never Gonna Give You Up")))
(def scheme3 (read-file "test/clojure_project/data-for-test/scheme/scheme3.txt"))

(def tree4 (read-file "test/clojure_project/data-for-test/data/data4.txt"))
(def scheme4 (read-file "test/clojure_project/data-for-test/scheme/scheme4.txt"))

(def tree5 (read-file "test/clojure_project/data-for-test/data/data5.txt"))
(def scheme5 (read-file "test/clojure_project/data-for-test/scheme/scheme5.txt"))

(def tree6 (read-file "test/clojure_project/data-for-test/data/data6.txt"))
(def scheme6 (read-file "test/clojure_project/data-for-test/scheme/scheme6.txt"))

(def tree7 (read-file "test/clojure_project/data-for-test/data/data7.txt"))

(deftest read-test
  (testing "чтение из файла" )
  (is (= tree3 tree3a)))
(deftest zip-test
  (testing "Zip-функция пустых списков"
  (is (= (zip '() '()) '())))

  (testing "Zip-функция списков одинаковой длины")
  (is (= (zip [1 2 3] ["a" "b" "c"])
         (list [1 "a"] [2 "b"] [3 "c"])))

  (testing "Zip-функция списков разной длины")
  (is (= (zip [1 2 3] ["a" "b"])
         (list [1 "a"] [2 "b"])))
  )

(deftest validate-test
  (testing "Тест, если схема и дерево пустые")
  (is (= (validate () ()) true))

  (testing "Тест, если дерево построено по схеме")
  (is (= (validate tree1 scheme1) true))

  (testing "Тест, если в дереве неправильное имя")
  (is (= (validate tree2 scheme1) false))

  (testing "Тест, если дерево построено по схеме cо строгой последовательностью тэгов sequence-check")
  (is (= (validate tree3 scheme3) true))

  (testing "Тест, если дерево построено по схеме c нестрогой последовательностью xmlsequence-check (порядок совпадает)")
  (is (= (validate tree3 scheme4) true))

  (testing "Тест, если дерево построено по схеме c sequence-check, но с порядком для xmlsequence-check")
  (is (= (validate tree4 scheme3) false))

  (testing "Тест, если дерево построено по схеме c xmlsequence-check (порядок не совпадает")
  (is (= (validate tree4 scheme4) true)))

(deftest tag-check-test
  (testing "Тест, если тэги в порядке")
  (is (= (tag-check tree1 scheme5) true))

  (testing "Тест, если имя тэга не совпадает со схемой")
  (is (= (tag-check tree2 scheme5) false))

  (testing "Тест, если в середине документа есть несоответствие тэгов")
  (is (= (tag-check tree5 scheme5) false)))

(deftest sequence-check-test
  (testing "Тест, если последовательность тэгов такая же, как в схеме")
  (is (= (sequence-check tree6 scheme6) true))

  (testing "Тест, если последовательность тэгов не совпадает со схемой")
  (is (= (sequence-check tree7 scheme6) false)))

(deftest xmlsequence-check-test
  (testing "Тест, если последовательность тэгов такая же, как в схеме")
  (is (= (xmlsequence-check tree6 scheme6) true))

  (testing "Тест, если последовательность тэгов не совпадает со схемой")
  (is (= (xmlsequence-check tree7 scheme6) true)))

(deftest string-check-test
  (testing "Тест, если данные пустые")
  (is (= (string-check (list "") (list "")) true))

  (testing "Тест, если данные являются строкой")
  (is (= (string-check (list "ЗАГОЛОВОК") (list "")) true))

  (testing "Тест, если данные не являются строкой")
  (is (= (string-check (list 2) (list "")) false)))

(deftest  number-check-test
  (testing "Тест, если данные пустые")
  (is (= (number-check (list "")) false))

  (testing "Тест, если данные являются строкой")
  (is (= (number-check (list "ЗАГОЛОВОК")) false))

  (testing "Тест, если данные являются числом")
  (is (= (number-check (list 2)) true)))







