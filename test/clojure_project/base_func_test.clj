(ns clojure-project.base_func_test
  (:require [clojure.test :refer :all]
            [clojure-project.base_func :refer :all]
            [clojure-project.scheme-check :refer :all]))

(def data (tag (nm :note)
               (tag (nm :to) "You")
               (tag (nm :from) "Me")
               (tag (nm :heading) "Reminder")
               (tag (nm :body) "Hi! Hello!")
               (tag (nm :body) "Never Gonna Give You Up")))

(deftest type-test
  (testing "type-test"
    (is (= (get-expr-type data) :clojure-project.base_func/tag))))

(deftest check-type-test
  (testing "check-type-test")
  (is (= (legal-expr? data :clojure-project.base_func/tag) true)))

(deftest expr-value-test
  (testing " expr-value-test1"
    (is (= (expr-value data (get-expr-type data)) (second data))))
  (testing " expr-value-test2"
    (is (= (expr-value (second data) :clojure-project.base_func/name) :note)))
  (testing " expr-value-test-error"
    (is (thrown? Exception (expr-value (second data) :name)))) )

(deftest args-test
  (testing "args-test1"
    (is (= (args (second data)) (list :note))))
  (testing "args-test2"
    (is (= (args (last data)) (list (list :clojure-project.base_func/name :body) "Never Gonna Give You Up"))))
  )

(deftest tags-test
  (testing "tag-content-test1"
    (is (= (first(tag-content data)) (list :clojure-project.base_func/tag
                                       (list :clojure-project.base_func/name :to) "You"))))
  (testing "tag-content-test2"
    (is (= (tag-content (last data))) (list "Never Gonna Give You Up" )))
  (testing "tag-content-test-error"
    (is (thrown? Exception (tag-content (second data)))))
  (testing "tag-name-test1"
    (is (= (tag-name data) :note)))
  (testing "tag-name-test-error"
    (is (= (tag-name (second data)) "")))
  )

(deftest to-str-test
  (testing "to-str-test1"
    (is (= (to-str (second data)) ":note")))
  (testing "to-str-test2"
    (is (= (to-str data) " :note  :to You  :from Me  :heading Reminder  :body Hi! Hello!  :body Never Gonna Give You Up")))
  )

(deftest apply-path-test
  (testing "apply-path-test1"
    (is (= (apply-path data (path :note :body)) (list "Hi! Hello!" "Never Gonna Give You Up"))))
  (testing "apply-path-test2"
    (is (= (apply-path data (path :note :to)) (list "You"))))
  (testing "apply-path-test3"
    (is (= (apply-path data (path :nenote)) ())))
  (testing "apply-path-test4"
    (is (= (apply-path data (path :note :nenote)) ())))
  )

(deftest search-content-test
  (testing "search-content-test1"
    (is (= (search-content data (path :note 0) "You") data)))
  (testing "search-content-test2"
    (is (= (search-content data (path :note 0) "bruh") nil)))
  )
(deftest to-xml-test
  (testing "to-xml-test1"
    (is (= (to-xml data) "<note><to>You</to><from>Me</from><heading>Reminder</heading><body>Hi! Hello!</body><body>Never Gonna Give You Up</body></note>")))
  (testing "to-xml-test2"
    (is (= (to-xml (last data)) "<body>Never Gonna Give You Up</body>")))
  )

