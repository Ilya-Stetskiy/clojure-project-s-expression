(ns clojure-project.base_func)
(use '[clojure.string :only (replace-first)])

; Utils
(defn get-expr-type
  "Получение типа. [expr] - узел, тип которого нужно получить"
  [expr]
  (if (seq? expr) (first expr) ::value))

(defn legal-expr?
  "проверка соответвия типов узла  где [expr] - проверяемое выражение, [expr-type] - необходимый тип"
  [expr expr-type]
  (= expr-type (get-expr-type expr)))

(defn expr-value
  "получение значения с проверкой типа. [expr] - заданное выражение, [expr-type] - необходимый тип выражения"
  [expr expr-type]
  (if (legal-expr? expr expr-type)
    (second expr)
    (throw (IllegalArgumentException. "Bad type"))))

(defn args
  "получение всех аргументов [expr] узла"
  [expr]
  (rest expr))

; Primitives
(defn nm
  "создания имени для [value]"
  [value]
  (list ::name value))

(defn tag
  "создания тега по [name & values]"
  [name & values]
  (concat (list ::tag name) values))

(defn tag-content
  "получение значения тега [expr]"
  [expr]
  (if (legal-expr? expr ::tag)
    (rest (rest expr))
    (throw (IllegalArgumentException. "Bad type"))))

(defn tag-name
  "получение имени тега [expr]"
  [expr]
  (if (legal-expr? expr ::tag)
    (expr-value (first (filter #(legal-expr? % ::name) expr)) ::name)
    ""))

; Document to string
(defmulti to-str "вывод данных в аккуратном формате в строку" (fn [expr] (get-expr-type expr)))
(defmethod to-str ::value [expr] (str expr))
(defmethod to-str ::name [expr] (str (expr-value expr ::name)))
(defmethod to-str ::tag [expr] (reduce (fn [acc val] (str acc " " (to-str val))) "" (args expr)))

; Path
(defn path
  "Обозначение пути, по именам тегов через пробел"
  [& values]
  values)

(defn node-type [node]
  (if (number? node) :idx node))

(defmulti pred (fn [expr node] (node-type node)))
(defmethod pred :* [expr node] (legal-expr? expr ::tag))
(defmethod pred :default  [expr node] (= node (tag-name expr)))

(defn get-node-content [expr-list node]
    (reduce (fn [acc val] (concat acc (tag-content val)))
            (list)
            (filter (fn [elem] (pred elem node)) expr-list)))

(defmulti apply-node (fn [expr node] (node-type node)))
(defmethod apply-node :* [expr node] (get-node-content expr node))
(defmethod apply-node :idx [expr node] (nth expr node))
(defmethod apply-node :default  [expr node] (get-node-content expr node))

(defn apply-path
  "Функция возвращает содержимое документа, удовлетворяющее пути"
  [expr path]
  (if (legal-expr? expr ::tag)
    (reduce
      (fn [acc node]
        (apply-node acc node))
      (list expr)
      path)
    (throw (IllegalArgumentException. "Bad expression"))))


(defn search-content
  "Функция проверяет совпадение контента по данному пути"
  [expr path content]
  (if (not (legal-expr? expr ::tag))
    (throw (IllegalArgumentException. "Bad expression"))
    (if  (= () (filter (fn [val] ( and (legal-expr? val ::value) (= val content)) ) (apply-path expr path)))
          nil
          expr)))


(defn trim-tag-name [expr] (replace-first (tag-name expr) #":" ""))

(defmulti to-xml "перевод данных в формат xml" (fn [expr] (get-expr-type expr)))
(defmethod to-xml ::value [expr] (str expr))
(defmethod to-xml ::tag [expr] (str "<" (trim-tag-name expr) ">" (apply str (map to-xml (tag-content expr))) "</" (trim-tag-name expr) ">"))

