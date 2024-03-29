# Представление данных в S-выражениях
Требования

1. Определите формат представления данных с древовидной структурой в виде S-выражений. В качестве семантической основы рекомендуется рассмотреть форматы XML и/или JSON. Допускается использование как классических S-выражений, так и расширенных, используемых в языке Clojure.
2. Разработайте язык навигации и простых запросов для заданного формата. Должны поддерживаться: относительный или абсолютный путь к узлу дерева, путь с условиями на свойства промежуточных узлов, путь с переменной вложенностью. В качестве семантической основы рекомендуется рассмотреть язык XPath для XML. Реализуйте функции поиска и модификации заданного формата посредством этого языка.
3. Разработайте представление схемы (по аналогии с XML Schema). Реализуйте функции проверки документа по схеме.

Дополнительные требования 
* Разработайте язык трансформации в HTML, как упрощенный аналог XSLT.

Структура проекта

1. Основной код (папка src)
* base_func.clj - содержит функции, определяющие узлы документа, преобразования документа в строку, поиска узла по заданному пути
* scheme_check.clj - содержит функции для проверки корректности определения тэгов и их последовательности, проверки документа на соответствие схеме

2. Тесты (папка test)
* base_func.clj - тестирует функционал base_func.clj
* scheme_check_test.clj - тестирует функционал scheme_check.clj
* test-data - содержит 4 документа для тестирования
* test-schema - содержит 3 схемы для тестирования

Зоны ответственности
* base_func.clj, base_func_test.clj - Стецкий Илья
* scheme-check.clj, scheme-check-test.clj - Ропперт Екатерина

