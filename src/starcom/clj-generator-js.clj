(ns starcom.clj-generator-js
  (:require [starcom.js :refer [js]]))

(defn gen-clj [code]
  (cond

    (symbol? code)
    (str code)
    
    ))

(js '(str "a" "b"))

(defn in-double-quotes [& s]
  (str "\"" (apply str s) "\""))

(in-double-quotes "sdf")

(defn spaced [s]
  (clojure.string/join " + \" \" + " (map gen-clj s)))

(defn gen-clj
  [code]
  (let [substrings (cond (symbol? code)                            (in-double-quotes code)
                         (number? code)                            (in-double-quotes code)
                         (string? code)                            (->> (in-double-quotes "\\\"" code "\\\"")
                                                                        (clojure.string/replace #"\\" "\\\\")
                                                                        (clojure.string/replace #"\"" "\\\""))
                         (keyword? code)                           (in-double-quotes code)
                         (vector? code)                            (str (in-double-quotes "[")
                                                                        " + " (spaced code) (if (empty? code) "" " + ")
                                                                        (in-double-quotes "]"))
                         (map? code)                               (str (in-double-quotes "{")
                                                                        " + " (spaced (apply concat (into [] code))) (if (empty? code) "" " + ")
                                                                        (in-double-quotes "}"))
                         (and (list? code) (= (first code) '$))    (second code)
                         (list? code)                              (str (in-double-quotes "(")
                                                                        " + " (spaced code) (if (empty? code) "" " + ")
                                                                        (in-double-quotes ")")))]
    substrings))

(comment)

(defn connect-substrings [[last-of-result & _ :as reverse-result] substrings]
  (if (and (= (last-of-result)))))

(println (gen-clj  "sfd" ))

(defmacro xxxx [x]
  (println x))

(xxxx '(x ~d))

(defn gen-clj
  [code]
  (let [substrings (cond (symbol? code)                            [(in-double-quotes code)]
                         (number? code)                            [(in-double-quotes code)]
                         (string? code)                            [(->> (in-double-quotes "\\\"" code "\\\"")
                                                                         (clojure.string/replace #"\\" "\\\\")
                                                                         (clojure.string/replace #"\"" "\\\""))]
                         (keyword? code)                           [(in-double-quotes code)]
                         (vector? code)                            (concat [(in-double-quotes "[")]
                                                                           (interpose (in-double-quotes " ") (map gen-clj code))
                                                                           [(in-double-quotes "]")])
                         (map? code)                               (concat [(in-double-quotes "{")]
                                                                           (interpose " " (apply concat (into [] (map gen-clj code))))
                                                                           (in-double-quotes "}"))
                         (and (list? code) (= (first code) '$))    [(second code)]
                         (list? code)                              (concat [(in-double-quotes "(")]
                                                                           (interpose " " (map gen-clj code))
                                                                           [(in-double-quotes ")")]))]
    (reduce (fn [result sub-string]))
    (clojure.string/join " + " substrings)))

(butlast (butlast [1 2 3]))

(spit "output.txt" (gen-clj [:a [:sadf]]))
(gen-clj 3)
((gen-clj "asdf"))

(concat [1] [2] [3])

(println '("\\\"" code "\\\""))
(clojure.string/join (gen-clj '(println "Hello")))
(println "\\\"")
(count "\\\"")

(println (gen-clj "\\\""))

(println "\\\"")

(println "\\\"")

(println (gen-clj '(defn gen-clj
                     [code]
                     (cond (symbol? code)                            (in-double-quotes code)
                           (number? code)                            (in-double-quotes code)
                           (string? code)                            (in-double-quotes "\\\"" code "\\\"")
                           (keyword? code)                           (in-double-quotes code)
                           (vector? code)                            (str (in-double-quotes "[")
                                                                          " + " (spaced code) (if (empty? code) "" " + ")
                                                                          (in-double-quotes "]"))
                           (map? code)                               (str (in-double-quotes "{")
                                                                          " + " (spaced (apply concat (into [] code))) (if (empty? code) "" " + ")
                                                                          (in-double-quotes "}"))
                           (and (list? code) (= (first code) '$))    (second code)
                           (list? code)                              (str (in-double-quotes "(")
                                                                          " + " (spaced code) (if (empty? code) "" " + ")
                                                                          (in-double-quotes ")"))))))