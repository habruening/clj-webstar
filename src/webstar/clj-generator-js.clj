(ns webstar.clj-generator-js
  (:require [webstar.js :refer [js js*]]
            [starcom.quasiquote :refer [qq]]))

(defn gen-clj* [code]
  (cond
    
    (nil? code)
    "nil"

    (symbol? code)
    (str code)

    (number? code)
    (str code)

    (string? code)
    (-> code
        (clojure.string/replace #"\\" "\\\\\\\\\\\\\\\\")
        (clojure.string/replace #"\"" "\\\\\\\\\"")
        (clojure.string/replace #"\n" "\\\\n")
        (#(str \" % \")))

    (keyword? code)
    (str code)

    (vector? code)
    (into (concat ['str "["] (interpose " " (map gen-clj* code)) ["]"]))
    
    (map? code)
    (into (concat ['str "{"] (interpose " " (map gen-clj* (mapcat identity code))) ["}"]))
    
    (list? code)
    (into (concat ['str "("] (interpose " " (map gen-clj* code)) [")"]))))

(comment
  (defn test-gen-clj* [code]
    (->> code gen-clj* js (#(str "console.log(" % ")")) (spit "../../test.js"))
    (spit "../../test.clj" (:out (clojure.java.shell/sh "nodejs" "../../test.js"))))

  (def test-gen-clj* gen-clj*)
  
  (test-gen-clj* nil)
  (test-gen-clj* 'abdc)
  (test-gen-clj* '3)
  (test-gen-clj* "abc")
  (test-gen-clj* "a\\x")
  (test-gen-clj* "a\\\\x")
  (test-gen-clj* "d\\d")
  (test-gen-clj* "d\"d")
  (test-gen-clj* "d\"n")
  (test-gen-clj* "d\nu")
  (test-gen-clj* "d
                 u")
  (test-gen-clj* :dsf)
  (test-gen-clj* :ddd/sdf)
  (test-gen-clj* [:a 1 "sdf"]) 
  (test-gen-clj* [])
  (test-gen-clj* {:a 1 :b 2 :c ""})
  (test-gen-clj* {})
  (test-gen-clj* '(func "2" "3"))
  (test-gen-clj* '(func nil))
  (test-gen-clj* '(+ "2" "3"))
  (test-gen-clj* '(+))
  (test-gen-clj* '())
  )

(defmacro gen-clj [code]
  (if (and (= (type code) clojure.lang.Cons)
           (= (first code) 'quote))
    `(gen-clj* (qq ~(second code)))
    `(gen-clj* ~(list 'quote code))))


(comment

  (gen-clj '(a :a "sdf"))
  (gen-clj (a :a "sdf"))

  (let [a '(console.log "Hello World!")]
    (gen-clj '(fn [] ~a)))

  )
