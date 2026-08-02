(ns starcom.quasiquote)

(defmacro qq
  [code]
  #_(println "value: " code)
  #_(println "type: " (type code))
  (cond

    (symbol? code)
    (list 'quote code)

    (number? code)
    code

    (string? code)
    code

    (keyword? code)
    code

    (vector? code)
    (apply vector (map #(list `qq %) code))

    (map? code)
    (update-keys (update-vals code #(list `qq %)) #(list `qq %))

    (and (= (type code) clojure.lang.Cons)
         (= (first code) 'clojure.core/unquote))
    (second code)

    (and (= (type code) clojure.lang.Cons)
         (= (first code) 'quote)) 
    (list 'quote  code)
    
    (list? code)
    (apply list 'list (map #(list `qq %) code))))

(comment
  (qq 4)
  (qq b)
  (qq "abc")
  (qq ())
  (qq [])
  (qq (a))
  (qq (a b))
  (qq ({a b}))
  (qq {a b})
  (qq [a b c])
  (qq (a [b c d] e))
  (qq (a (b c d) e))
  (qq (a ($ (+ 1 1)) b))
  (qq (dd `abc))
  (let [x 'b] (qq 4))
  (let [x 'b] (qq (~x)))
  (let [x 'b] (qq (a (~x))))
  (let [x 'b] (qq ({a (~x)})))
  (let [x 'b] (qq {a (~x)}))
  (let [x 'b] (qq [a (~x) c]))
  (let [x 'b] (qq (a [(~x) c d] e)))
  (let [x 'b] (qq (a ((~x) c d) e)))
  (let [x 'b] (qq ({a (~x) c d} e)))
  (let [x 'b] (qq (~(map identity ['a (qq ((~x) c))]))))
  (let [b 3]
    (qq ~b)) 
  (let [b 3]
    (qq (a b ~b c ~(+ 1 b))))
  (let [b '(x y)]
    (qq (a b ~b c)))
  (qq 'a)
  
  ; As we can see, qq accepts unquote. But as this is not obvious, we should use a wrapper that
  ; requires from the user the quote symbol.

  (defmacro does-not-work [code]
    (qq ~code))
  (let [x 3]
    ; This does not work, because unquote would not be recognised. The macro qq would resolve ~code to
    ; the value of code, which is '(adsf ~x). That is all. 
    (does-not-work '(adsf ~x)))
  (macroexpand-1 '(does-not-work '(adsf ~x)))


  (defmacro this-works [code]
    `(qq ~(second code)))
  (this-works '(+ a 2))
  (let [x 3]
    (this-works '(adsf ~x)))
  (macroexpand-1 '(this-works '(adsf ~x)))

  ; Nested runtime expressions only works with explicit calls to qq. In practice we would
  ; rename qq in our namespace. For example to something like query or js. 
  (let [a 3
        x (qq (u v ~a))]
    (this-works '(adsf ~x)))

  ; Nested compile time expressions do not work, but perhaps would be possible. We would have
  ; to walk through the expression first and replace qhotes with qq. With runtime expressions
  ; this would not be possible, because qq is a macro.
  (let [x 3]
    (this-works '(a ~(list :a '(:a ~x :b)))))

  ; Often the qq call can be implemented and expressions can be composed at runtime in an
  ; intuitive way. We can use the same name.
  (defmacro python-code [code]
    `(qq ~(second code)))
  (let [x-initial-value 16
        a-initial 'rect.size]
    (python-code '((x = ~x-initial-value)
                   (y = ~{"a" (python-code '(inc ~a-initial)) "x" x-initial-value}))))
  )