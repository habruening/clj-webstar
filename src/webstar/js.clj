(ns starcom.js
  (:require [webstar.quasiquote :refer [qq]]))


(defmulti translate (fn [js-operation & args]
                      (cond (clojure.string/starts-with? js-operation ".-") :memberaccess
                            (clojure.string/starts-with? js-operation ".") :methodcall
                            :else js-operation)))

(defn js* [code]
  (cond
    
    (number? code)
    (str code)

    (symbol? code)
    (str code)

    (string? code)
    (str "'" code "'")

    (keyword? code)
    (str "'" (name code) "'")

    (vector? code)
    (str "[" (clojure.string/join "," (map js* code)) "]")

    (map? code)
    (str "{" (clojure.string/join "," (map #(str (js* %1) ":" (js* %2)) (keys code) (vals code))) "}") 

    (seq? code)
    (apply translate code)))

(comment (js* 3)
         (js* 3.2)
         (js* -3.2)
         (js* 'lkj)
         (js* "abc")
         (js* "")
         (js* :a)
         (js* [:a 1 [] 'b])
         (js* {:a 3 "u" :v})
         (js* {}))

(defmethod translate :default [function & arguments]
  (str (js* function) "(" (clojure.string/join "," (map js* arguments)) ")"))

(comment (js* '(lkj))
         (js* '(lkj a b))
         )

(defmethod translate :methodcall [method & arguments]
  (str (js* (first arguments)) method "(" (clojure.string/join "," (map js* (rest arguments))) ")"))

(comment (js* '(.lkj a))
         (js* '(.lkj a b))
         )

(defmethod translate :memberaccess [method & arguments]
  (str (js* (first arguments)) "." (subs (str method) 2)))

(comment (js* '(.-lkj a b))
         (js* '(func a (.method b arg1 (.-member c))))
         )

(defmethod translate '<- [_ code]
  code)

(comment (js* '(func (<- "console.log(\"Hello World!\"")))
         (js* '(console.log (<- "inc(a)"))))

(comment         
                 (js* '(do (inc a) (<- "inc(a)") ~"inc(a)"))
                 (js* '[])
                 (js* '[a])
                 (js* '[a b])
                 (js* '[a "b" :c])
                 (js* '[a (+ 1 2) c])
                 )

(defn- js-block [statements]
  (clojure.string/join ";" (map js* statements)))

(defmethod translate 'do [_ & code]
  (js-block code))

(defmethod translate 'do* [_ code]
  (js-block code))

(comment (js* '(do (inc a)
                   (inc b)))
         (js* '(do (inc a)))
         (js* '(do))
         (js* '(do* ((inc a))))
         (js* '(do* ((inc a)
                     (inc b))))
         (js* '(do* ((inc a))))
         (js* '(do* ()))
         (js* (list 'do* (map #(list 'inc %) ['a 'b 'c 'd])))
         ; We will se later how the js macro does this better with unquoting.
         ; But as a function there is no good solution.
         )

(defn- infix [operator args]
  (str "(" (->> args (map js*) (interpose operator) (clojure.string/join)) ")"))

(defmethod translate '+ [_ & args] 
  (infix "+" args))

(defmethod translate '- [_ & args]
  (infix "-" args))

(defmethod translate '* [_ & args]
  (infix "/" args))

(defmethod translate '/ [_ a b]
  (str "(" (js* a) "/" (js* b) ")"))

(comment
  (js* '(+ a))
  (js* '(+ a b))
  (js* '(+ a b c d ew))
  (js* '(- a b))
  (js* '(* a b))
  (js* '(/ a b))
  (js* '((* (+ a b) (- (/ a 2) b))))
  )

(defn- connect-adjacent-strings [args]
  (let [reduction (fn [[last & before :as result] s]
                    (if (and (string? last)
                             (string? s))
                      (conj before (str last s))
                      (conj result s)))
        reverted-result (reduce reduction '() args)]
    (reverse reverted-result)))

(defn- ->string [s]
  (js* (list '<- (if (string? s) s (list 'String s)))))

(defmethod translate 'str [_ & args]
  (->> args
       connect-adjacent-strings
       (map ->string)
       (infix "+")))

(comment
  (js* '(str 3))
  (js* '(str a b c))
  (js* '(str "a" "b"))
  (js* '(str "a" b "c" "d" 3 "e"))
  )

(defmethod translate '= [_ left right]
  (str "(" (js* left) "==" (js* right) ")"))

(defmethod translate '!= [_ left right]
  (str "(" (js* left) "!=" (js* right) ")"))

(comment (js* '(= a b))
         (js* '(!= a b))
         (js* '(!= c (+ a b))))

(defmethod translate 'set! [_ from to]
  (str (js* from) "=" (js* to)))

(defmethod translate 'defn [_ name arguments & body]
  (str "function " name "(" (clojure.string/join "," (map str arguments)) "){" (js-block body) "}"))

(defmethod translate 'fn [_ arguments & body]
  (str "function" "(" (clojure.string/join "," (map str arguments)) "){" (js-block body) "}"))

(defmethod translate 'return [_ func]
  (str "return " (js* func)))

(comment (js* '(set! a 3))
         (js* '(set! a (+ b c)))
         (js* '(set! a b))
         (js* '(set! (.-a m) b))
         (js* '(defn func_1 [] (set! a 4)))
         (js* '(defn func_1 [b] (set! a b)))
         (js* '(defn func_1 [b c] (set! a (+ b c))))
         (js* '(defn func_1 [] (set! a 16) (set! b 12)))
         (js* '(set! func_1 (fn [] (set! a 4))))
         (js* '(set! func_1 (fn [b] (set! a b))))
         (js* '(set! func_1 (fn [b c] (set! a (+ b c)))))
         (js* '(set! func_1 (fn [] (set! a 16) (set! b 12))))
         (js* '(fn [] (set! a 4)))
         (js* '(fn [b] (set! a b)))
         (js* '(fn [b c] (set! a (+ b c))))
         (js* '(fn [] (set! a 16) (set! b 12)))
         )

(defmethod translate 'aget [_ array idx]
  (str (js* array) "[" (js* idx) "]"))

(comment
  (js* '(aget a 3))
  (js* '(set! (aget a 3) (+ b c))))

(defmethod translate 'new [_ class & arguments]
  (str "new " (str class) "(" (clojure.string/join "," (map js* arguments)) ")"))

(comment
  (js* '(new a 3))
  (js* '(set! (aget l 4) (new A "ixi"))))

(defmethod translate 'doseq [_ seq-exprs body]
  (str (js* seq-exprs) ".forEach(" (js* body) ")"))

(defmethod translate 'async [_ func]
  (str "async " (js* func)))

(defmethod translate 'await [_ func]
  (str "await " (js* func)))

(defmethod translate 'if
  ([_ condition then]
   (str "if" (js* condition) "{" (js* then) "}"))
  ([if condition then else]
   (str (translate if condition then) "else{" (js* else) "}"))) ; don't know if parenthesis required.

(comment
  (js* '(doseq [1 2 3] (fn [num] (console.log num)))) 
  (js* '(async (xy)))
  (js* '(await (xy)))
  (js* '(if (= a 2) b))
  (js* '(if (= a 2) b c))
  )

(defmacro js [code]
  (if (and (= (type code) clojure.lang.Cons)
           (= (first code) 'quote))
    `(js* (qq ~(second code)))
    `(js* ~(list 'quote code))))

(comment

  ; Normally we quote the code.
  (js '(console.log "Hello World!"))
  (js (console.log "Hello World!"))

  ; We can use unquote
  (js '(console.log ~(clojure.string/upper-case "Hello World!")))

  ; We can even requote 
  (defn log-user-id [upper]
    (let [formatting (fn [s]
                       (if upper (js '(.toUpperCase (<- ~s))) (js '(.toLowerCase (<- ~s)))))]
      (js '(console.log (<- ~(formatting (js '(get-login))))))))

  (js '(do (<- ~(log-user-id true))
           (<- ~(log-user-id false))))

  ; These are the options for composition; 
  (js '(console.log "Hello World!"))
  (let [message "Hello World!"]
    (js '(console.log ~message)))
  (let [message '(get_message)]
    (js '(console.log ~message)))
  (let [message "get_message()"]
    (js '(console.log (<- ~message))))
  )

(comment
  
  (let [lk "License-Key" f "file" rn "RealName" x "get_annotations.xfdf?file=file"]
    (js '(.then (WebViewer {path "WebViewer/lib"
                            licenceKey ~lk
                            initialDoc ~f}
                           (document.getElementById "viewer"))
                (fn [instance]
                  (set! annotation_manager instance.Core.annotationManager)
                  (annotation_manager.setCurrentUser ~rn)
                  (instance.Core.documentViewer.setDocumentXFDFRetriever
                   (async (fn []
                            (set! response (await (fetch "get_annotations.xfdf?file=file")))
                            (set! xfdf (await (response.text)))
                            (console.log xfdf)
                            (return xfdf))))
                  (instance.Core.annotationManager.addEventListener "annotationChanged"
                                                                    (async (fn [e]
                                                                             (await (fetch "annotation_changed.html?file=file"
                                                                                           {method "POST"
                                                                                            headers {"Content-Type" "application/xml"}
                                                                                            body (await (instance.Core.annotationManager.exportAnnotationCommand))}))
                                                                             (console.log (instance.Core.annotationManager.exportAnnotations)))))
                  (instance.UI.disableElements ["toolbarGroup-Shapes"])
                  (instance.UI.setHeaderItems (fn [header]
                                                (set! item (header.getItems))
                                                (set! item (item.slice 2 -1))   ; todo: Better not use the indices here
                                                (header.update item)))))))
; => "WebViewer({path:\"WebViewer/lib\",licenseKey:\"license-key\",initialDoc:\"file\"},document.getElementById(\"viewer\")).then(function(instance){annotation_manager=instance.Core.annotationManager;annotation_manager.setCurrentUser(\"realname\");instance.Core.documentViewer.setDocumentXFDFRetriever(async function(){response=await fetch(\"get_annotations.xfdf?file=file\");xfdf=await response.text();console.log(xfdf);return xfdf});instance.Core.annotationManager.addEventListener(\"annotationChanged\",async function(e){await fetch(\"annotation_changed.html?file=file\",{method:\"POST\",headers:{\"Content-Type\":\"application/xml\"},body:await instance.Core.annotationManager.exportAnnotationCommand()});console.log(instance.Core.annotationManager.exportAnnotations())});instance.UI.disableElements([\"toolbarGroup-Shapes\"]);instance.UI.setHeaderItems(function(header){item=header.getItems();item=item.slice(2,-1);header.update(item)})})"
  )
