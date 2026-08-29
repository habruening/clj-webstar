(ns webstar.core
  (:require [starfederation.datastar.clojure.adapter.http-kit :as hk-gen]
            [starfederation.datastar.clojure.api :as d*]
            [hiccup2.core :as h]
            [webstar.js :as js]))

(def sessions (atom {}))

(defn sse-handler [request session updated-element]
  (println "new sesssion " session)
  (hk-gen/->sse-response
   request
   {hk-gen/on-open (fn [sse-gen]
                     (swap! sessions assoc session sse-gen)
                     (d*/patch-elements! sse-gen (str (h/html updated-element))))}))

(defmethod js/translate 'ds-get [_ arg]
  (str "@get" "(" (js/js* arg) ")"))

(defn patch [session updated-element]
  (d*/patch-elements! (@sessions session) (str (h/html updated-element))))

(defn patch-for-everybody [updated-element] 
  (doseq [session (keys @sessions)]
    (println session)
    (patch session updated-element)))

(defmacro on-server [form]
  `(js/js* (list ~''ds-get (list ~''encodeURI (list ~''str "/eval?session=" ('<- "session") "&form=" ~(list 'cljgen/gen-clj form))))))

(require '[webstar.clj-generator-js :as cljgen])

(on-server '(demo/add (on-client session) (on-client $name)))