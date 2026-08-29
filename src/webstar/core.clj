(ns webstar.core
  (:require [starfederation.datastar.clojure.adapter.http-kit :as hk-gen]
            [starfederation.datastar.clojure.api :as d*]
            [hiccup2.core :as h]
            [webstar.js :as js]))

(def con)

(defn sse-handler [request updated-element]
  (hk-gen/->sse-response
   request
   {hk-gen/on-open (fn [sse-gen]
                     (def con sse-gen)
                     (d*/patch-elements! sse-gen (str (h/html updated-element))))}))

(defmethod js/translate 'ds-get [_ arg]
  (str "@get" "(" (js/js* arg) ")"))

(defn patch [updated-element]
  (d*/patch-elements! con (str (h/html updated-element)))
  )

(defmacro on-server [form]
  `(js/js* (list ~''ds-get (list ~''encodeURI (list ~''str "/eval?form=" ~(list 'cljgen/gen-clj form))))))