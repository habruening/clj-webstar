(ns webstar.core
  (:require [starfederation.datastar.clojure.adapter.http-kit :as hk-gen]
            [starfederation.datastar.clojure.api :as d*]
            [hiccup2.core :as h]
            [webstar.js :as js]))

(def sessions (atom {}))

(defn session-data [session]
  ((@sessions session) :session-data))

(defn switch!-session-data [session new-session-data]
  (swap! sessions assoc-in [session :session-data] new-session-data))

(defn swap!-session-data [session f]
  (swap! sessions update-in [session :session-data] f))

; Todo: Sessions must be deleted after a timeout.

(defn load [session]
  (list [:script (h/raw (js/js '(do (set! session ~session))))]
        [:script {:type :module :src "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.2/bundles/datastar.js"}]))

(defn sse-handler [request session updated-element] 
  (hk-gen/->sse-response
   request
   {hk-gen/on-open (fn [sse-gen]
                     (swap! sessions assoc-in [session :connection] sse-gen)
                     (d*/patch-elements! sse-gen (str (h/html updated-element))))
    hk-gen/on-close (fn [sse-gen status]
                      (swap! sessions update-in [session] dissoc :connection))}))

(defmethod js/translate 'ds-get [_ arg]
  (str "@get" "(" (js/js* arg) ")"))

(defn patch [session updated-element]
  (if-let [connection ((@sessions session) :connection)]
    (d*/patch-elements! connection (str (h/html updated-element)))))

(defn patch-for-everybody [updated-element] 
  (doseq [session (keys @sessions)] 
    (patch session updated-element)))

(defmacro on-server [form]
  `(js/js* (list ~''ds-get (list ~''encodeURI (list ~''str "/eval?session=" ('<- "session") "&form=" ~(list 'cljgen/gen-clj form))))))
