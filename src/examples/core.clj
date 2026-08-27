(ns core
  (:require [org.httpkit.server :as hk-server]
            [ring.middleware.params :refer [wrap-params]] 
            [compojure.core :refer :all] 
            [starfederation.datastar.clojure.api :as d*]
            [starfederation.datastar.clojure.adapter.http-kit :as hk-gen]
            [hiccup2.core :as h]
            [webstar.js :as js]
            [webstar.clj-generator-js :as cljgen]))

(def con)

(defn sse-handler [request]
  (hk-gen/->sse-response
   request
   {hk-gen/on-open (fn [sse-gen]
                     (def con sse-gen)
                     (d*/patch-elements! sse-gen (str (h/html [:div#aa "connected"]))))}))

(defmethod js/translate 'ds-get [_ arg]
  (str "@get" "(" (js/js* arg) ")"))

(defmacro on-server [form]
  `(js/js* (list ~''ds-get (list ~''encodeURI (list ~''str "/eval?form=" ~(list 'cljgen/gen-clj form))))))

(defn say [msg]
  (d*/patch-elements! con (str (h/html [:div#aa msg]))))

(defn main-html []
  [:html
   [:script {:type :module :src "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.2/bundles/datastar.js"}]
   [:body
    [:div#aa {:data-init "@get('/connect')"} "not-connected"]
    (let [serverdate (str (new java.util.Date))
          hellomsg "Hello You!"]
      (list [:button {:data-on:click (js/js '(alert ~hellomsg))} "hello from server"]
            [:button {:data-on:click (on-server '(core/say "cleared"))} "clear"]
            [:button {:data-on:click (on-server '(core/say ~serverdate))} "server"] 
            [:button {:data-on:click (on-server '(core/say (on-client (new Date))))} "client"] 
            [:button {:data-on:click (on-server '(core/say (on-client (str "client: " (new Date) " / server: " ~serverdate))))} "both"]
            [:button {:data-on:click (on-server '(core/say (on-client (.toUpperCase ~serverdate))))} "mixed"]
            ))]])

(defroutes app
  (GET "/" [] (str (h/html (main-html))))
  (GET "/connect" request (sse-handler request))
  (GET "/eval" [form] (do (eval (read-string form)) "")))

(def my-server (hk-server/run-server (wrap-params app) {:port 8080}))
(my-server)


