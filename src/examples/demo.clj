(ns demo
  (:require [org.httpkit.server :as hk-server]
            [ring.middleware.params :refer [wrap-params]]
            [compojure.core :refer :all]
            [starfederation.datastar.clojure.api :as d*]
            [starfederation.datastar.clojure.adapter.http-kit :as hk-gen]
            [hiccup2.core :as h]
            [webstar.js :as js]
            [webstar.clj-generator-js :as cljgen]
            [webstar.core :as w*]))

(def data (atom ["Peter" "Jon" "Julia" "Daniel"]))

(defn name-line [i]
  [:tr {:id i}
   [:td (@data i)]
   [:td [:button {:data-on:click (w*/on-server '(demo/upper ~i))} "to upper case"]]
   [:td [:button {:data-on:click (w*/on-server '(demo/lower ~i))} "to lower case"]]
   [:td [:button {:data-on:click (w*/on-server '(demo/remove ~i))} "x"]]])

(defn people []
  [:table {:id "everybody"} (map name-line (range (count @data)))])

(defn upper [i]
  (swap! data update i clojure.string/upper-case)
  (w*/patch (name-line i)))

(defn lower [i]
  (swap! data update i clojure.string/lower-case)
  (w*/patch (name-line i)))

(defn remover [i v]
  (into (subvec v 0 i) (subvec v (inc i) (count v))))

(defn remove [i]
  (swap! data (partial remover i))
  (w*/patch (people)))

(defn add [name]
  (swap! data #(conj % name))
  (w*/patch (people)))

(defn main-html []
  [:html 
   [:script {:type :module :src "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.2/bundles/datastar.js"}]
   [:body
    [:div#aa {:data-init "@get('/connect')"} "not-connected"]
    (people)
    [:input {:data-bind :name}]
    [:button {:data-on:click (w*/on-server '(demo/add (on-client $name)))} "add"]]])

(defroutes app
  (GET "/" [] (str (h/html (main-html))))
  (GET "/connect" request (w*/sse-handler request [:div#aa "connected"]))
  (GET "/eval" [form] (do (println form) (eval (read-string form)) "")))

(comment (def my-server (hk-server/run-server (wrap-params app) {:port 8080}))
         (my-server)
         )


