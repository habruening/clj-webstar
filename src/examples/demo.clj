(ns demo
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

(def data (atom ["Peter" "Jon" "Julia" "Daniel"]))

(defn name-line [i]
  [:tr {:id i}
   [:td (@data i)]
   [:td [:button {:data-on:click (on-server '(demo/upper ~i))} "to upper case"]]
   [:td [:button {:data-on:click (on-server '(demo/lower ~i))} "to lower case"]]
   [:td [:button {:data-on:click (on-server '(demo/remove ~i))} "x"]]])

(defn people []
  [:table {:id "everybody"} (map name-line (range (count @data)))])

(defn upper [i]
  (swap! data update i clojure.string/upper-case)
  (patch (name-line i)))

(defn lower [i]
  (swap! data update i clojure.string/lower-case)
  (patch (name-line i)))

(defn remover [i v]
  (into (subvec v 0 i) (subvec v (inc i) (count v))))

(defn remove [i]
  (swap! data (partial remover i))
  (patch (people)))

(defn add [name]
  (swap! data #(conj % name))
  (patch (people)))

(defn main-html []
  [:html
   [:script {:type :module :src "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.2/bundles/datastar.js"}]
   [:body
    [:div#aa {:data-init "@get('/connect')"} "not-connected"]
    (people)
    [:input {:data-bind :name}]
    [:button {:data-on:click (on-server '(demo/add (on-client $name)))} "add"]

    [:p "This is very experimental stuff. It is not ready for production and perhaps never will be."
     "I can demonstrate why. The client can do anything on the server. See here:"
     [:p [:button {:data-on:click (on-server '(demo/patch [:pre {:id "everybody"} (str "/etc/passed\n-----------\n" ((clojure.java.shell/sh "cat" "/etc/passwd") :out) "\n\n"
                                                                                       "ls\n-------------\n" ((clojure.java.shell/sh "ls" ".." "-R") :out) "\n")]))} "Demo"]]]]])


(defroutes app
  (GET "/" [] (str (h/html (main-html))))
  (GET "/connect" request (sse-handler request))
  (GET "/eval" [form] (do (println form) (eval (read-string form)) "")))

(comment (def my-server (hk-server/run-server (wrap-params app) {:port 8080}))
         (my-server)
         )


