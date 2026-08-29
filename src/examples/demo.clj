(ns demo
  (:require [org.httpkit.server :as hk-server]
            [ring.middleware.params :refer [wrap-params]]
            [compojure.core :refer :all] 
            [hiccup2.core :as h] 
            [webstar.clj-generator-js :as cljgen]
            [webstar.core :as w*]
            [webstar.js :as js]))

(def data (atom ["Peter" "Jon" "Julia" "Daniel"]))

(defn name-line [i]
  [:tr {:id i}
   [:td (@data i)]
   [:td [:button {:data-on:click (w*/on-server '(demo/upper (on-client session) ~i))} "to upper case"]]
   [:td [:button {:data-on:click (w*/on-server '(demo/lower (on-client session) ~i))} "to lower case"]]
   [:td [:button {:data-on:click (w*/on-server '(demo/remove (on-client session) ~i))} "x"]]])

(defn people []
  [:table {:id "everybody"} (map name-line (range (count @data)))])

(defn upper [session i]
  (swap! data update i clojure.string/upper-case)
  (w*/patch-for-everybody (name-line i)))

(defn lower [session i]
  (swap! data update i clojure.string/lower-case)
  (w*/patch-for-everybody (name-line i)))

(defn remover [i v]
  (into (subvec v 0 i) (subvec v (inc i) (count v))))

(defn remove [session i]
  (swap! data (partial remover i))
  (w*/patch-for-everybody (people)))

(defn add [session name]
  (swap! data #(conj % name))
  (w*/patch-for-everybody (people)))

(w*/patch-for-everybody (people))

(defn main-html []
  (let [session (str (random-uuid))]
    [:html
     [:head
      [:script (h/raw (js/js '(do (set! session ~session))))]
      [:script {:type :module :src "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.2/bundles/datastar.js"}]] 
     [:body 
      [:div#aa {:data-init (str "@get('/connect?session=' + session)")} "not-connected"]
      (people)
      [:input {:data-bind :name}] 
      [:button {:data-on:click (w*/on-server '(demo/add (on-client session) (on-client $name)))} "add"]]]))

(h/html (main-html))

(defroutes app
  (GET "/" [] (do (println "new session")
                  (str (h/raw "<!DOCTYPE html>") (h/html (main-html)))))
  (GET "/connect" [session :as request] (w*/sse-handler request session [:div#aa (str "connected as session " session)]))
  (GET "/eval" [session form] (do (println session ": " form) (eval (read-string form)) "")))

(comment (def my-server (hk-server/run-server (wrap-params app) {:port 8080}))
         (my-server)
         )


