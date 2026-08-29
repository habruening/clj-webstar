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
   [:td [:button {:data-on:click (w*/on-server '(demo/upper ~i))} "to upper case"]]
   [:td [:button {:data-on:click (w*/on-server '(demo/lower ~i))} "to lower case"]]
   [:td [:button {:data-on:click (w*/on-server '(demo/remove ~i))} "x"]]
   [:td [:button {:data-on:click (w*/on-server '(demo/invite (on-client session) ~(@data i)))} "invite"]]])

(defn people []
  [:table#contents (map name-line (range (count @data)))])

(defn upper [i]
  (swap! data update i clojure.string/upper-case)
  (w*/patch-for-everybody (name-line i)))

(defn lower [i]
  (swap! data update i clojure.string/lower-case)
  (w*/patch-for-everybody (name-line i)))

(defn remover [i v]
  (into (subvec v 0 i) (subvec v (inc i) (count v))))

(defn remove [i]
  (swap! data (partial remover i))1
  (w*/patch-for-everybody (people)))

(defn add [name]
  (swap! data #(conj % name))
  (w*/patch-for-everybody (people)))

(defn invite [session name]
  (w*/swap!-session-data session #(conj % name))
  (w*/patch session [:span#audience (clojure.string/join ", " (w*/session-data session))]))

(defn main-html []
  (let [session (str (random-uuid))]
    [:html
     [:head
      (w*/load session)] 
     [:body
      [:div {:data-init (str "@get('/connect?session=' + session)")}]
      (people)
      [:input {:data-bind :name}]
      [:button {:data-on:click (w*/on-server '(demo/add (on-client $name)))} "add"]
      [:div "Audience: " [:span#audience ""]]]]))

(defroutes app
  (GET "/" [] (do (println "new session")
                  (str (h/raw "<!DOCTYPE html>") (h/html (main-html)))))
  (GET "/connect" [session :as request] (w*/sse-handler request session (people)))
  (GET "/eval" [session form] (do (println session ": " form) (eval (read-string form)) "")))

(comment (def my-server (hk-server/run-server (wrap-params app) {:port 8080}))
         (my-server)
         )


