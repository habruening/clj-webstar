(ns demo
  (:require [org.httpkit.server :as hk-server]
            [ring.middleware.params :refer [wrap-params]]
            [compojure.core :refer :all] 
            [hiccup2.core :as h] 
            [webstar.core :as w*]))

(def data (atom ["Peter" "Jon" "Julia" "Daniel"]))

(defn name-line [session i]
  [:tr {:id i}
   [:td (@data i)]
   [:td [:button {:data-on:click (w*/on-server session '(demo/upper ~session ~i))} "to upper case"]]
   [:td [:button {:data-on:click (w*/on-server session '(demo/lower ~session ~i))} "to lower case"]]
   [:td [:button {:data-on:click (w*/on-server session '(demo/remove ~session ~i))} "x"]]])

(defn people [session]
  [:table {:id "everybody"} (map (partial name-line session) (range (count @data)))])

(defn upper [session i]
  (swap! data update i clojure.string/upper-case)
  (w*/patch session (name-line session i)))

(defn lower [session i]
  (swap! data update i clojure.string/lower-case)
  (w*/patch session (name-line session i)))

(defn remover [i v]
  (into (subvec v 0 i) (subvec v (inc i) (count v))))

(defn remove [session i]
  (swap! data (partial remover i))
  (w*/patch session (people session)))

(defn add [session name]
  (swap! data #(conj % name))
  (w*/patch session (people session)))

(defn main-html []
  (let [session (str (random-uuid))]
    [:html
     [:script {:type :module :src "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.2/bundles/datastar.js"}]
     [:body
      [:div#aa {:data-init (str "@get('/connect?session=" session "')")} "not-connected"]
      (people session)
      [:input {:data-bind :name}]
      [:button {:data-on:click (w*/on-server session '(demo/add ~session (on-client $name)))} "add"]]]))

(main-html)

(defroutes app
  (GET "/" [] (str (h/html (main-html))))
  (GET "/connect" [session :as request] (w*/sse-handler request session [:div#aa (str "connected as session " session)]))
  (GET "/eval" [session form] (do (println session ": " form) (eval (read-string form)) "")))

(comment (def my-server (hk-server/run-server (wrap-params app) {:port 8080}))
         (my-server)
         )


