(module main
  (:import
    router
    html
    [str :from piglet:string]
    [css :from piglet:css]
    [http-server :from piglet:node/http-server]))

(defn GET-index [req]
  {:status 200
   :head [:title "-INDEX-"]
   :html [:h1 "INDEX"]
   })

(def styles
  [:main {:max-width "80rem"
          :margin "0 auto"
          :background-color "#eff7ef"}])

(defn GET-styles [req]
  {:status 200
   :headers {"Content-Type" "text/css"}
   :body (css:css styles)})

(defn base-layout [h]
  [:main h])

(def routes
  [["" {:head [:<>
               [:link {:rel "stylesheet" :href "/styles.css"}]]
        :layout base-layout}
    ["/" {:get #'GET-index}]
    ["/styles.css" {:get #'GET-styles}]]
])

(defn merge-data-fn [k o n]
  (cond
    (= :layout k)
    (comp o n)
    (= :middleware k)
    (into o n)
    :else
    n))

(def server
  (http-server:create-server (fn [req]
                               ((router:router routes {:merge-fn merge-data-fn
                                                       :middleware [html:wrap-render]}) req))
    {:port 9876}))

(comment
  ((router:router routes {:merge-fn merge-data-fn
                          :middleware [wrap-render]})
    {:method :get
     :path "/"})

  (router:match-path
    (router:compile-routes routes merge-data-fn)
    "/foo/123")

  (http-server:stop! server)
  (http-server:start! server))
