(module main
  (:import
    router
    [str :from piglet:string]
    [css :from piglet:css]
    [http-server :from piglet:node/http-server]))

(defn split-tag [tag]
  (let [tag (name tag)
        parts (str:split #"(?=[\.#])" tag)]
    [(first parts)
     (some #(when (str:starts-with? % "#")
              (str:subs % 1))
       parts)
     (keep #(when (str:starts-with? % ".")
              (str:subs % 1))
       parts)]))

(defn render-opts [opts]
  (str:join
    (for [[k v] opts]
      (str " " (name k) "='" (str:replace (str v) #"'" "&apos;") "'"))))

(defn escape-string [s]
  (-> s
    (str:replace #"&" "&amp;")
    (str:replace #"<" "&lt;")))

(defn render-html [h]
  (cond
    (string? h)
    (escape-string h)

    (vector? h)
    (cond (fn? (first h))
      (render-html
        (apply (first h) (rest h)))

      (= :<> (first h))
      (apply str (map render-html (rest h)))

      :else
      (let [[tag ?opts & children] h
            [opts children] (if (dict? ?opts)
                              [?opts children]
                              [nil (cons ?opts children)])
            [tag id klz] (split-tag tag)
            opts (cond-> opts
                   id (assoc :id id)
                   (seq klz) (update :class #(str:join " "
                                               (if (string? %)
                                                 (conj klz %)
                                                 (concat klz %)))))]

        (str "<" tag (render-opts opts)
          (if (seq children)
            (str ">"
              (apply str (map render-html children))
              "</" tag ">")
            (str "/>")))))

    :else
    (str h)))

(defn page-layout [head body]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:content "width=device-width, initial-scale=1" :name "viewport"}]
    head]
   [:body
    body]])

(defn wrap-render [handler]
  (fn [req]
    (let [res (handler req)]
      (if (not (vector? (:html res)))
        res
        (let [route-data (:route-data req)
              page-layout (or (:page-layout res) (:page-layout route-data) page-layout)
              layouts (keep :layout [res route-data])
              head (cond-> [:<>]
                     (:head route-data)
                     (conj (:head route-data))
                     (:head res)
                     (conj (:head res)))
              body (:html res)]
          (-> res
            (assoc :body (render-html (page-layout head (reduce (fn [acc f] (f acc)) body layouts))))
            (assoc-in [:headers "Content-Type"] "text/html")))))))

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
                                                       :middleware [wrap-render]}) req))
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
