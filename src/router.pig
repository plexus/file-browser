(module router
  "Bare bones routing functionality"
  (:import
    [str :from piglet:string]))

(def no-inherit #{:name :get :post :put :delete})

(defn flatten-routes [merge-fn prefix data routes]
  (cond
    (and
      (vector? routes)
      (string? (first routes)))
    (let [[path ?opts] routes
          path (str prefix path)
          data (if (dict? ?opts) (merge-with-kv merge-fn (apply dissoc data no-inherit) ?opts) data)]
      (into [[path data]]
        (mapcat (partial flatten-routes merge-fn path data))
        (if (dict? ?opts)
          (rest (rest routes))
          (rest routes))))

    (vector? routes)
    (mapcat (partial flatten-routes merge-fn prefix data) routes)))

(defn compile-route [tree [path data]]
  (let [segments (str:split #"/" (str:subs path 1))]
    (loop [[seg & segs] segments
           path []
           tree tree]
      (if (nil? seg)
        tree
        (if (str:starts-with? seg ":")
          (recur
            segs
            (conj path :param)
            (-> tree
              (assoc-in (conj path :param :param-key) (keyword (str:subs seg 1)))
              (assoc-in (conj path :param :data) data)))
          (recur
            segs
            (conj path seg)
            (assoc-in tree (conj path seg :data) data)))))))

(defn match-path [tree path]
  (let [[{:keys [data]} params]
        (reduce
          (fn [[tree params] segment]
            (if-let [o (get tree segment)]
              [o params]
              (if-let [p (:param tree)]
                [p (assoc params (:param-key p) (js:decodeURIComponent segment))]
                (reduced nil))))
          [tree {}]
          (str:split #"/" (str:subs path 1)))]
    {:data data
     :params params}))

(defn compile-routes
  ([routes]
    (compile-routes routes nil))
  ([routes merge-fn]
    (reduce
      compile-route
      {}
      (flatten-routes (or merge-fn (fn [k o n] n)) "" {} routes))))

(def four-oh-four
  {:status 404
   :body "not found"
   :headers {"Content-Type" "text/plain"}})

(defn router
  ([routes]
    (router routes nil))
  ([routes {:keys [merge-fn middleware]}]
    (let [routes (compile-routes routes merge-fn)]
      (fn [{:keys [path] :as req}]
        (let [path (if (str:ends-with? path "/") (str:subs path 0 -1) path)
              {:keys [data params]} (match-path routes path)]
          (if-let [f (get data (:method req))]
            (let [middleware (concat middleware (:middleware data) (get-in data [(:method req) :middleware]))
                  handler (reduce (fn [handler wrap]
                                    (assert handler)
                                    (if (vector? wrap)
                                      (apply (first wrap) handler (rest wrap))
                                      (wrap handler)))
                            (:handler f f)
                            middleware)]
              (try
                (handler (assoc req :route-data data :path-params params))
                (catch e
                  {:status 500
                   :headers {"Content-Type" "text/plain"}
                   :body (str e "\n\n"
                           (.-stack e))})))
            four-oh-four))))))
