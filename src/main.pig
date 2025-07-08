(module main
  (:import
    router html relative-time
    [assets :from node/http/assets]
    [fs :from "node:fs"]
    [path :from "node:path"]
    [str :from piglet:string]
    [css :from piglet:css]
    [http-server :from piglet:node/http-server]))

(def styles
  [[":where(*)"
    {:box-sizing "border-box"
     :--theme-page-bg "rgb(248,248,248)"
     :--theme-button-bg "rgb(243,243,243)"
     :--theme-bg "#fff"
     :--theme-color-separator "hsl(0, 0%, 90.9%)"
     :--theme-color-text "hsl(0, 0%, 9%)"
     :--theme-color-text-secondary "hsl(0, 0%, 43.5%)"
     :--theme-color-accent "rgb(100, 253, 69)"}]
   [:html
    {:font-family "'Libre Franklin', sans-serif"
     :background-color "var(--theme-page-bg)"
     :color "var(--theme-color-text)"}]
   [#{:html :body} {:margin 0 :padding 0}]
   [:main {:max-width "80rem"
           :margin "0 auto"
           :background-color "var(--theme-bg)"}]])

(def root-dir "/home/arne/Downloads")

(defn format-byte-size [num]
  (let [prefixes ["" "k" "M" "G" "T" "P" "E" "Z" "B"]
        magnitude (js:Math.floor (/ (js:Math.log10 num) 3))
        scaled-num (/ num (js:Math.pow 1000 magnitude))]
    (str
      (.toLocaleString scaled-num undefined #js {:maximumFractionDigits 1})
      " "
      (get prefixes magnitude)
      "B")))

(defn dir->data [dir]
  (for [file (fs:readdirSync dir)]
    (assoc (fs:statSync (path:join dir file))
      :dir dir
      :name file)))

(def component-styles (box {}))

(defmacro defc
  "Poor man's styled component macro

  - classnames aren't namespaced"
  [name & forms]
  (let [[doc & forms] (if (string? first forms) forms (cons nil forms))
        styles (butlast forms)
        fntail (last forms)]
    (swap! component-styles assoc name (into [(str "." name)] styles))
    `(defn ~name ~@(when doc [doc]) ~(first fntail)
       (loop [[tag# & ch#] (do ~@(rest fntail))]
         (if (fn? tag#)
           ;; FIXME: seems piglet gets confused about recur in macros
           (~'recur (apply tag# ch#))
           (into [(keyword (str (name tag#) "." '~name))] ch#))))))

(defc file-table-entry
  {:padding "1rem 0"
   :border-bottom "1px solid var(--theme-color-separator)"}
  [:.filename
   {:margin-bottom "0.3rem"
    :font-weight "450"}]
  [:.fileinfo
   {:color "var(--theme-color-text-secondary)"}]
  ([{:keys [dir name mtimeMs size]}]
    (let [now (js:Date.now)]
      [foo
       [:div.filename {:title (str dir "/" name)} name]
       [:div.fileinfo
        (format-byte-size size)
        " – "
        (relative-time:distance-in-words now mtimeMs) " ago"]])))

(defn file-table [files]
  (for [f files]
    [file-table-entry f]))

(defc dir-overview-page
  {:padding "0 1rem"}
  [:h1 {:margin-top "0"
        :padding "1rem 0"
        :border-bottom "3px solid var(--theme-color-accent)"}]
  ([files-info]
    [:div
     [:h1 "Your Files"]

     [file-table files-info]]))

(defn GET-index [req]
  (let [files (fs:readdirSync root-dir)]
    {:status 200
     :head [:title "File Browser"]
     :html [dir-overview-page (sort-by (comp - :mtimeMs) (dir->data root-dir))]}))

(defn GET-styles [req]
  {:status 200
   :headers {"Content-Type" "text/css"}
   :body (css:css (apply conj styles (vals @component-styles)))})

(defn base-layout [h]
  [:main h])

(def routes
  [["" {:head [:<>
               [:link {:rel "stylesheet" :href "/fonts/libre_franklin.css"}]
               [:link {:rel "stylesheet" :href "/styles.css"}]]
        :layout base-layout}
    ["/" {:get #'GET-index}]
    ["/styles.css" {:get #'GET-styles}]]])

(defn merge-data-fn [k o n]
  (cond
    (= :layout k)
    (comp o n)
    (= :middleware k)
    (into o n)
    :else
    n))

(defonce server (box nil))

(defn start-server! [opts]
  (when-let [s @server]
    (http-server:stop! s))
  (let [s (http-server:create-server (assets:wrap-assets
                                       (fn [req]
                                         ((router:router routes {:merge-fn merge-data-fn
                                                                 :middleware [html:wrap-render
                                                                              ]}) req))
                                       {:roots ["public"]})
            opts)]
    (reset! server s)
    (http-server:start! s)
    s))

(start-server! {:port 9876})

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
