(module main
  (:import
    chromecast html relative-time router
    [assets :from node/http/assets]
    [css :from piglet:css]
    [fs :from "node:fs"]
    [http-server :from piglet:node/http-server]
    [path :from "node:path"]
    [str :from piglet:string]))

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

(def root-dir "/mnt/Breadbox/Cloud/PutIO")
(def origin "http://192.168.1.40")

(defn format-byte-size [num]
  (if (= 0 num)
    "0 B"
    (let [prefixes ["" "k" "M" "G" "T" "P" "E" "Z" "B"]
          magnitude (js:Math.floor (/ (js:Math.log10 num) 3))
          scaled-num (/ num (js:Math.pow 1000 magnitude))]
      (str
        (.toLocaleString scaled-num undefined #js {:maximumFractionDigits 1})
        " "
        (get prefixes magnitude)
        "B"))))

(defn dir->data [dir]
  (for [file (fs:readdirSync dir)]
    (let [stat (fs:statSync (path:join dir file))]
      (assoc stat
        :dir? (.isDirectory stat)
        :dir dir
        :name file))))

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

(defn root? [dir]
  (#{"" "."} dir))

(defn dir-name [dir]
  (if (root? dir) "Your files" dir))

(defn dir-path [dir]
  (if (root? dir)
    "/"
    (path:join "/dir" (js:encodeURIComponent dir))))

(defn file-path [f type]
  (path:join "/file" (js:encodeURIComponent f) type))

(defc file-table-entry
  {:padding "1rem 0"
   :border-bottom "1px solid var(--theme-color-separator)"}
  [:.filename
   {:margin-bottom "0.3rem"
    :font-weight "450"}]
  [:.fileinfo
   {:color "var(--theme-color-text-secondary)"}]
  ([dir {:keys [dir? name mtimeMs size] :as stat}]
    (let [now (js:Date.now)
          path (path:join dir name)]
      [:div
       [:div.filename {:title path}
        [:a {:href (if dir? (dir-path path) (file-path path "preview"))}
         name]]
       [:div.fileinfo
        (format-byte-size size)
        " – "
        (relative-time:distance-in-words now mtimeMs) " ago"]])))

(defn file-table [dir files]
  (if (seq files)
    (for [f files]
      [file-table-entry dir f])
    [:p "No files"]))

(defc page-header
  {:margin-top "0"
   :padding "1rem 0"
   :border-bottom "3px solid var(--theme-color-accent)"}
  ([& children]
    (into [:header] children)))

(defc dir-overview-page
  {:padding "0 1rem"}
  ([dir parent files-info]
    [:div
     [page-header
      [:h1 (dir-name dir)]
      (when parent
        [:a {:href (dir-path parent)} "← " (if (= "." parent) "Your Files" parent)])]
     [file-table dir files-info]]))

(defc file-preview-page
  {:padding "0 1rem"}
  ([file parent]
    [:div
     [page-header
      [:h1 file]
      (when parent
        [:a {:href (dir-path parent)} "← " (if (= "." parent) "Your Files" parent)])]
     [:a {:href (file-path file "download")} "Download"]
     [:form {:method "POST" :action "./cast"}
      [:button "Play on Chromecast"]]]))

(defn GET-index [req]
  {:status 200
   :head [:title "File Browser"]
   :html
   [dir-overview-page
    "" nil
    (sort-by
      (comp - :mtimeMs)
      (dir->data root-dir))]})

(defn GET-directory [req]
  (let [dir (-> req :path-params :path)
        full-dir (path:join root-dir dir)
        parent (path:join dir "..")]
    (if (not (fs:existsSync full-dir))
      {:status 404
       :head [:title "Not found: " full-dir]
       :body [:p "Directory not found: " full-dir]}
      {:status 200
       :head [:title (dir-name dir)]
       :html
       [dir-overview-page dir parent
        (sort-by
          (comp - :mtimeMs)
          (dir->data full-dir))]})))

(defn GET-preview [req]
  (let [path (-> req :path-params :path)
        parent (path:join path "..")
        full-path (path:join root-dir path)]
    (if (not (fs:existsSync full-path))
      {:status 404
       :head [:title "Not found: " full-path]
       :body [:p "File not found: " full-path]}
      {:status 200
       :head [:title (dir-name path)]
       :html
       [file-preview-page path parent]})))

(defn GET-download [req]
  (let [path (-> req :path-params :path)
        full-path (path:join root-dir path)]
    (if (not (fs:existsSync full-path))
      {:status 404
       :head [:title "Not found: " full-path]
       :body [:p "File not found: " full-path]}
      (assets:asset-response req full-path))))

(defn GET-styles [req]
  {:status 200
   :headers {"Content-Type" "text/css"}
   :body (css:css (apply conj styles (vals @component-styles)))})

(defn POST-cast [req]
  (let [path (-> req :path-params :path)]
    (chromecast:play! (str origin (file-path path "download"))))
  {:status 302
   :headers {"Location" "./preview"}
   :body ""})

(defn base-layout [h]
  [:main h])

(def routes
  [["" {:html-head [:<>
                    [:link {:rel "stylesheet" :href "/fonts/libre_franklin.css"}]
                    [:link {:rel "stylesheet" :href "/styles.css"}]]
        :layout base-layout}
    ["/" {:get #'GET-index}]
    ["/dir/:path" {:get #'GET-directory}]
    ["/file/:path"
     ["/preview" {:get #'GET-preview}]
     ["/download" {:get #'GET-download
                   :head #'GET-download}]
     ["/cast" {:post #'POST-cast}]]

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
                                                                 :middleware [html:wrap-render]})
                                           req))
                                       {:roots ["public"]})
            opts)]
    (reset! server s)
    (http-server:start! s)
    s))

(start-server! {:port 9876})

(comment
  ((router:router routes {:merge-fn merge-data-fn
                          :middleware [html:wrap-render]})
    {:method :get
     :path "/file/libre-franklin-v19-latin_latin-ext.zip/download"})

  (router:match-path
    (router:compile-routes routes merge-data-fn)
    "/file/foo.bar/preview")

  (http-server:stop! server)
  (http-server:start! server))
