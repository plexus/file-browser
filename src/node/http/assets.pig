(module node/http/assets
  "Serve static assets from the filesystem"
  (:import
    [str :from piglet:string]
    [fs :from "node:fs"]
    [fsp :from "node:fs/promises"]
    [mime-db :from "mime-db"]
    [path :from "node:path"]))

(def ext->mime
  "File extension to MIME type map"
  (into {"pig" ["application/piglet" true "UTF-8"]}
    (for [[mime opts] (->pig mime-db:default)
          :when (:extensions opts)
          ext (:extensions opts)
          :let [comp (:compressible opts)
                charset (:charset opts)]]
      [ext [(name mime) comp charset]])))

(defn find-asset
  "Find the given HTTP path in one of the roots (directories)"
  [roots path]
  (let [asset (some (fn [root]
                      (let [asset (path:resolve root (str "." path))]
                        (when (fs:existsSync asset)
                             asset)))
                roots)]
    (if (and asset (.isDirectory (fs:lstatSync asset)))
      (let [index (str asset "/index.html")]
        (when (fs:existsSync index)
          index))
      asset)))

(defn media-type [filename]
  (let [[type _ charset] (or (get ext->mime (last (str:split "." filename)))
                           [])]

    (cond
      charset
      (str type ";charset=" charset)
      type
      type
      :else
      "application/octet-stream")))

(defn ^:async file-response [etag file]
  (-> (fsp:stat file)
    (.then
      (fn [stat]
        (if (and etag (= (str (hash (:mtime stat))) etag))
          {:status 304
           :headers {}
           :body ""}
          (.then (fsp:readFile file)
            (fn [data]
              {:status 200
               :headers {"Content-Type" (media-type file)
                         "ETag" (hash (:mtime stat))}
               :body data})))))
    (.then
      identity
      (fn [err]
        {:status 500
         :body (str "Error loading file: " err)}))))

(defn ^:async head-response [etag file]
  (-> (fsp:stat file)
    (.then
      (fn [stat]
        (if (and etag (= (str (hash (:mtime stat))) etag))
          {:status 304
           :headers {}
           :body ""}
          (.then (fsp:readFile file)
            (fn [data]
              {:status 200
               :headers {"Content-Type" (media-type file)
                         "ETag" (hash (:mtime stat))
                         "Content-Length" (count data)}
               })))))
    (.then
      identity
      (fn [err]
        {:status 500
         :body (str "Error loading file: " err)}))))

(defn wrap-assets
  "Serve static assets

  `roots` are directories to look for assets, `prefix` is the URL path
  prefix, can be blank."
  [handler {:keys [roots prefix]
            :or {prefix ""}
            :as opts}]
  (println "HANDLER" handler)
  (println "OPTS" opts)
  (fn [{:keys [method path] :as req}]
    (if-let [file (and
                    (#{:get :head} method)
                    (str:starts-with? path prefix)
                    (find-asset roots (str:subs (:path req) (count prefix))))]
      (if (= :get method)
        (file-response (get-in req [:headers "if-none-match"]) file)
        (head-response (get-in req [:headers "if-none-match"]) file))
      (handler req))))

(comment
  ((wrap-assets identity {:roots ["public"]})
    {:method :get
     :path "/fonts/libre_franklin.css"}))
