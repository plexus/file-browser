(module node/http/assets
  "Serve static assets from the filesystem"
  (:import
    [str :from piglet:string]
    [fs :from "node:fs"]
    [fsp :from "node:fs/promises"]
    [mime-db :from "mime-db"]
    [path :from "node:path"]
    [stream-consumers :from "node:stream/consumers"]))

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

(defn header [req header]
  (some #(when (= (str:downcase (first %)) (str:downcase header))
           (second %)) (:headers req)))

(defn parse-ranges [range-header]
  (map (fn [range] (map #(when % (parse-long %)) (rest range)))
    (re-seq #"(\d+)-(\d+)?" range-header)))

(defn ^:async range-response* [stat range file]
  (let [ranges (parse-ranges range)]
    (if (= 1 (count ranges))
      (let [start  (ffirst ranges)
            end    (or
                     (first (second ranges))
                     (:size stat))
            stream (fs:createReadStream file #js {:start start
                                                  :end end})]
        {:status 206
         :headers {"Content-Type" (media-type file)
                   "ETag" (hash (:mtime stat))
                   "Content-Length" (- end start)
                   "Content-Disposition" (str "inline; filename=\"" (path:basename file) "\"")
                   "Content-Range" (str "bytes " start "-" (dec end) "/" (:size stat))}
         :body stream})
      (do
        (println "WARN: got multiple ranges in a range request, " range)
        {:status 200
         :body (str "multiple ranges not implemented " range)}))))

(defn ^:async range-response [etag range file]
  (->
    (fsp:stat file)
    (.then
      (fn [stat]
        (if (or (not etag) (= (str (hash (:mtime stat))) etag))
          (range-response* stat range file)
          (.then (fsp:readFile file)
            (fn [data]
              {:status 200
               :headers {"Content-Type" (media-type file)
                         "Content-Length" (:size stat)
                         "ETag" (hash (:mtime stat))
                         "Content-Disposition" (str "attachment; filename=\"" (path:basename file) "\"")
                         "Accept-Ranges" "bytes"}
               :body data})))))
    (.then
      identity
      (fn [err]
        {:status 500
         :body (str "Error loading file: " err)}))))

(defn ^:async file-response [etag file]
  (->
    (fsp:stat file)
    (.then
      (fn [stat]
        (if (and etag (= (str (hash (:mtime stat))) etag))
          {:status 304
           :headers {"Accept-Ranges" "bytes"}
           :body ""}

          (.then (fsp:readFile file)
            (fn [data]
              {:status 200
               :headers {"Content-Type" (media-type file)
                         "Content-Length" (:size stat)
                         "ETag" (hash (:mtime stat))
                         "Content-Disposition" (str "attachment; filename=\"" (path:basename file) "\"")
                         "Accept-Ranges" "bytes"}
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
           :headers {"Accept-Ranges" "bytes"}
           :body ""}
          {:status 200
           :headers {"Content-Type" (media-type file)
                     "ETag" (hash (:mtime stat))
                     "Content-Length" (:size stat)
                     "Content-Disposition" (str "attachment; filename=\"" (path:basename file) "\"")
                     "Accept-Ranges" "bytes"}})))
    (.then
      identity
      (fn [err]
        {:status 500
         :body (str "Error loading file: " err)}))))

(defn asset-response [{:keys [method] :as req} file]
  (cond
    (= :head method)
    (head-response (header req "if-none-match") file)

    (and (= :get method) (header req "range"))
    (range-response
      (header req "if-range")
      (header req "range")
      file)

    (= :get method)
    (file-response (header req "if-none-match") file)))

(defn wrap-assets
  "Serve static assets

  `roots` are directories to look for assets, `prefix` is the URL path
  prefix, can be blank."
  [handler {:keys [roots prefix]
            :or {prefix ""}
            :as opts}]
  (fn [{:keys [method path] :as req}]
    (if-let [file (and
                    (#{:get :head} method)
                    (str:starts-with? path prefix)
                    (find-asset roots (str:subs (:path req) (count prefix))))]
      (asset-response req file)
      (handler req))))

(comment
  ((wrap-assets identity {:roots ["public"]})
    {:method :get
     :path "/fonts/libre_franklin.css"}))
