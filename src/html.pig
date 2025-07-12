(module html
  "Piglet forms to HTML (XML, really) string rendering

   See also [[piglet:dom]] for direct rendering to DOM objects."
  (:import
    [str :from piglet:string]))

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

;; TODO: dasherized-to-camelcase with awareness of data- and svg attributes (configurable prefixes)
(defn render-opts [opts]
  (str:join
    (for [[k v] opts]
      (str " " (name k) "='" (str:replace (str v) #"'" "&apos;") "'"))))

(defn escape-string [s]
  (-> s
    (str:replace #"&" "&amp;")
    (str:replace #"<" "&lt;")))

(def block-level
  "For these elements we introduce a newline after the opening
  and before the closing tag, to keep the result a little more readable"
  #{"html" "head" "body" "div" "p" "main" "article" "section" "ul" "ol" "header"
    "footer"})

(defn render
  "Render HTML forms to string

  Syntax understood

  - `[:tag {...attrs} ...children]` : Regular elements
  - `[fn ...args]` : component style
  - `[:<> ...children]` : HTML fragment
  - `[:doctype :html]` : Doctype declaration

  Tags accept class and id shorthand, e.g. `:input#password.error` =>
  `<input id='password' class='error />`.
  "
  [h]
  (cond
    (string? h)
    (escape-string h)

    (seq? h)
    (apply str (map render h))

    (vector? h)
    (cond (fn? (first h))
      (render
        (apply (first h) (rest h)))

      (= :<> (first h))
      (apply str (map render (rest h)))

      (= :doctype (first h))
      (str "<!DOCTYPE " (name (second h)) ">\n")

      :else
      (let [[tag ?opts & children] h
            [opts children] (if (dict? ?opts)
                              [?opts children]
                              [nil (when ?opts (cons ?opts children))])
            [tag id klz] (split-tag tag)
            opts (cond-> opts
                   id (assoc :id id)
                   (seq klz) (update :class #(str:join " "
                                               (if (string? %)
                                                 (conj klz %)
                                                 (concat klz %)))))
            block? (block-level tag)]

        (str  "<" tag (render-opts opts)
          (str ">"
            ;; Add a couple newlines so it's not just one single line of text. This is a far cry from
            ;; actual pretty output because that requires more context-aware generation, but marginally
            ;; easier to parse if you happen to look at the raw output.
            (when block? (str "\n"))
            (apply str (map render children))
            (when block? (str "\n"))
            "</" tag ">"))))

    :else
    (str h)))

(defn default-page-layout [head body]
  [:<>
   [:doctype :html]
   [:html
    [:head
     [:meta {:charset "utf-8"}]
     [:meta {:content "width=device-width, initial-scale=1" :name "viewport"}]
     head]
    [:body
     body]]])

(defn wrap-render
  "HTTP Middleware that handles HTML rendering

  Understands the following keys either in the response map or in route-data:

  - `:html` - HTML form, inserted in the body section of the page layout
  - `:head` - HTML form, inserted in the head section of the page layout
  - `:page-layout` - Two-arg function that takes a body and head form and returns a HTML form
  - `:layout` - One-arg function that wraps the HTML form
  "
  [handler]
  ;; TODO: when also a :body a specified, the HTML rendering should only happen when the client
  ;; asked for it (Accept: text/html), so that otherwise other middleware can do content
  ;; negotiating to render the body (collection) to an appropriate format.
  (assert handler)
  (fn [req]
    (let [res (handler req)]
      (if (not (vector? (:html res)))
        res
        (let [route-data (:route-data req)
              page-layout (or (:page-layout res) (:page-layout route-data) default-page-layout)
              layouts (keep :layout [res route-data])
              head (cond-> [:<>]
                     (:html-head route-data)
                     (conj (:html-head route-data))
                     (:html-head res)
                     (conj (:html-head res)))
              body (:html res)]
          (-> res
            (assoc :body (render (page-layout head (reduce (fn [acc f] (f acc)) body layouts))))
            (assoc-in [:html-headers "Content-Type"] "text/html")))))))

(comment
  (render [:<>
           [:doctype :html]
           [:html
            [:head

             [:p [:span [:em [:a {:href "xxx"}"xxx"]]]]
             [:span "yy"]
             [:p [:em "xxx"]]]]]))
