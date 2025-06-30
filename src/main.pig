(module main
  (:import
    router
    [http-server :from piglet:node/http-server]))

(def routes
  [["/" {:name :index
         :get (fn [req] {:status 200 :body "OK"})}]

   ["/foo/:id" {:get (fn [req] {:status 200 :body (print-str req)})}]])


(def server
  (http-server:create-server (fn [req]
                               ((router:router routes) req))
    {:port 9876}))

(router:match-path
  (router:compile-routes routes)
  "/foo/123")

(http-server:stop! server)
(http-server:start! server)
