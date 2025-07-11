(module chromecast
  (:import
    [ChromecastClient :from "chromecast-api"]))

(def client (ChromecastClient.))

(defn play! [link]
  (println "CHROMECAST PLAY" link)
  (let [dev (first (.-devices client))]
    (.play dev link)))


(comment
  (.setVolume dev 0.5)
  (.stop dev)
(.pause dev)

  (do
    (.on dev "status" (fn [status]
                        (println "GOT STATUS" status)
                        (def sss status)
                        ))
    nil)

  (type-name sss)

  (keys (first (.-devices client)))

  (.on client
    "device"
    (fn [dev]
      (println "GOT DEV" dev)
      (swap! devices (constantly dev))))

  (.update client))
