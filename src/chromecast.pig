(module chromecast
  (:import
    [ChromecastClient :from "chromecast-api"]))

(defn client []
  (ChromecastClient.))

(defn play! [link]
  (let [client (client)
        dev (first (.-devices client))]
    (println "CHROMECAST PLAY" link)
    (println "DEVICE" dev)
    (.play dev link)))

(comment
  (play! "http://192.168.1.40/file/Community%20(2009)%20Season%203%20S03%20%2B%20Extras%20(1080p%20BluRay%20x265%20HEVC%2010bit%20AAC%205.1%20RCVR)%2FCommunity%20(2009)%20-%20S03E06%20-%20Advanced%20Gay%20(1080p%20BluRay%20x265%20RCVR).mkv/download"))

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
