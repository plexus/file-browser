(module chromecast
  (:import
    [ChromecastClient :from "chromecast-api"]))

(defn client []
  (ChromecastClient.))

(defn play! [link]
  (let [client (client)
        dev (first (.-devices client))]
    (println "CHROMECAST PLAY" link)
    (.on client "device"
      (fn [dev]
        (println "DEVICE" dev)
        (.on dev "status" (fn [status]
                            (println "GOT STATUS" status)))
        (.play dev link)))))

;; (let [client (client)]
;;   (.on client "device"
;;     (fn [dev]
;;       (.stop dev))))

(comment
  (play! "http://192.168.1.18:9876/file/1506389835.mp4/download")
  (play! "http://localhost:8000/1506389835.mp4")
  (play! "https://s73-cdn77.put.io/download/1506389835.mp4?u=1KOU9Af0NN4tL09fHKTOSoYGQcsoy4GxjzJmszhGPni_slmOxiRtnA0kq8H77F8YAePBVrHVKYDhI9_ErSE9WA%3D%3D&stream=1&oauth_token=ZYNNRURI5GUBC3YBL727")
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
