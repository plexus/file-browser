(module chromecast
  (:import
    [ChromecastClient :from "chromecast-api"]))

(defn client []
  (ChromecastClient.))

(defn play-on-dev [dev link]
  (println "DEVICE" dev)
  (.on dev "status" (fn [status]
                      (println "GOT STATUS" status)))
  (.play dev link))

(defn play! [link]
  (let [client (client)]
    (println "CHROMECAST PLAY" link)
    (.on client "device" #(play-on-dev % link))
    (when-let [dev (first (.-devices client))]
      (play-on-dev dev link))))

;; (let [client (client)]
;;   (.on client "device"
;;     (fn [dev]
;;       (.stop dev))))

(comment
  (play! "https://bmo.squid.casa/file/Cloud%2FPutIO%2FDaria%2FSeason%204%2FDaria%20-%20S04E02%20-%20Antisocial%20Climbers.mp4/download")
  (play! "http://192.168.1.18:9876/file/1506389835.mp4/download")
  (play! "https://bmo.squid.casa/file/big_buck_bunny_1080p.mp4/download")
  (play! "http://localhost:8000/1506389835.mp4")
  (play! "https://bmo.squid.casa/download/1506389835.mp4?u=1KOU9Af0NN4tL09fHKTOSoYGQcsoy4GxjzJmszhGPni_slmOxiRtnA0kq8H77F8YAePBVrHVKYDhI9_ErSE9WA%3D%3D&stream=1&oauth_token=ZYNNRURI5GUBC3YBL727")
  (play! "https://s73-cdn77.put.io/download/1506389835.mp4?u=1KOU9Af0NN4tL09fHKTOSoYGQcsoy4GxjzJmszhGPni_slmOxiRtnA0kq8H77F8YAePBVrHVKYDhI9_ErSE9WA%3D%3D&stream=1&oauth_token=ZYNNRURI5GUBC3YBL727")
  (play! "http://192.168.1.40/file/Community%20(2009)%20Season%203%20S03%20%2B%20Extras%20(1080p%20BluRay%20x265%20HEVC%2010bit%20AAC%205.1%20RCVR)%2FCommunity%20(2009)%20-%20S03E06%20-%20Advanced%20Gay%20(1080p%20BluRay%20x265%20RCVR).mkv/download"))

(comment
  (.setVolume dev 0.5)
  (.pause dev)
  (def c (client))
  (.stop (first (.-devices c)))
  (.setVolume (first (.-devices c)) 0)

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
