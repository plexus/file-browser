(module relative-time
  (:import
    [str :from piglet:string]))

(def relative-time-patterns
  "Word patterns, can be passed in for i18n"
  {:about-x-hours ["about 1 hour" "about {{count}} hours"]
   :about-x-months ["about 1 month" "about {{count}} months"]
   :about-x-weeks ["about 1 week" "about {{count}} weeks"]
   :about-x-years ["about 1 year" "about {{count}} years"]
   :almost-x-years ["almost 1 year" "almost {{count}} years"]
   :half-a-minute "half a minute"
   :less-than-x-seconds ["less than 1 second" "less than {{count}} seconds"]
   :less-than-x-minutes ["less than a minute" "less than {{count}} minutes"]
   :over-x-years ["over 1 year" "over {{count}} years"]
   :x-seconds ["1 second" "{{count}} seconds"]
   :x-minutes ["1 minute" "{{count}} minutes"]
   :x-days ["1 day" "{{count}} days"]
   :x-months ["1 month" "{{count}} months"]
   :x-years ["1 year" "{{count}} years"]})

(defn render-pattern [patterns k cnt]
  (let [p (get patterns k)
        p (cond
            (string? p)
            p
            (= 1 cnt)
            (first p)
            :else
            (second p))]
    (str:replace p #"\{\{count\}\}" (str cnt))))

(defn leap-year? [year]
  (and
    (= 0 (mod year 4))
    (or
      (not (= 0 (mod year 100)))
      (= 0 (mod year 400)))))

(defn distance-in-words
  ([from to]
   (distance-in-words from to nil))
  ([from to {:keys [patterns]
             :or {patterns relative-time-patterns}}]
   (let [[from to] (if (< to from) [to from] [from to])
         delta-ms  (- to from)
         delta-s   (js:Math.round (/ delta-ms 1000))
         delta-m   (js:Math.round (/ delta-ms 60000))
         delta-h   (js:Math.round (/ delta-ms 3600000))
         pattern   (partial render-pattern patterns)]
     (cond
       (<= 0 delta-m 1)
       (cond
         (<= 0 delta-s 4)   (pattern :less-than-x-seconds 5)
         (<= 5 delta-s 9)   (pattern :less-than-x-seconds 10)
         (<= 10 delta-s 19) (pattern :less-than-x-seconds 20)
         (<= 20 delta-s 39) (pattern :half-a-minute nil)
         (<= 40 delta-s 59) (pattern :less-than-x-minutes 1)
         :else
         (pattern :x-minutes 1))
       (<= 2 delta-m 44)         (pattern :x-minutes delta-m)
       (<= 45 delta-m 89)        (pattern :about-x-hours 1)
       ;; 90 mins up to 24 hours
       (<= 90 delta-m 1439)      (pattern :about-x-hours delta-h)
       ;; 1 day to 7 days
       (<= 1440 delta-m 10079)   (pattern :x-days (js:Math.round (/ delta-m 1440)))
       ;; 1 week to 4 weeks
       (<= 10080 delta-m 40319)  (pattern :about-x-weeks (js:Math.floor (/ delta-m 10080)))
       ;; 28 days to 30 days
       (<= 40320 delta-m 43199)   (pattern :about-x-months (js:Math.round (/ delta-m 40320)))
       ;; 30 days to 365 days
       (<= 43200 delta-m 525600) (pattern :x-months (js:Math.round (/ delta-m 43200)))

       :else
       (let [from (js:Date. from)
             to (js:Date. to)
             from-year                      (cond-> (:year from) (< 2 (:month from)) inc)
             to-year                        (cond-> (:year to) (< (:month from) 3) dec)
             leap-years                     (count (filter leap-year? (range from-year (inc to-year))))
             leap-year-minute-offset        (* leap-years 1440)
             minutes-offset                 (- delta-m leap-year-minute-offset)
             minutes-in-quarter-year 	    131400
             minutes-in-three-quarters-year 394200
             minutes-in-year                525600
             remainder                      (mod minutes-offset minutes-in-year)
             distance-years                 (quot minutes-offset minutes-in-year)]

         (cond
           (< remainder minutes-in-quarter-year)
           (pattern :about-x-years distance-years)

           (< remainder minutes-in-three-quarters-year)
           (pattern :over-x-years distance-years)

           :else
           (pattern :almost-x-years (inc distance-years))))))))
