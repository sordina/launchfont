(ns launchpad-bitmap-record.core (:gen-class :main true))

; Includes
(use 'midi)
(use 'matchure)
(use 'seesaw.core)

; Declarations
(declare main serialize clear-device exit trues cell-toggle central side switch stop-button handle-events neighbours set-cell glider handler bound getZ clear-device render newstate alive? cell-on cell-off cell-to-note note-to-cell)

; Constants
(def coords        (for [x (range 0 8) y (range 0 8)] [x y]))
(def lights        (midi-out "Launchpad"))
(def keyboard      (midi-in  "Launchpad"))
(def state         (atom {}))
(def side-bindings (atom {}))

(defn flush-file [] (prn (trues @state)))

(defn exit [&args] (do (clear-device) (System/exit 0)))

(native!)

(def life-window   (frame  :title    "Novation Launchpad Bitmap Recorder"
                           :on-close :exit))

(def life-button   (button :text  "Exit"))

(config! life-window :content life-button)
(listen  life-button :action  exit)

(defn -main [] (main))

; Main
(defn main [] (do (-> life-window pack! show!)
                  (config! life-window :size [500 :by 100])
                  (handle-events)
                  (clear-device)
                  (Thread/sleep 1000000000000))) ; TODO - Use a blocker instead

; Library

(defn handle-events [] (midi-handle-events keyboard handler))

(defn handler [x y] (cond-match [(:note x) (:vel x)]
                                [?n         0      ] (switch n)))  ; Only trigger on the release

(defn switch [n] (let [[x y] (note-to-cell n)]
                   (cond (and (<= 0 y) (< y 8) (<= 0 x) (< x 8)) (central [x y])
                         :else                                   (side    [x y]))))

(defn central [xy] (cell-toggle xy))

(defn bind-button [xy f] (cell-on xy) [xy f])

(defn setup-side-bindings [] (reset! side-bindings (apply hash-map (apply concat
  [(bind-button [8 4] #(exit))
   (bind-button [8 5] #(flush-file))
   ]))))

(defn side [xy] ((@side-bindings xy)))

(defn step [m] (doseq [xy coords] (newstate m xy)))

(defn newstate [m xy]
  (let [on-now  (getZ m xy)
        on-next (alive? on-now (apply + (neighbours m xy)))]

    ; Change the lights that need updating
    (cond (< 0 (bit-and          on-now (bit-not on-next))) (cell-off xy)
          (< 0 (bit-and (bit-not on-now)         on-next )) (cell-on  xy))))

(defn bound [a] (mod a 8))

(defn cell-on     [xy] (do (set-cell xy 1) (midi-note-on  lights (cell-to-note xy) 127)))
(defn cell-off    [xy] (do (set-cell xy 0) (midi-note-off lights (cell-to-note xy))))

(defn cell-toggle [xy] (if (= 0 (getZ @state xy)) (cell-on xy) (cell-off xy)))

(defn set-cell [xy v] (swap! state (fn [m] (assoc m xy v))))

(defn clear-device [] (do (doseq [xy coords] (cell-off xy)))
                          (setup-side-bindings))

(defn cell-to-note [xy] (let [[x y] xy] (+ x (* y 16))))

(defn note-to-cell [n ] [(mod n 16) (quot n 16)])

(defn getZ [m k] (let [v (m k)] (cond (nil? v) 0 :else v)))

(defn trues
  "Find true keys in a map"
  [m]
  (filter #(= 1 (m %)) (keys m)))
