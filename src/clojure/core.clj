(ns the-nature-of-sound.core
  (import xtractJNI)
  (gen-class))

(defn window-size [] (println (xtractJNI/XTRACT_WINDOW_SIZE_get)))
