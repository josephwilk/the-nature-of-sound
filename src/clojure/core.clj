(ns the-nature-of-sound.core
  (import [xtract]
          [xtractJNI]
          [xtract_features_]))

(defonce xtract-spectrum (.swigValue (xtract_features_/XTRACT_SPECTRUM)))

(defn- double->clojure [vs len]
  (let [ls (map-indexed
            (fn [idx v] (xtract/double_array_getitem vs idx))
            (range 0 len))]
    ls))

(defn- make-double [vs]
  (let [len (count vs)
        xtract-vs (xtract/new_double-array len)]
    (doall
     (map-indexed
      (fn [idx f] (xtract/double_array_setitem xtract-vs idx (double f)))
      vs))
    xtract-vs))

(defn mean [vs]
  (let [result (double-array 1)
        len (count vs)
        xtract-vs (xtract/new_double-array len)]
    (doall
     (map-indexed
      (fn [idx f] (xtract/double_array_setitem xtract-vs idx (double f)))
      vs))
    (xtract/xtract_mean xtract-vs len nil result)
    (xtract/delete_double_array xtract-vs)
    (double (first result))))

(defn variance [vs argv]
  (let [result (double-array 1)
        len (count vs)
        xtract-vs (make-double vs)]
    (xtract/xtract_variance xtract-vs len (xtract/doublea_to_voidp argv) result)
    (xtract/delete_double_array xtract-vs)
    (double (first result))))

(defn spectrum [vs rate]
  (let [len (count vs)
        argv (make-double [(double rate)])
        xtract-vs (make-double vs)
        result (make-double (double-array len))]

    (when (= 0 (mod len 2)) ;;FFT only works when divisible by 2
      (xtract/xtract_init_fft len xtract-spectrum)
      (xtract/xtract_spectrum xtract-vs len (xtract/doublea_to_voidp argv) result)
      (xtract/delete_double_array xtract-vs)

      (let [r (double->clojure result len)]
        (xtract/delete_double_array result)
        r))))

(let [m (mean [1 2 4 8 10 12 14 16])
      v (variance [1 2 4 8 10 12 14 16] (make-double [m]))
      spec (spectrum [1 2 4 8 10 12 14 16] (/ 44100.0 8))]

  (println (str "Mean:" m))
  (println (str "Var:" v))
  (println (str "Spectrum: " (pr-str spec))))
