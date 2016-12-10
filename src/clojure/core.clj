(ns the-nature-of-sound.core
  (:require [clojure.java.io :as io]
            [dynne.sampled-sound :as sample])
  (:import [xtract]
           [xtractJNI]
           [xtract_mel_filter]
           [xtract_features_]
           [xtract_window_types_]
           [xtract_spectrum_]

          [javax.sound.sampled AudioSystem]))

(defonce xtract-spectrum (.swigValue (xtract_features_/XTRACT_SPECTRUM)))
(defonce xtract-windowed (.swigValue (xtract_features_/XTRACT_WINDOWED)))
(defonce xtract-hann     (.swigValue (xtract_window_types_/XTRACT_HANN)))

(defonce xtract-equal-gain (.swigValue (xtract_mfcc_types_/XTRACT_EQUAL_GAIN)))

(defonce xtract-spectrum-magnitude (.swigValue (xtract_spectrum_/XTRACT_MAGNITUDE_SPECTRUM)))

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

(defn spectrum [vs sample-rate]
  (let [len (count vs)
        argv (make-double [(double sample-rate)])
        xtract-vs (make-double vs)
        result (make-double (double-array len))]

    (when (= 0 (mod len 2)) ;;FFT only works when divisible by 2
      (xtract/xtract_init_fft len xtract-spectrum)
      (xtract/xtract_spectrum xtract-vs len (xtract/doublea_to_voidp argv) result)
      (xtract/delete_double_array xtract-vs)

      (let [r (double->clojure result len)]
        (xtract/delete_double_array result)
        r))))

(defn features-from-subframes [vs window-size window-type]
  (let [len (count vs)
        xtract-vs (make-double vs)
        window (xtract/xtract_init_window window-size window-type)
        result (make-double (double-array len))]

    (xtract/xtract_features_from_subframes xtract-vs
                                           len
                                           xtract-windowed
                                           (xtract/doublea_to_voidp window)
                                           result)
    (let [r (double->clojure result len)]
      (xtract/delete_double_array xtract-vs)
      (xtract/delete_double_array result)
      (xtract/xtract_free_window window)
      r)))

(let [m (mean [1 2 4 8 10 12 14 16])
      v (variance [1 2 4 8 10 12 14 16] (make-double [m]))
      spec (spectrum [1 2 4 8 10 12 14 16] (/ 44100.0 8))
      sub (features-from-subframes [1 2 4 8 10 12 14 16]
                                   (/ 8 2)
                                   xtract-hann)]

  (println (str "Mean:" m))
  (println (str "Var:" v))
  (println (str "Spectrum: " (pr-str spec)))
  (println (str "Features from subframes (window 4, hann): " (pr-str sub)))
  )


(let [path "test/fixtures/test.wav"  ;;             "test/fixtures/the_nature_of_sound.wav"
      s    (sample/read-sound path)
      MFCC_FREQ_BANDS (double 13)
      MFCC_FREQ_MIN (double 20)
      MFCC_FREQ_MAX (double 20000)
      MFCC_FREQ_BANDS 13
      SAMPLERATE 44100
      data   (sample/chunks s SAMPLERATE)
      data-slice (ffirst data)
      data-size (count data-slice)
      block-size 512
      half-block-size (/ 512 2)

      mel-filters  (xtract/create_filterbank  MFCC_FREQ_BANDS block-size)]

  (xtract/xtract_init_mfcc (/ block-size 2)
                           (/ SAMPLERATE 2)
                           xtract-equal-gain
                           MFCC_FREQ_MIN
                           MFCC_FREQ_MAX
                           MFCC_FREQ_BANDS
                           (xtract_mel_filter/.getFilters mel-filters))

  (let [w    (xtract/xtract_init_window block-size xtract-hann)
        subw (xtract/xtract_init_window half-block-size xtract-hann)
        argv (make-double [(double SAMPLERATE)])]

    ;;LEAKING ALL THE MEMORY
    (xtract/xtract_init_wavelet_f0_state)

    (doseq [s data]
      (doseq [v s]
        (let [ds (make-double (vec v))
              r  (double-array [1])]
          (xtract/xtract_wavelet_f0 ds block-size (xtract/doublea_to_voidp argv) r)
          (let [f0 (first (vec r))]
            (xtract/xtract_midicent nil 0 (xtract/doublea_to_voidp (make-double (vec r))) r)
            (let [cents (first (vec r))]
              (when (not= f0 0.0)
                (println "f0       :" f0)
                (println "midi-note:" (/ cents 100)))))


          ;;    xtract_windowed(&data[n], BLOCKSIZE, window, windowed);
          ;;  public static int xtract_windowed(SWIGTYPE_p_double data, int N, SWIGTYPE_p_void argv, SWIGTYPE_p_double result)
          (let [windowed (make-double (range 0 block-size))
                spectrum (make-double (range 0 block-size))

                argd (make-double [(/ SAMPLERATE block-size)
                                   xtract-spectrum-magnitude
                                   (double 0)
                                   (double 0)])]
            (xtract/xtract_windowed ds block-size (xtract/doublea_to_voidp w) windowed)
            (xtract/xtract_init_fft block-size xtract-spectrum)
            (xtract/xtract_spectrum windowed block-size (xtract/doublea_to_voidp argd) spectrum)
            (xtract/xtract_free_fft)

            (println "Magnitude Spectrum: " (pr-str (double->clojure spectrum block-size)))
            ))))

    (xtract/xtract_free_window w)
    (xtract/xtract_free_window subw)
    (xtract/destroy_filterbank mel-filters)))

;;f0: 167.04545454545453
;;f0: 196.875
;;f0: 303.440366972477
;;f0: 263.54581673306774
;;f0: 263.44086021505376
;;f0: 264.86486486486484
;;f0: 266.0633484162896
;;f0: 263.54581673306774
;;f0: 263.2835820895522
;;f0: 258.3984375
