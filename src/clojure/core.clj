(ns the-nature-of-sound.core
  (:require [clojure.java.io :as io]
            [dynne.sampled-sound :as sample])
  (:import [xtract]
           [xtractJNI]
           [xtract_mel_filter]
           [xtract_features_]
           [xtract_window_types_]
           [xtract_spectrum_]
           [xtract_subband_scales_]
           [xtractConstants]

          [javax.sound.sampled AudioSystem]))

(defonce xtract-spectrum (.swigValue (xtract_features_/XTRACT_SPECTRUM)))
(defonce xtract-windowed (.swigValue (xtract_features_/XTRACT_WINDOWED)))
(defonce xtract-bark-coefficents (.swigValue (xtract_features_/XTRACT_BARK_COEFFICIENTS)))

(defonce xtract-bark-bands (xtractConstants/XTRACT_BARK_BANDS))

(defonce xtract-hann     (.swigValue (xtract_window_types_/XTRACT_HANN)))

(defonce xtract-equal-gain (.swigValue (xtract_mfcc_types_/XTRACT_EQUAL_GAIN)))

(defonce xtract-spectrum-magnitude (.swigValue (xtract_spectrum_/XTRACT_MAGNITUDE_SPECTRUM)))

(defonce xtract-linear-subbands  (.swigValue (xtract_subband_scales_/XTRACT_LINEAR_SUBBANDS)))

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

(defn- xtract-args [& args] (xtract/doublea_to_voidp (make-double args)))

(defn with-result [t xtract-fn]
  (let [r t]
    (xtract-fn r)
    (let [out (nth (vec r) 0)]
      out)))

(defn spectral-inharmonicity [peaks-data block-size & args]
  (with-result
    (double-array [0])
    (fn [r] (xtract/xtract_spectral_inharmonicity peaks-data block-size (apply xtract-args args) r))))

(defn wavelet_f0 []

  )

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


(defn peek-inside [sample-path]
  (let [path sample-path
        s    (sample/read-sound path)
        MFCC_FREQ_BANDS (double 13)
        MFCC_FREQ_MIN   (double 20)
        MFCC_FREQ_MAX   (double 20000)
        NUM_HARMONICS   10
        MFCC_FREQ_BANDS 13
        SAMPLERATE 44100
        data   (sample/chunks s SAMPLERATE)
        data-slice (ffirst data)
        data-size (count data-slice)
        block-size 512
        half-block-size (/ 512 2)

        mel-filters  (xtract/create_filterbank  MFCC_FREQ_BANDS block-size)

      ;;  bark-band-limits (xtract/new_int_array 0)
    ;;    _ (xtract/int_array_setitem bark-band-limits 0 0)
        ]

    (xtract/xtract_init_mfcc (/ block-size 2)
                             (/ SAMPLERATE 2)
                             xtract-equal-gain
                             MFCC_FREQ_MIN
                             MFCC_FREQ_MAX
                             MFCC_FREQ_BANDS
                             (xtract_mel_filter/.getFilters mel-filters))

    ;;  public static int xtract_init_bark(int N, double sr, SWIGTYPE_p_int band_limits) {
  ;;  (xtract/xtract_init_bark block-size SAMPLERATE bark-band-limits)

;;    (println (xtract/int_array_getitem bark-band-limits 0))

    (let [w    (xtract/xtract_init_window block-size xtract-hann)
          subw (xtract/xtract_init_window half-block-size xtract-hann)
          argv (make-double [(double SAMPLERATE)])]

      ;;LEAKING ALL THE MEMORY
      (xtract/xtract_init_wavelet_f0_state)

      (let [stats
            (map-indexed
             (fn [idx s]
               (map
                (fn [v]
                  (let [ds (make-double (vec v))
                        r  (double-array [1])]

                    (xtract/xtract_wavelet_f0 ds block-size (xtract/doublea_to_voidp argv) r)

                    (let [f0 (first (vec r))]
                      (xtract/xtract_midicent nil 0 (xtract/doublea_to_voidp (make-double (vec r))) r)
                      (let [cents (first (vec r))
                            midi (format "%.0f" (/ cents 100))]

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

                          (let [peaks (make-double (range 0 block-size))
                                argv (make-double [(/ SAMPLERATE block-size)
                                                   (double 10)
                                                   (double 0)
                                                   (double 0)])]

                            (xtract/xtract_peak_spectrum spectrum (double (/ block-size 2)) (xtract/doublea_to_voidp argv) peaks)


;;                            public static int xtract_rms_amplitude(SWIGTYPE_p_double data, int N, SWIGTYPE_p_void argv, double[] result)

                            (let [rms (double-array [0])
                                  noiseness (double-array [0])
                                  loudness (double-array [0])
                                  tonality (double-array [0])
                                  irregularity_k (double-array [0])
                                  argv [(/ SAMPLERATE block-size)]
                                  spectral-irregularity (double-array [0])
                                  spectral-centroid (double-array [0])
                                  spectral-variance (double-array [0])

                                  spectral-inharmonicity (spectral-inharmonicity
                                                          peaks block-size
                                                          f0 0.5 NUM_HARMONICS 0)]

                              (xtract/xtract_rms_amplitude ds block-size (apply xtract-args argv) rms)

                              (xtract/xtract_spectral_centroid spectrum block-size nil spectral-centroid)
                              (xtract/xtract_irregularity_j    spectrum half-block-size nil spectral-irregularity)


                              (let [spectral-skewness (double-array [0])
                                    spectral-std-deviation (double-array [0])
                                     spectral-mean (double-array [0])
                                    spectral-kurtosis (double-array [0])
                                    argv (make-double [])]

                                (xtract/xtract_spectral_mean spectrum block-size nil spectral-mean)
                                (xtract/xtract_variance spectrum block-size (xtract-args (first spectral-mean)) spectral-variance)
                                (xtract/xtract_spectral_standard_deviation spectrum block-size (xtract-args (first spectral-variance)) spectral-std-deviation)

                                (println :std (first spectral-std-deviation))
                                (println :cen (first spectral-centroid))

                                (let [argv (make-double [(first spectral-mean)
                                                         (first spectral-std-deviation)]) ])

                                ;;fails
                                (xtract/xtract_spectral_skewness spectrum block-size (xtract/doublea_to_voidp argv) spectral-skewness)
                                (xtract/xtract_spectral_kurtosis spectrum block-size (xtract/doublea_to_voidp argv) spectral-kurtosis)

                                ;;                              (xtract/xtract_noisiness     ds block-size (xtract-args NUM_HARMONICS ) noiseness)

                                (let [_ 10
                                      ;;bark-cofficents (make-double (range 0 xtract-bark-bands))
                                      ]

                                  ;; (println :BAND bark-band-limits)
                                  ;;                                (xtract/xtract_bark_coefficients spectrum half-block-size (xtract-args ) bark-cofficents)
                                  ;;                                (xtract/xtract_loudness      ds block-size (apply xtract-args argv) loudness)
                                  )

                                ;;TODO
;;                                (xtract/xtract_tonality      ds block-size (xtract-args ) tonality)

                                {(* idx SAMPLERATE)
                                 {:spectral-inharmonicity   spectral-inharmonicity
                                  :spectral-irregularity    (first spectral-irregularity)
                                  :spectral-centroid        (first spectral-centroid)

                                  :todo {:spectral-skewness (first spectral-skewness)
                                         :spectral-kurtosis (first spectral-kurtosis)}
                                  :rms_amplitude            (first rms)

                                  ;;                              :noisiness (first noiseness)
                                  ;;                                :loudness (first loudness)
                                  ;;                               :tonality (first tonality)
                                  :midi midi
                                  :f0 f0}
                                 })))
                              ;;(println "Magnitude Spectrum: " (pr-str (double->clojure spectrum block-size)))
                          )))))
                s))
             data)]

        (xtract/xtract_free_window w)
        (xtract/xtract_free_window subw)
        (xtract/destroy_filterbank mel-filters)

        (vec (flatten stats))
        ))))

(println "" )
(doseq [r (peek-inside "test/fixtures/test.wav")]
  (println r))
