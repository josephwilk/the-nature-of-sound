(ns the-nature-of-sound.example
  (import [xtract]
          [xtractJNI]))

(def v (xtract/doublea_to_voidp (xtract/new_double_array 1)))
(def len 8)
(def a (let [a (xtract/new_double-array len)]
         (doseq [idx (range 0 len)] (xtract/double_array_setitem a idx (double (* idx 2)))) a))
(xtract/double_array_getitem a 1)

(def r (double-array 1))
;;  public static int xtract_mean(SWIGTYPE_p_double data, int N, SWIGTYPE_p_void argv, double[] result) {
(xtract/xtract_mean a 7 nil r)
(println (str "Mean: " (first r)))
