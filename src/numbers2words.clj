(ns numbers2words)


(def ^:private single-digit-numerals ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",])


(def ^:private  tenthy-numerals
  (let  [numerals ["Ten", "Eleven","Twelve","Thirteen", "Fourteen","Fifteen","Sixteen","Seventeen","Eighteen","Nineteen"]]
   (fn [numeral]
    (-> numeral (mod 10)  (numerals)))))


(def ^:private double-digit-no-tenty-numerals
  (let [numerals  ["Twenty", "Thirty","Fourty", "Fifty","Sixty", "Seventy","Eighty","Ninty"]]
     (fn [numeral]
       (-> numeral  (- 2) (numerals)))))


(def ^:private power-of-ten-numeral      [ "Hundred", "Thousand", "Millions",])

(defn make-appender- [string-builder]
 (partial (comp (partial (memfn ^String append word) string-builder) str) \space))


;(defn number->words*)


(defn number->words*
  ([the-number] (number->words* the-number true))
  ( [the-number has-and?]
    (let  [words (StringBuilder.) append (make-appender- words)]
     (loop [ numeral  the-number]
      (cond
        (and (>= numeral 0)       (< numeral 10))         (append  (single-digit-numerals numeral))

        (and (>= numeral 10)      (< numeral 20))         (append (tenthy-numerals numeral))

        (and (>= numeral 20)      (< numeral 100))        (let [modulo (mod numeral 10)]
                                                            (append (double-digit-no-tenty-numerals (int (/ numeral 10))))
                                                            (when (pos? modulo)
                                                              (recur modulo)))

        (and (>= numeral 100)     (< numeral 1000))       (let [modulo (mod numeral 100)]
                                                            (append (single-digit-numerals (int (/ numeral 100))))
                                                            (append (power-of-ten-numeral 0))
                                                            (when (pos? modulo)
                                                              (when has-and? (append "And"))
                                                              (recur modulo)))))
     (str (.delete words 0 1)))))

(defn number->words  [the-number]
 (let  [words (StringBuilder.) append (make-appender- words)]
  (loop [ numeral  the-number]
   (cond
    (< numeral 1000)                                    (append (number->words* numeral))
    (and (>= numeral 1000)    (< numeral 1000000))      (let [modulo (mod numeral 1000)]
                                                         (append (number->words* (int (/ numeral 1000))))
                                                         (append "Thousand")
                                                         (when (pos? modulo)
                                                          (recur modulo)))

    (and (>= numeral 1000000)    (< numeral 999999999)) (let [modulo (mod numeral 1000000)]
                                                         (append (number->words* (int (/ numeral 1000000))))
                                                         (append "Million")
                                                         (when (pos? modulo)
                                                          (recur modulo)))))

  (str (.delete words 0 1))))



