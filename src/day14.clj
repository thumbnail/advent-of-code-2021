(ns day14
  (:require [clojure.string :as str]))

(def example-input "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C")
(def puzzle-input "SCSCSKKVVBKVFKSCCSOV\n\nCP -> C\nSF -> S\nBH -> F\nSS -> N\nKB -> N\nNO -> N\nBP -> F\nNK -> P\nVP -> H\nOF -> O\nVH -> O\nFV -> F\nOP -> V\nFP -> B\nVB -> B\nOK -> S\nBS -> B\nSK -> P\nVV -> H\nPC -> S\nHV -> K\nPS -> N\nVS -> O\nHF -> B\nSV -> C\nHP -> O\nNF -> V\nHB -> F\nVO -> B\nVN -> N\nON -> H\nKV -> K\nOV -> F\nHO -> H\nNB -> K\nCB -> F\nFF -> H\nNH -> F\nSN -> N\nPO -> O\nPH -> C\nHH -> P\nKF -> N\nOH -> N\nKS -> O\nFH -> H\nCC -> F\nCK -> N\nFC -> F\nCF -> H\nHN -> B\nOC -> F\nOB -> K\nFO -> P\nKP -> N\nNC -> P\nPN -> O\nPV -> B\nCO -> C\nCS -> P\nPP -> V\nFN -> B\nPK -> C\nVK -> S\nHS -> P\nOS -> N\nNP -> K\nSB -> F\nOO -> F\nCV -> V\nBB -> O\nSH -> O\nNV -> N\nBN -> C\nKN -> H\nKC -> C\nBK -> O\nKO -> S\nVC -> N\nKK -> P\nBO -> V\nBC -> V\nBV -> H\nSC -> N\nNN -> C\nCH -> H\nSO -> P\nHC -> F\nFS -> P\nVF -> S\nBF -> S\nPF -> O\nSP -> H\nFK -> N\nNS -> C\nPB -> S\nHK -> C\nCN -> B\nFB -> O\nKH -> O")

(defn parse [in]
  (let [[polymer _ & rules] (str/split-lines in)]
    {:polymer polymer
     :rules   (reduce (fn [m rule]
                        (let [[k v] (str/split rule #" -> ")]
                         (apply assoc m (vec k) v)))
                      {}
                      rules)}))

(defn insert [{:keys [polymer rules]} steps]
  (loop [remaining steps
         [pairs letter-freq] [(frequencies (partition 2 1 (seq polymer)))
                              (frequencies polymer)]]
    (if (zero? remaining)
      letter-freq
      (recur (dec remaining)
             (reduce (fn [[pairs letter-freq] [[a c :as pair] freq]]
                       (let [b (rules pair)]
                         [(-> pairs
                              (update [a c] (fnil - 0) freq)
                              (update [a b] (fnil + 0) freq)
                              (update [b c] (fnil + 0) freq))
                          (update letter-freq b (fnil + 0) freq)]))
                     [pairs letter-freq]
                     pairs)))))

(defn solve [in steps]
  (let [freqs (insert (parse in) steps)
        max (apply max (vals freqs))
        min (apply min (vals freqs))]
    (- max min)))

(comment
 (parse example-input)

 ;; 1588
 (solve example-input 10)

 ;; 2112
 (solve puzzle-input 10)

 ;; 3243771149914
 (solve puzzle-input 40))
