(ns day16)

(def example-input "D2FE28")
(def example-input2 "38006F45291200")
(def example-input3 "EE00D40C823060")
(def puzzle-input "C20D42002ED333E7774EAAC3C2009670015692C61B65892239803536C53E2D307A600ACF324928380133D18361005B336D3600B4BF96E4A59FED94C029981C96409C4A964F995D2015DE6BD1C6E7256B004C0010A86B06A1F0002151AE0CC79866600ACC5CABC0151006238C46858200E4178F90F663FBA4FDEC0610096F8019B8803F19A1641C100722E4368C3351D0E9802D60084DC752739B8EA4ED377DE454C0119BBAFE80213F68CDC66A349B0B0053B23DDD61FF22CB874AD1C4C0139CA29580230A216C9FF54AD25A193002A2FA002AB3A63377C124205008A05CB4B66B24F33E06E014CF9CCDC3A2F22B72548E842721A573005E6E5F76D0042676BB33B5F8C46008F8023301B3F59E1464FB88DCBE6680F34C8C0115CDAA48F5EE45E278380019F9EC6395F6BE404016849E39DE2EF002013C873C8A401544EB2E002FF3D51B9CAF03C0010793E0344D00104E7611C284F5B2A10626776F785E6BD672200D3A801A798964E6671A3E9AF42A38400EF4C88CC32C24933B1006E7AC2F3E8728C8E008C759B45400B4A0B4A6CD23C4AF09646786B70028C00C002E6D00AEC1003440080024658086A401EE98070B50029400C0014FD00489000F7D400E000A60001E870038800AB9AB871005B12B37DB004266FC28988E52080462973DD0050401A8351DA0B00021D1B220C1E0013A0C0198410BE1C180370C21CC552004222FC1983A0018FCE2ACBDF109F76393751D965E3004E763DB4E169E436C0151007A10C20884000874698630708050C00043E24C188CC0008744A8311E4401D8B109A3290060BE00ACEA449214CD7B084B04F1A48025F8BD800AB4D64426B22CA00FC9BE4EA2C9EA6DC40181E802B39E009CB5B87539DD864A537DA7858C011B005E633E9F6EA133FA78EE53B7DE80")

(def mapping
  {\0 [0 0 0 0]
   \1 [0 0 0 1]
   \2 [0 0 1 0]
   \3 [0 0 1 1]
   \4 [0 1 0 0]
   \5 [0 1 0 1]
   \6 [0 1 1 0]
   \7 [0 1 1 1]
   \8 [1 0 0 0]
   \9 [1 0 0 1]
   \A [1 0 1 0]
   \B [1 0 1 1]
   \C [1 1 0 0]
   \D [1 1 0 1]
   \E [1 1 1 0]
   \F [1 1 1 1]})

(defn parse [in]
  (mapcat mapping (seq in)))

(defn bits->int [x]
  (Long/parseLong (apply str x) 2))

(defn read-packet
  [bits]
  (let [[version bits] (split-at 3 bits)
        [id bits]      (split-at 3 bits)
        version (bits->int version)
        id (bits->int id)]
   (case id
     4 (loop [value []
              [[flag & nibble] & bits] (partition-all 5 bits)]
         (case flag
           0 [{:id id
               :version version
               :literal (bits->int (into value nibble))}
              (flatten bits)]
           1 (recur (into value nibble) bits)))

     (let [[operator & bits] bits]
       (cond
         (= 0 operator)
         (let [total-length (bits->int (take 15 bits))
               [bits rest] (split-at total-length (drop 15 bits))]
           (loop [bits bits
                  vs   []]
             (if (empty? bits)
               [{:id      id
                 :version version
                 :literal vs}
                rest]
               (let [[v rest] (read-packet bits)]
                 (recur rest (conj vs v))))))

         (= 1 operator)
         (let [packet-count (bits->int (take 11 bits))]
           (loop [bits (drop 11 bits)
                  vs   []
                  i    0]
             (if (= i packet-count)
               [{:id      id
                 :version version
                 :literal vs}
                bits]
               (let [[v rest] (read-packet bits)]
                 (recur rest
                        (conj vs v)
                        (inc i)))))))))))

(defn sum-versions [{:keys [id literal version]}]
  (case id
    4 version
    (apply + version (map sum-versions literal))))

(defn gt-than [x y] (if (> x y) 1 0))
(defn lt-than [x y] (if (< x y) 1 0))
(defn eql [x y] (if (= x y) 1 0))

(defn evaluate [{:keys [id literal]}]
  (case id
    4 literal
    0 (->> literal (map evaluate) (reduce +))
    1 (->> literal (map evaluate) (reduce *))
    2 (->> literal (map evaluate) (reduce min))
    3 (->> literal (map evaluate) (reduce max))
    5 (->> literal (map evaluate) (reduce gt-than))
    6 (->> literal (map evaluate) (reduce lt-than))
    7 (->> literal (map evaluate) (reduce eql))))

(defn solve* [in]
  (first (read-packet (parse in))))

(defn solve-1 [in]
  (sum-versions (solve* in)))

(defn solve-2 [in]
  (evaluate (solve* in)))

(comment
 ; 2021
 (read-packet (parse example-input))

 (solve-1 example-input2) ;; 9
 (solve-1 example-input3) ;; 14
 (solve-1 "8A004A801A8002F478") ;; 16
 (solve-1 "620080001611562C8802118E34") ;; 12
 (solve-1 "C0015000016115A2E0802F182340") ;; 23
 (solve-1 "A0016C880162017C3686B18A3D4780") ;; 31

 ;; 943
 (solve-1 puzzle-input)

 (solve-2 "C200B40A82") ;; 3
 (solve-2 "04005AC33890") ;; 54
 (solve-2 "880086C3E88112") ;; 7
 (solve-2 "CE00C43D881120") ;; 9
 (solve-2 "D8005AC2A8F0") ;; 1
 (solve-2 "F600BC2D8F") ;; 0
 (solve-2 "9C005AC2F8F0") ;; 0
 (solve-2 "9C0141080250320F1802104A08") ;; 1

 ;; 167737115857
 (solve-2 puzzle-input))
