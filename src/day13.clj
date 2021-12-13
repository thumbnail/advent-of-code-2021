(ns day13
  (:require [clojure.string :as str]))

(def example-input "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5")
(def puzzle-input "1148,688\n1020,159\n857,707\n1176,415\n388,275\n50,849\n544,520\n1,418\n1119,280\n217,26\n358,110\n1302,684\n910,791\n45,287\n544,806\n423,859\n529,866\n1150,686\n977,668\n666,245\n380,603\n892,343\n437,483\n1109,411\n296,534\n763,676\n711,218\n139,774\n923,397\n1202,341\n664,12\n416,89\n522,54\n524,401\n541,796\n1153,523\n1101,427\n977,308\n102,856\n769,546\n966,523\n1218,820\n833,682\n872,806\n457,542\n333,586\n238,537\n528,103\n169,840\n321,44\n102,408\n825,346\n1220,791\n1223,584\n907,597\n785,766\n147,654\n201,707\n775,674\n746,651\n221,354\n213,460\n843,105\n59,110\n224,773\n654,368\n666,21\n1134,736\n475,121\n445,799\n1141,411\n835,688\n810,12\n810,882\n1064,51\n1240,805\n266,723\n125,812\n1048,299\n213,68\n129,738\n100,256\n666,551\n1004,632\n1250,505\n119,674\n565,264\n937,220\n387,497\n1235,259\n8,658\n902,294\n159,884\n222,159\n184,645\n700,646\n1251,110\n53,469\n1228,282\n157,779\n862,759\n694,31\n10,876\n652,110\n1104,505\n554,582\n574,742\n985,642\n410,830\n873,420\n623,42\n70,89\n922,395\n627,446\n724,848\n610,534\n1042,283\n852,731\n370,193\n109,420\n1275,239\n582,546\n88,537\n1133,682\n584,847\n835,654\n299,649\n735,82\n467,547\n408,51\n1278,233\n758,325\n1044,171\n233,364\n490,16\n326,187\n455,114\n1231,504\n1300,876\n403,830\n1185,418\n731,187\n256,354\n1200,725\n1101,338\n781,399\n1207,147\n1153,502\n335,752\n716,389\n811,747\n619,311\n865,95\n221,443\n567,310\n503,112\n646,658\n296,617\n314,40\n813,483\n1223,252\n813,502\n566,724\n445,95\n832,437\n191,614\n268,611\n162,78\n303,5\n865,114\n373,108\n688,9\n692,511\n403,696\n1071,724\n786,401\n544,164\n150,457\n75,198\n1006,607\n441,68\n300,873\n160,686\n1059,646\n53,47\n850,847\n458,182\n1265,259\n869,826\n571,241\n408,395\n1007,889\n515,95\n458,163\n154,761\n642,774\n571,653\n199,266\n364,719\n109,474\n700,360\n160,14\n671,345\n448,583\n711,666\n296,277\n984,187\n1096,824\n381,469\n649,624\n1310,635\n1206,607\n392,35\n381,698\n520,479\n482,145\n498,444\n239,170\n601,21\n522,840\n1067,770\n415,259\n827,595\n52,725\n251,597\n0,10\n150,736\n194,840\n504,630\n586,275\n140,387\n716,284\n619,127\n1066,178\n827,299\n858,634\n652,336\n738,612\n1016,233\n431,595\n760,656\n909,803\n960,679\n171,376\n45,194\n687,852\n923,385\n1116,840\n112,696\n1193,575\n119,786\n290,159\n401,803\n515,662\n708,110\n1054,354\n1019,351\n1103,408\n1250,610\n979,430\n213,434\n574,395\n1153,859\n654,635\n671,526\n912,873\n664,236\n1126,697\n157,819\n1129,271\n460,46\n494,196\n604,849\n1131,206\n975,80\n1111,655\n798,473\n716,262\n493,644\n378,607\n1087,285\n490,526\n914,400\n485,444\n938,584\n735,812\n639,549\n408,724\n244,178\n934,166\n739,241\n325,700\n139,120\n1067,124\n950,681\n356,863\n639,345\n618,735\n1242,270\n848,302\n222,107\n663,142\n1302,658\n441,98\n850,495\n759,733\n386,516\n488,250\n892,691\n869,684\n1136,886\n1067,572\n671,318\n890,560\n82,53\n692,735\n1061,234\n1097,460\n922,275\n1006,679\n631,341\n100,424\n1032,166\n959,553\n1046,22\n422,64\n537,266\n340,830\n945,562\n190,357\n1206,516\n541,546\n656,78\n756,190\n146,411\n723,15\n1148,816\n602,744\n602,560\n1171,351\n262,859\n761,578\n410,206\n923,110\n223,285\n554,246\n420,425\n251,248\n1287,229\n90,651\n691,311\n879,161\n445,418\n32,9\n97,681\n1032,280\n782,551\n1228,53\n479,334\n869,98\n1221,264\n959,889\n57,120\n795,120\n435,452\n750,830\n529,674\n666,721\n996,152\n596,740\n1072,203\n1141,252\n937,200\n825,444\n192,262\n117,266\n969,10\n45,700\n202,634\n909,539\n930,291\n852,289\n1113,459\n447,668\n1174,145\n766,806\n234,493\n497,168\n725,479\n243,572\n36,308\n231,35\n653,700\n1155,742\n967,352\n90,567\n1210,256\n26,133\n164,430\n852,182\n550,835\n575,278\n1096,518\n408,618\n1126,645\n291,203\n843,547\n393,589\n251,646\n403,597\n956,873\n504,158\n438,730\n1042,611\n36,383\n622,233\n954,255\n686,336\n1072,89\n882,312\n1012,184\n1056,231\n1240,89\n639,368\n1185,226\n37,530\n1240,133\n1033,619\n1116,847\n624,336\n1227,770\n298,38\n418,243\n60,877\n817,112\n554,638\n1148,766\n495,94\n363,239\n171,518\n167,884\n192,632\n1227,124\n119,332\n1279,42\n1275,655\n387,385\n843,211\n145,271\n416,203\n1042,560\n560,275\n378,597\n199,239\n1242,723\n1200,54\n684,255\n649,270\n709,425\n234,137\n52,560\n36,159\n1298,264\n1201,420\n1143,570\n769,796\n773,19\n1181,128\n474,712\n535,184\n713,120\n1273,635\n267,385\n768,849\n848,592\n94,718\n1101,767\n1054,21\n156,819\n827,518\n1258,560\n243,770\n1043,497\n1278,9\n234,849\n962,262\n602,150\n720,35\n1054,522\n758,38\n1287,341\n1207,733\n1114,849\n254,572\n1114,493\n836,712\n951,751\n410,64\n1282,255\n818,414\n776,759\n813,392\n1198,824\n150,158\n850,757\n489,334\n1272,415\n23,344\n1043,833\n110,840\n104,656\n750,512\n766,730\n1116,137\n594,610\n358,534\n474,182\n408,276\n282,870\n172,749\n619,799\n418,19\n653,418\n795,792\n1034,119\n1191,786\n1255,228\n562,220\n410,688\n139,214\n806,630\n683,446\n57,827\n508,147\n1193,266\n766,520\n147,78\n186,835\n441,826\n756,638\n80,499\n1141,483\n1089,443\n932,597\n1044,569\n1185,28\n937,108\n915,687\n165,334\n954,863\n468,835\n246,499\n671,121\n89,264\n745,30\n542,824\n311,352\n395,207\n878,159\n981,252\n104,387\n623,303\n333,859\n972,469\n1225,155\n364,287\n1216,830\n668,264\n560,718\n902,276\n766,390\n528,551\n445,875\n606,54\n1242,171\n1305,668\n268,507\n1118,144\n1076,493\n112,198\n773,394\n403,259\n1203,228\n1148,78\n276,735\n22,378\n1029,694\n1124,835\n162,206\n169,483\n467,120\n900,830\n668,64\n49,367\n1037,308\n760,59\n736,742\n820,480\n411,770\n663,752\n500,425\n500,469\n104,159\n756,582\n103,147\n68,270\n873,698\n1146,318\n1310,240\n105,764\n642,64\n102,38\n1148,592\n890,21\n497,194\n145,308\n266,171\n873,411\n258,714\n870,719\n500,882\n1253,120\n261,81\n744,651\n1258,520\n542,70\n1298,885\n1115,518\n813,75\n852,163\n587,15\n460,399\n552,653\n89,712\n806,158\n88,651\n381,420\n726,47\n547,676\n925,67\n1205,92\n716,610\n281,786\n537,319\n1268,247\n1208,856\n497,726\n460,847\n181,271\n981,194\n1171,571\n592,200\n154,133\n594,505\n687,303\n1200,358\n1255,666\n159,772\n535,108\n453,476\n1170,112\n542,268\n281,694\n705,815\n117,866\n746,203\n1253,67\n1191,75\n119,75\n328,411\n850,46\n1088,607\n44,646\n682,761\n1231,726\n1200,838\n445,114\n176,158\n335,682\n129,290\n1265,194\n639,318\n1059,597\n1116,54\n45,259\n701,252\n644,873\n1037,831\n832,736\n738,393\n131,70\n1205,316\n239,259\n828,761\n529,259\n1086,241\n1179,824\n1011,245\n1295,325\n791,425\n282,24\n792,313\n879,285\n441,684\n1150,219\n140,283\n49,115\n256,873\n504,264\n668,824\n552,508\n858,260\n1042,287\n1146,32\n594,465\n431,285\n1266,646\n278,166\n1280,336\n447,226\n704,840\n438,806\n1298,233\n687,591\n1114,737\n294,885\n1179,70\n776,135\n459,518\n1144,219\n666,873\n1014,617\n60,389\n602,179\n1160,437\n1153,115\n915,207\n100,190\n1148,480\n920,263\n199,655\n10,245\n485,848\n1071,635\n290,271\n1054,746\n387,397\n177,682\n586,270\n658,336\n1163,654\n282,758\n1094,515\n94,830\n623,852\n38,522\n549,92\n564,327\n744,243\n1200,851\n495,800\n1240,668\n775,786\n1298,70\n1200,374\n835,121\n186,465\n415,892\n851,518\n564,203\n1056,663\n709,873\n837,623\n246,395\n756,424\n398,204\n587,187\n1266,310\n628,133\n924,334\n455,655\n398,873\n320,222\n1170,334\n83,124\n739,124\n959,540\n1220,679\n390,631\n544,88\n290,607\n102,632\n820,576\n278,280\n1310,187\n80,395\n105,443\n70,668\n1044,395\n251,382\n68,848\n267,61\n1111,332\n127,556\n1258,179\n1044,505\n738,282\n82,612\n934,614\n1054,204\n72,348\n254,663\n1043,397\n85,291\n877,155\n417,91\n460,137\n565,579\n564,567\n1041,550\n565,30\n1000,859\n1056,791\n688,233\n595,336\n1012,479\n140,334\n256,148\n1220,651\n542,178\n209,767\n534,759\n1287,547\n1213,681\n1076,849\n508,747\n273,831\n276,149\n256,298\n194,54\n872,88\n441,656\n214,175\n266,569\n865,875\n12,630\n1000,523\n1242,494\n873,196\n157,35\n420,469\n445,780\n551,733\n760,835\n1118,750\n551,161\n709,831\n448,311\n1275,319\n1057,192\n136,749\n628,761\n365,332\n1153,75\n846,117\n546,133\n1288,378\n900,512\n223,609\n350,159\n1287,344\n1103,668\n125,418\n440,719\n832,826\n36,586\n776,57\n1171,214\n480,456\n668,120\n646,12\n947,95\n378,159\n31,852\n1170,387\n535,786\n572,612\n619,583\n1086,101\n432,159\n560,619\n2,320\n1143,10\n53,847\n256,220\n894,805\n152,186\n136,817\n32,233\n1101,786\n579,288\n117,351\n1043,845\n731,707\n666,18\n1208,408\n32,885\n268,283\n929,698\n326,570\n590,35\n104,607\n654,436\n139,571\n619,655\n403,417\n1076,401\n1220,243\n990,672\n1308,574\n1109,707\n1032,614\n499,152\n\nfold along x=655\nfold along y=447\nfold along x=327\nfold along y=223\nfold along x=163\nfold along y=111\nfold along x=81\nfold along y=55\nfold along x=40\nfold along y=27\nfold along y=13\nfold along y=6")

(defn parse [input]
  (let [[coords folds] (str/split input #"\n\n")]
    {:coords (mapv (fn [row]
                     (let [[x y] (str/split row #",")]
                       {:x (Long/parseLong x)
                        :y (Long/parseLong y)}))
                   (str/split-lines coords))
     :folds  (mapv (fn [instr]
                     (let [[_, axis value] (re-matches #"fold along (x|y)\=([0-9]+)" instr)]
                       {(keyword axis) (Long/parseLong value)}))
                   (str/split-lines folds))}))

(defn ->matrix [coords]
  (let [maxX (inc (apply max (map :x coords)))
        maxY (inc (apply max (map :y coords)))]
    (reduce (fn [m {:keys [x y]}]
              (assoc-in m [y x] 1))
            (vec (repeat maxY (vec (repeat maxX 0))))
            coords)))

(defn render [m]
  (->> (mapv (fn [row] (mapv #(if (= % 0) \. \#) row)) m)
       (mapv #(apply str %))
       (run! println)))

(defn rotate [m]
  (apply mapv vector m))

(defn fold [matrix {:keys [x y] :as fold-action}]
 (cond
   (some? x)
   (rotate (fold (rotate matrix) {:y x}))

   (some? y)
   (let [[one two] (split-at y matrix)
         one (vec (reverse one))
         two (vec (rest two))]
     (->> (map-indexed vector two)
          (reduce (fn [m [idx row]]
                    (update m idx #(mapv + % row)))
                  one)
          (reverse)
          (vec)))))

(defn solve-1 [input]
  (let [{:keys [coords folds]} (parse input)]
    (->> (reduce fold (->matrix coords) (take 1 folds))
         (flatten)
         (remove zero?)
         (count))))

(defn solve-2 [input]
  (let [{:keys [coords folds]} (parse input)]
    (render (reduce fold (->matrix coords) folds))))

(comment
 (parse example-input)

 (def m (->matrix (:coords (parse example-input))))

 (render m)

 ; #####
 ; #...#
 ; #...#
 ; #...#
 ; #####
 ; .....
 ; .....
 (render (fold (fold m {:y 7}) {:x 5}))

 ;; 17
 (solve-1 example-input)

 ;; 781
 (solve-1 puzzle-input)

 ;; ###..####.###...##...##....##.###..###..
 ;; #..#.#....#..#.#..#.#..#....#.#..#.#..#.
 ;; #..#.###..#..#.#....#.......#.#..#.###..
 ;; ###..#....###..#....#.##....#.###..#..#.
 ;; #....#....#.#..#..#.#..#.#..#.#....#..#.
 ;; #....####.#..#..##...###..##..#....###..
 (solve-2 puzzle-input))