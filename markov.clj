(require '[clojure.string :as str])

(def n-g 2)

(defn read-log [file]
  (let [matcher (map (partial re-matcher #".*?<\s*(.+?)>\s+?(.+)") (str/split-lines (slurp file)))] 
    (map (comp rest re-groups) (filter #(.matches %) matcher)))
  )

(defn parse-line [text]
  (let [x (str/split (.replaceAll (str/lower-case text) "[^0-9a-z ]" "") #" ")]
    (partition (inc n-g) 1 x))
  )

(defn merge-maps [entries]
  (zipmap 
    (keys entries)
    (map #(apply merge-with into %)  (vals entries)))
  )

(defn make-ngrams [entries] 
  (merge-maps 
    (zipmap 
      (keys entries) 
      (map 
        (fn [a] (map (fn [x] (assoc nil (butlast x) (list (last x)))) a)) 
        (vals entries))))
  )

(defn parse-logs [entries]
  (make-ngrams 
    (apply merge-with into 
           (map #(assoc nil (first %) (parse-line (first (rest %)))) entries)))
  )

(defn generate-sentence[ngrams head]
  (let [word (rand-nth (get ngrams head)) l (vec head)]
    (if (nil? word) nil
      (let [chain (generate-sentence ngrams (conj (vec (nthrest l 1)) word))]
        (conj (vec (take 1 head)) (if (nil? chain) (conj nil word (drop 1 l)) chain))))
    )
  )

(defn get-user 
  ([user m] (flatten (let [col (get m user)] (generate-sentence col (rand-nth (keys col))))))
  ([user m first-word] (flatten (generate-sentence (get m user) first-word)))
  )
;
(println (get-user (second *command-line-args*) (parse-logs (read-log (first *command-line-args*)))))
