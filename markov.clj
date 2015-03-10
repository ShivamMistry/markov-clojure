(require '[clojure.string :as str])

(defn read-log [file]
  (let [matcher (map (partial re-matcher #".+?<(.+?)>\s+?(.+)") (str/split-lines (slurp file)))] 
    (map (comp rest re-groups) (filter #(.matches %) matcher)))
  )

(defn parse-line [text]
  (let [x (str/split (.replaceAll (str/lower-case text) "[^0-9a-z ]" "") #" ")]
    (partition 2 1 x))
  )

(defn merge-maps [entries]
  (zipmap 
    (keys entries)
    (map #(apply merge-with 
                 (fn [res latter] (conj (if (seq? (first res)) res (list res)) latter)) %) 
         (vals entries)))
  )

(defn make-ngrams [entries] 
  (merge-maps 
    (zipmap 
      (keys entries) 
      (map 
        (fn [a] (map (fn [x] (assoc nil (first x) (rest x))) a)) 
        (vals entries))))
  )

(defn parse-logs [entries]
  (make-ngrams 
    (apply merge-with into 
           (map #(assoc nil (first %) (parse-line (first (rest %)))) entries)))
  )

(defn generate-sentence[ngrams first-word]
  (if (nil? first-word) nil
    (let [x (get ngrams first-word)]
      (let [word (rand-nth x)]
        (cons first-word (generate-sentence ngrams (if (seq? word) (first word) word)))
        )
      )
    )
  )

(defn get-user [user m]
  (let [col (get m user)] (generate-sentence col (rand-nth (keys col))))
  )

(println (get-user (second *command-line-args*) (parse-logs (read-log (first *command-line-args*)))))
