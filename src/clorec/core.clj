(ns clorec.core
  (:require [clojure.string :as s]))

(defn ^:private keywordize [k]
  (-> k
      (s/replace #"([a-z])([A-Z])" "$1-$2")
      (s/replace "_" "-")
      (s/lower-case)
      (keyword)))

(defn ^:private parse-record-line [line]
  (let [parts (s/split line #": ?" 2)]
    (if (and (= (count parts) 2) (seq (first parts)))
      parts
      (throw (ex-info "bad record line" {:line line})))))

(defn ^:private parse-continuation-line [line]
  (if (.startsWith line "+ ")
    (.substring line 2)
    (.substring line 1)))

(defn ^:private in-record? [state]
  (= (:id state) :in-record))

(defn ^:private finish-record [state]
  (assoc state
         :id :default
         :record {}))

(defn ^:private record-add-kv [state [raw-key raw-value]]
  (let [k (keywordize raw-key)
        v raw-value
        record (update (:record state) k #(conj (or % []) v))]
    (assoc state
         :id :in-record
         :last-key k
         :record record)))

(defn ^:private append-line [s1 s2]
  (if (or (s/blank? s1) (.endsWith s1 "\n"))
    (str s1 s2 \newline)
    (str s1 \newline s2 \newline)))

(defn ^:private record-add-multi-line [state line]
  (let [k (:last-key state)
        index (dec (count (get-in state [:record k])))]
    (update-in state [:record k index] append-line line)))

(defn parse-lines
  "Parses a collection of .rec lines and returns a lazy sequence of records."
  ([input-lines]
   (parse-lines input-lines {:id :default
                             :record {}}))
  ([input-lines input-state]
   (lazy-seq
     (loop [state input-state
            lines input-lines]
       (if-let [line (first lines)]
         (cond
           (.startsWith line "%") (throw (ex-info "% not implemented" {}))
           (.startsWith line "#") (recur state (rest lines))
           (.startsWith line "+") (if (in-record? state)
                                    (parse-lines
                                      (rest lines)
                                      (record-add-multi-line state (parse-continuation-line line)))
                                    (throw (ex-info "stray multi line statement" {})))
           (s/blank? line) (if (in-record? state)
                             (cons
                               (:record state)
                               (parse-lines (rest lines) (finish-record state)))
                             (parse-lines (rest lines) state))
           :else (parse-lines (rest lines) (record-add-kv state (parse-record-line line))))
         (when (seq (:record state))
           (cons (:record state) nil)))))))

(defn parse-file
  "Parses a .rec file and returns a realized sequence of records.
  Accepts whatever clojure.java.io/reader accepts."
  [x]
  (with-open [r (clojure.java.io/reader x)]
    (doall (parse-lines (line-seq r)))))
