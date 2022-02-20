(ns clorec.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [clorec.core :refer [parse-file]]))

(defn fixtures [path]
  (for [file (.listFiles (clojure.java.io/file path))
        :let [file-name (.getAbsolutePath file)]
        :when (.endsWith file-name ".rec")]
    [file-name (str (subs file-name 0 (- (count file-name) 3)) "edn")]))

(deftest ok-fixtures
  (doseq [[rec edn] (fixtures "test/clorec/data/ok")]
    (testing (str "file " rec " parses correctly")
      (let [expected (read-string (slurp edn))]
        (is (= expected (parse-file rec)))))))

(deftest fail-fixtures
  (doseq [[rec _] (fixtures "test/clorec/data/fail")]
    (testing (str "file " rec " fails parsing")
      (is (thrown? clojure.lang.ExceptionInfo (parse-file rec))))))
