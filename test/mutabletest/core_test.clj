(ns mutabletest.core-test
  (:require [clojure.test :refer :all]
            [mutabletest.core :refer :all]
            [clojure.string]))

(defn clean-mean [x]
  (->> (re-seq #".+Execution time mean : .*" x)
       first
       clojure.string/trim))

(defn collect-sample [test-name f]
  (let [res (with-out-str (f))]
    {:test (name test-name) :res (clean-mean res)}))

(defn all-tests []
  (let [targets (->> (ns-publics 'mutabletest.core)
                     (filter (fn [[s f]]
                               (clojure.string/includes? (name s) "test")))
                     (sort-by (fn [[s f]] (-> f meta :line))))
        bound (count targets)
        _ (println [:Running bound :performance-tests])]
  (doseq [[n [s f]] (map-indexed vector targets)]
    (println (assoc (collect-sample s f) :n n)))))

(all-tests)
