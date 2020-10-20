;;derived from https://gist.githubusercontent.com/lnostdal/1fbd9b3d2ddc7bff1830638ea88348cc/raw/4b8a41782ee1ee36957e1ef6c37b375ce43d2a11/fast_local_mutation.clj

;;with optimizations/ideas.
(ns mutabletest.core
  (:require [criterium.core :as c]
            [primitive-math :as p]
            [proteus :as prot]))

(set! *warn-on-reflection* true)


;;baseline:
(defn simple-test []
  (c/quick-bench (inc 0)))
;;Execution time mean : 5.311472 ns


(defprotocol IOObject
  (setVal [this new-val])
  (getVal [this])
  (oswap [this f]
         [this f x]
         [this f x y]
         [this f x y z]))

(deftype OObject
    [^:unsynchronized-mutable val]

  IOObject
  (setVal [o new-val] (set! val new-val) o)
  (getVal [o] val)
  (oswap [o f] (set! val (f val)) o)
  (oswap [o f x] (set! val (f val x)) o)
  (oswap [o f x y] (set! val (f val x y)) o)
  (oswap [o f x y z] (set! val (f val x y z)) o)

  clojure.lang.IDeref
  (deref [o] val)
  clojure.lang.IAtom
  (swap [this f] (.oswap this f)))


(defrecord counter [^long n]
  IOObject
  (setVal [o new-val] (counter. new-val))
  (getVal [o] n)
  (oswap [o f] (counter. (f n)))
  (oswap [o f x] (counter. (f n x)))
  (oswap [o f x y] (counter. (f n x y)))
  (oswap [o f x y z] (counter. (f n  x y z)))
  clojure.lang.IFn
  (invoke [this k] (when (identical? k :n) n)))

(deftype arraycell [^objects box]
  clojure.lang.IDeref
  (deref [this] (aget box 0) this)
  clojure.lang.IAtom
  (reset [this x] (aset box 0 x) this)
  (swap [this f]  (aset box 0 (f (aget box 0))) this))

(deftype cell [^{:unsynchronized-mutable true} contents]
  clojure.lang.IDeref
  (deref [this] contents)
  clojure.lang.IAtom
  (reset [this x] (set! contents x) this)
  (swap [this f]  (set! contents (f contents)) this))

(defn ref-test []
  (let [a (ref 0)]
    (c/quick-bench
     (dosync
      (alter a (fn [^long val] (inc val)))))))
;;Execution time mean : 5.209393 µs

(defn ref-test-commute []
  (let [a (ref 0)]
    (c/quick-bench
     (dosync
      (commute a (fn [^long val] (inc val)))))))
;;Execution time mean : 5.593799 µs

(defn agent-test []
  (let [a (agent 0)]
    (c/quick-bench
     (await (send a (fn [^long val] (inc val)))))))
;;Execution time mean : 42.521203 µs

(defn wlv-test []
  (with-local-vars [wlv 0]
    (c/quick-bench
     (var-set wlv (inc ^long (var-get wlv))))))
;;Execution time mean : 284.557776 ns

(defn wlv-test-unchecked []
  (with-local-vars [wlv 0]
    (c/quick-bench
     (.set wlv (unchecked-inc (.get wlv))))))

;; Execution time mean : 274.098816 ns

(defn wlv-test-primitive []
  (with-local-vars [wlv 0]
    (c/quick-bench
     (.set wlv (p/inc ^long (.get wlv))))))
;; Execution time mean : 252.544390 ns

(defn atom-test []
  (let [a (atom 0)]
    (c/quick-bench 
       (swap! a (fn [^long val] (inc val))))))
;;Execution time mean : 42.648446 ns

(defn atom-test-unchecked []
  (let [a (atom 0)]
    (c/quick-bench
     (swap! a (fn [^long val] (unchecked-inc val))))))
;;Execution time mean : 44.841221 ns ;;strange.

(defn atom-test-prim []
  (let [a (atom 0)]
    (c/quick-bench
     (swap! a (fn [^long val] (p/inc val))))))
;;Execution time mean : 42.178471 ns

(defn atom-test-direct []
  (let [^clojure.lang.IAtom a (atom 0)]
    (c/quick-bench
     (.swap a (fn [^long val] (p/inc val))))))
;;Execution time mean : 39.995364 ns

(defn vol-test []
  (let [v (volatile! 0)]
    (c/quick-bench
     (vswap! v (fn [^long val] (inc val))))))
;;Execution time mean : 22.704557 ns

(defn vol-test-unchecked []
  (let [v (volatile! 0)]
    (c/quick-bench
     (vswap! v (fn [^long val] (unchecked-inc val))))))
;; Execution time mean : 22.573719 ns

(defn vol-test-prim []
  (let [v (volatile! 0)]
    (c/quick-bench
     (vswap! v (fn [^long val] (p/inc val))))))
;; Execution time mean : 22.939165 ns

;;no need for direct, since vswap! is already a type-hinted
;;macro expansion.

(defn type-test []
  (let [o (OObject. 0)]
    (c/quick-bench
     (.oswap o (fn [^long val] (inc val))))))
;;Execution time mean : 18.560995 ns

(defn type-test-unchecked []
  (let [o (OObject. 0)]
    (c/quick-bench
     (.oswap o (fn [^long val] (unchecked-inc val))))))
;;Execution time mean : 17.574168 ns

(defn type-test-prim []
  (let [o (OObject. 0)]
    (c/quick-bench
     (.oswap o (fn [^long val] (p/inc val))))))
;; Execution time mean : 17.708783 ns

(defn record-test []
  (let [c (->counter 0)]
    (c/quick-bench (->counter (inc (c :n))))))
;;Execution time mean : 45.013730 ns

(defn record-test-dm []
  (let [^counter c (->counter 0)]
    (c/quick-bench (counter. (inc (.n c))))))
;;Execution time mean : 12.756774 ns

(defn record-test-prim []
  (let [^counter c (->counter 0)]
    (c/quick-bench (counter. (p/inc (.n c))))))
;;Execution time mean : 11.895746 ns

(defn record-test-protocol []
  (let [^counter c (->counter 0)]
    (c/quick-bench (oswap c inc))))
;;Execution time mean : 49.945048 ns

(defn record-test-protocol-prim []
  (let [^counter c (->counter 0)]
    (c/quick-bench (oswap c #(p/inc ^long %)))))
;;  Execution time mean : 43.647200 ns

(defn record-test-protocol-set []
  (let [^counter c (->counter 0)]
    (c/quick-bench (setVal c (p/inc ^long (getVal c))))))
;;  Execution time mean : 44.326989 ns

(defn arr-test []
  (let [x (object-array [0])]
    (c/quick-bench
     (aset x 0 (inc (aget x 0))))))
;; Execution time mean : 24.414424 ns
(defn arr-test-unchecked []
  (let [x (object-array [0])]
    (c/quick-bench
     (aset x 0 (unchecked-inc  (aget x 0))))))
;;Execution time mean : 24.449794 ns

;;sometimes inline aset/aget are not happy for profiling, since
;;they obfuscate hotspot.  They seem to like to be in methods..
;;let's wrap them in an object an see if we're faster.
;;[These are both faster]
(defn arrcell-test []
  (let [^arraycell x (arraycell. (object-array [0]))]
    (c/quick-bench
     (.swap x #(inc %)))))

(defn arrcell-test-unchecked []
  (let [^arraycell x (arraycell. (object-array [0]))]
    (c/quick-bench
     (.swap x #(unchecked-inc ^long %)))))

;;Just smashing on a long in an array should be getting
;;towards some bounds...
(defn long-test []
  (let [x (long-array [0])]
    (c/quick-bench
     (aset x 0 (inc (aget x 0))))))
;;Execution time mean : 15.759055 ns
(defn long-test-unchecked []
  (let [x (long-array [0])]
    (c/quick-bench
     (aset x 0 (unchecked-inc (aget x 0))))))
;;Execution time mean : 15.183619 ns
(defn long-test-prim []
  (let [x (long-array [0])]
    (c/quick-bench
     (aset x 0 (p/inc (aget x 0))))))
;;Execution time mean : 15.509367 ns

;;proteus lets us use local mutable primitives via java interop
;;and macros.  The semantics are a bit less straightforward
;;for use with criterium, since we have to use a "local" function
;;with the bench call, we can't just invoke set! from quick-bench
;;as is.
(defn prot-test []
  (prot/let-mutable [x 0]
                    (let [f ^:local (fn [] (set! x (inc x)))]
                      (c/quick-bench (f)))))
;;Execution time mean : 8.343787 ns
(defn prot-test-unchecked []
  (prot/let-mutable [x 0]
                    (let [f ^:local (fn [] (set! x (unchecked-inc x)))]
                      (c/quick-bench (f)))))
;;Execution time mean : 8.091041 ns

(defn prot-test-prim []
  (prot/let-mutable [x 0]
                    (let [f ^:local (fn [] (set! x (p/inc x)))]
                      (c/quick-bench (f)))))
;;Execution time mean : 7.519785 ns

