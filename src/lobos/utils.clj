;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.utils
  "Helpers used in unrelated namespaces."
  (:refer-clojure :exclude [defonce replace])
  (:use (clojure [string :only [lower-case
                                replace
                                upper-case]]
                 [walk :only [postwalk]])))

(defn join [separator & coll]
  (clojure.string/join separator (filter identity coll)))

(defn as-str ; taken from clojure.contrib.string
  "Like clojure.core/str, but if an argument is a keyword or symbol,
  its name will be used instead of its literal representation."
  ([] "")
  ([x] (if (instance? clojure.lang.Named x)
         (name x)
         (str x)))
  ([x & ys]
     ((fn [^StringBuilder sb more]
        (if more
          (recur (. sb (append (as-str (first more)))) (next more))
          (str sb)))
      (new StringBuilder ^String (as-str x)) ys)))

(defn as-list
  "Returns the given collection parenthesized string with its items
  separated by commas. Apply as-str to coll items."
  [coll]
  (when (not-empty coll)
    (format "(%s)" (apply join ", " (map as-str coll)))))

(defn as-sql-keyword
  "Returns the given string, symbol or keyword as an upper-cased string
  and replace dashes with spaces."
  [s]
  (when s (replace (-> s as-str upper-case) \- \space)))

(defn as-keyword [s]
  (-> s lower-case (replace #"[_ ]" "-") keyword))

(defn make-constraint-name [table ctype columns]
  (keyword
   (replace (apply join "_" (conj (map name columns)
                                  (name ctype)
                                  (name (if (keyword? table)
                                          table
                                          (:name table)))))
             \- \_)))

(defn optional [pred? args]
  (if (pred? (first args))
    [(first args) (next args)]
    [nil args]))

(defn conj-when
  "Like conj but if test is false returns coll untouched."
  [coll test x & xs]
  (if test
    (apply conj coll x xs)
    coll))

(defn capture-keywords
  "Returns a set containing all keywords found in the given form as
  strings."
  [form]
  (let [acc (atom #{})
        f #(if (keyword? %2) (conj %1 (name %2)) %1)]
    (postwalk #(swap! acc f %) form)
    @acc))

(defmacro defonce
  "Same as defonce but with an optional docstring."
  ([name expr]
     (list 'clojure.core/defonce
           name
           expr))
  ([name expr doc]
     (list 'clojure.core/defonce
           (with-meta name (assoc (meta name) :doc doc))
           expr)))
