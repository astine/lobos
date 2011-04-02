(ns lobos.migrations
  (:use clojure.pprint
	clojure.java.io
	clojure.walk
	(clojure.contrib [def :only [name-with-attributes]]))
  (:import [java.util Date]
	   [java.io PushbackReader]))

;;; A simplistic implementation of migrations.
;;; There is no automatic generation of migrations yet in this
;;; implementation.

;; -----------------------------------------------------------------------------

;; ## Migration Type

(defrecord Migration [do undo timestamp done?] java.lang.Comparable
  (compareTo [this other]
	     (cond (== timestamp (:timestamp other))
		   (if (not (= done? (:done? other)))
		     (throw (Exception. (str "Migrations out of order: " this "; " other)))
		     0)
		   (> timestamp (:timestamp other))
		   (if (and done? (not (:done? other)))
		     (throw (Exception. (str "Migrations out of order: " this "; " other)))
		     -1)
		   (< timestamp (:timestamp other))
		   (if (and (not done?) (:done? other))
		     (throw (Exception. (str "Migrations out of order: " this "; " other)))
		     1)))
  (toString [this]
	    (str "Migration:{ do:" do "; undo:" undo "; timestamp:" timestamp "; done?:" done? "}")))

;; -----------------------------------------------------------------------------

;; ## Migrations Stack and Stack Operations

;; Migrations are stored in a stack with older migrations near the bottom and
;; newer ones near the top.
(defonce
  ^{:doc "Migrations Stack: The stack is implement with a list in an atom. *For internal use*"}
  migrations
  (atom '()))

(defmacro alter-migrations
  "Wrapper around swap! migrations idiom.
   Forms are evaluated with `migrations` bound to the value of the migations stack
   and the value returned by the last form becomes the new value of the stack.
   *For internal use*"
  [& forms]
  `(swap! migrations
	  (fn [~'migrations]
	    ~@forms)))

(defn add-migration
  "Creates a migration object and adds it to the migration stack."
  [do undo & [timestamp done?]]
  (let [migration (Migration. do
			      undo
			      (or timestamp (.getTime (Date.)))
			      (or done? false))]
    (alter-migrations
     (sort
      (cons migration
	    migrations)))
    migration))

(defmacro defmigration
  "Slightly cleaner wrapper around `add-migration`"
  [do undo & [timestamp done?]]
  `(add-migration (quote ~do) (quote ~undo) ~timestamp ~done?))

(defn- remove-migration
  "Remove a migration from the stack. This is a stack manipulation function;
   it does not rollback any migrations. *For internal use*"
  []
  (alter-migrations
   (rest migrations)))

(defn- clear-migrations
  "Clear migrations stack. *For interal use*"
  []
  (alter-migrations
   '()))
    
;; -----------------------------------------------------------------------------

;; ## Save and Load migrations

;; One of the advantages to a migrations system is that it allows you to store
;; your schema and an entire edit and undo history of that schema in version
;; control. Of course, in order to do this, the migrations must be saved to a
;; file(s).

;; The idea here, is that a user should be able to define migrations from the
;; REPL or through an automatic function and have them seriallized to a file
;; automatically. In addition, a person should be able to add and edit 
;; migrations in that file manually.

;; The chief function here is the `with-migrations` macro which manages loading
;; and saving of migrations automatically.

(defonce ^{:docs "Primary file migrations are to be stored in."}
  migrations-file
  "migrations.clj")

(defn save-migrations
  "Saves the migration stack a file which can be loaded again, or editted by a person.
   Migrations are saved as `defmigration` forms."
  [& [file]]
  (with-open [out (writer (or file migrations-file))]
    (.write out "; This file generated automatically.\n")
    (doseq [migration @migrations]
      (.write out "\n")
      (pprint `(defmigration
		 ~(:do migration)
		 ~(:undo migration)
		 ~(:timestamp migration)
		 ~(:done? migration))
	      out))))

(defn dump-migrations
  [& [file]]
  (save-migrations file)
  (clear-migrations))

(defn load-migrations
  "Perform a clean load of migrations from a file."
  [& [file]]
  (clear-migrations)
  (load-file (or file migrations-file)))

(defn load-additional-migrations
  "Load migration from a file into the current stack."
  [file]
  (load-file (or file migrations-file)))

(defmacro with-migrations
  "Execute statements in the context of a certain migration file; effectively tieing
   the migration stack to the file."
  [[file & other-files] & body]
  `(do (load-migrations ~(or file migrations-file))
       ~@(map #(list 'load-additional-migrations %)
	      other-files)
       ~@body
       (dump-migrations ~(or file migrations-file))))

;; -----------------------------------------------------------------------------

;; ## Running and Rolling Back Migrations

(defn delete-migration
  "Removes a migration from the stack, rolling it back if necessary."
  []
  (let [migration (first @migrations)]
    (if (:done? migration)
      ((:undo migration))))
  (remove-migration))

(defn run-migration
  "Runs the most recent migration not already completed."
  []
  (if-not (or (empty? @migrations)
	      (every? :done? @migrations))
    (alter-migrations
     (let [tail (drop-while #(not (:done? %)) migrations)
	   head (take-while #(not (:done? %)) migrations)
	   target-migration (last head)
	   head (butlast head)]
       (eval (:do target-migration))
       (concat head
	       (list (assoc target-migration :done? true))
	       tail)))))

(defn rollback-migration
  "Rolls back the most recently completed migration."
  []
  (if-not (or (empty? @migrations)
	      (not-any? :done? @migrations))
    (alter-migrations
     (let [tail (drop-while #(not (:done? %)) migrations)
	   head (take-while #(not (:done? %)) migrations)
	   target-migration (first tail)
	   tail (rest tail)]
       (eval (:undo target-migration))
       (concat head
	       (list (assoc target-migration :done? false))
	       tail)))))

(defn run-all-migrations                ; this is an inefficient way to do this, but without
					; transactions, there isn't a simple better way
  "Runs all migrations in the stack not yet completed."
  []
  (if-not (empty? @migrations)
    (while (not (every? :done? @migrations))
      (run-migration))))
 
;(defn run-all-migrations
  ;"Runs all migrations in the stack not run."
  ;[]
  ;(if-not (or (empty? @migrations)
	      ;(every? done? @migrations))
    ;(alter-migrations
     ;(let [tail (drop-while (not (:done %)) migrations)
	   ;head (take-while (not (:done %)) migrations)]
       ;(doseq [migration (reverse head)]
	 ;(:do migration))
       ;(concat (map #(assoc % :done? true)
		    ;head)
	       ;tail)))))
	   
