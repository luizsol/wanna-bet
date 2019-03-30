(ns wanna-bet.db
  (:require [taoensso.carmine :as car :refer (wcar)]
            [digest :as d]
            [wanna-bet.util :as util]
            [clj-time.core :as t]
            [clj-time.local :as l]))

;; -------------------------
;; DB Configuration

(def server1-conn {:pool {} 
                   :spec {:host "127.0.0.1" :port 6379 :db 3}})

;; -------------------------
;; Basic DB functions

(defmacro wcar* [& body] `(car/wcar server1-conn ~@body))

(defn db-get [key] (wcar* (car/get key)))

(defn db-set! [key value] (wcar* (car/set key value)))

(defn id-to-keywork 
  [prefix] 
  (fn [id] (keyword (str prefix "-" id))))

(def max-big-int-id 2147483647)

(defn get-random-keyword 
  [id-to-type-keyword-fn]
  (loop [id (rand-int max-big-int-id)]
    (let [type-keyword (id-to-type-keyword-fn id)]
      (if (nil? (db-get type-keyword))
        [id type-keyword]
        (recur (rand-int max-big-int-id))))))

;; -------------------------
;; User

(def user-id-to-keyword (id-to-keywork "user"))

(def hash-salt (or (System/getenv "WANNA_BET_HASH_SALT")
                   "dlvmpd-=b)m_c1h0y69!i1!xgo=c)m2)plr+6huk+m9tf_py!0"))

(defn salt [s] (apply str (concat (map #(apply str (interpose % s)) hash-salt))))

(defn hash-and-salt [s] (d/sha-256 (salt s)))

(defn new-user-record 
  [id keyword name email passwd]
  {:id id
   :validated false
   :active false
   :keyword keyword
   :name name
   :email email
   :hash (hash-and-salt passwd)
   :created-at (l/local-now)
   :updated-at (l/local-now)
   :wallet 0
   :transactions []
   :portfolio {}
   :orders []})

(defn update-user-name-and-email-catalog 
  [name email user-keyword] 
  (let [records (db-get "user-name-email-registry")
        name name
        email email]
    (if (nil? records)
      (do (db-set! "user-name-email-registry" {name user-keyword
                                               email user-keyword})
          true)
      (if (and (nil? (get records name)) (nil? (get records email)))
        (let [updated (into records {name user-keyword
                                     email user-keyword})]
          (do (db-set! "user-name-email-registry" updated)
              true))
        false))))

(defn create-user!
  [name email passwd]
  (if (and (util/validate-user name) (util/validate-email email))
    (let [[id user-keyword] (get-random-keyword user-id-to-keyword)
          record (new-user-record id user-keyword name email passwd)]
      (if (and (update-user-name-and-email-catalog name email user-keyword)
               (db-set! user-keyword record))
        record
        nil))))
