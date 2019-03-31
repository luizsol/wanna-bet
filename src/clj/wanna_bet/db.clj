(ns wanna-bet.db
  (:require [taoensso.carmine :as car :refer (wcar)]
            [digest :as d]
            [wanna-bet.util :as util]
            [clj-time.core :as t]
            [clj-time.local :as l]))

;; -------------------------
;; DB Configuration

(def conn-pool {:pool {}
                :spec {:host (or (System/getenv "WANNA_BET_REDIS_HOST")
                                 "localhost")
                       :port (or (System/getenv "WANNA_BET_REDIS_PORT")
                                 6379)
                       :db (or (System/getenv "WANNA_BET_REDIS_DB")
                               3)}})

;; -------------------------
;; Basic DB functions

(defmacro wcar* [& body] `(car/wcar conn-pool ~@body))

(defn db-get [key] (wcar* (car/get key)))

(defn db-set! [key value] (wcar* (car/set key value)))

;; -------------------------
;; Basic entity functions

(def max-int 2147483647)

(defn random-entity-id
  "Generates a function that generates a valid random id for a given entity"
  [entity-type]
  (fn [] (loop [id (rand-int max-int)]
           (if (empty? (wcar* (car/keys (car/key entity-type id "*"))))
             id
             (recur (rand-int max-int))))))

(defn set-entity-field!
  "Generates a function that upserts a field of a given entity"
  [entity]
  (fn [id field value] (db-set! (car/key entity id field) value)))

(defn get-entity-field
  "Generates a function that fetches a field of a given entity"
  [entity]
  (fn [id field] (db-get (car/key entity id field))))

(defn default-entity-to-list
  "Converts a default entity's map to a key value list"
  [entity]
  (apply concat (seq entity)))

(defn field-to-redis-key
  "Creates a function that converts a field to the redis form entity:id:field"
  [entity]
  (fn [id field] (car/key entity id field)))

;; -------------------------
;; User

(def user-field-to-redis-key (field-to-redis-key "user"))

(defn user-fields-to-redis-keys 
  "Maps each key to a Redis dialect key such as user:id:field"
  [id user-keys] 
  (map #(user-field-to-redis-key id %) user-keys))

(def hash-salt (or (System/getenv "WANNA_BET_HASH_SALT")
                   "dlvmpd-=b)m_c1h0y69!i1!xgo=c)m2)plr+6huk+m9tf_py!0"))

(defn default-user-map
  "Generates an user default map"
  [] 
  {:validated false
   :active false
   :name nil
   :admin false
   :email nil
   :hash nil
   :created-at (l/local-now)
   :updated-at (l/local-now)
   :wallet 0
   :transactions []
   :trades []
   :orders []})

(def random-user-id (random-entity-id "user"))

(defn update-user-field! [id field value] (set-entity-field! "user" id field value))

(defn crete-user-key-value-list
  "Creates a mset ready key value list for a new user"
  [id name email passwd]
  (let [merged-map (into (default-user-map)
                         {:name name
                          :email email
                          :hash (hash-and-salt passwd)})]
    (default-entity-to-list (zipmap (user-fields-to-redis-keys id (keys merged-map))
                                    (vals merged-map)))))

(defn salt
  "Adds the salt string to a password"
  [s]
  (apply str (concat (map #(apply str (interpose % s)) hash-salt))))

(defn hash-and-salt
  "Salts and hashes a password"
  [s]
  (d/sha-256 (salt s)))

(defn get-all-user-emails
  "Retrieves a list of all users' emails"
  []
  (let [emails-keys (wcar* (car/keys (car/key :user "*" :email)))]
    (if (> (count emails-keys) 0)
      (wcar* (apply car/mget emails-keys))
      [])))

(defn get-all-user-names
  "Retrieves a list of all users' names"
  []
  (let [names-keys (wcar* (car/keys (car/key :user "*" :name)))]
    (if (> (count names-keys) 0)
      (wcar* (apply car/mget names-keys))
      [])))

(defn user-name-exists? [name] (.contains (get-all-user-names) name))

(defn user-email-exists? [email] (.contains (get-all-user-emails) email))

(defn create-user!
  "Creates and store new user if it's name and email are valid and unique"
  [name email passwd]
  (if (and (util/validate-user name)
           (util/validate-email email)
           (not (user-name-exists? name))
           (not (user-email-exists? email)))
    (let [id (random-user-id)]
      (do (wcar* (apply car/mset (crete-user-key-value-list id name email passwd)))
          id))))

(defn get-user-data
  "Retrieves an user's data as a map if it exists"
  [id]
  (into {:id id} 
        (let [user-keys (keys (default-user-map))]
          (zipmap user-keys 
                  (wcar* (apply car/mget (user-fields-to-redis-keys id user-keys)))))))

(defn get-user-id-by-name
  "Retrieves an user's id if it exists searching by its name"
  [name]
  (let [user-name-keys (wcar* (car/keys (car/key :user "*" :name)))]
    (if-not (empty? user-name-keys)
      (let [user-name-values (wcar* (apply car/mget user-name-keys))
            user-name-maps (zipmap user-name-values user-name-keys)
            filtered-user-name-maps (second (first (filter 
                                                    #(= name (first %)) 
                                                    user-name-maps)))]
        (if-not (nil? filtered-user-name-maps)
          (second (re-find #"user:(.*?):name" filtered-user-name-maps)))))))

(defn get-user-id-by-email
  "Retrieves an user's id if it exists searching by its email"
  [email]
  (let [user-email-keys (wcar* (car/keys (car/key :user "*" :email)))]
    (if-not (empty? user-email-keys)
      (let [user-email-values (wcar* (apply car/mget user-email-keys))
            user-email-maps (zipmap user-email-values user-email-keys)
            filtered-user-email-maps (second (first (filter 
                                                     #(= email (first %)) 
                                                     user-email-maps)))]
        (if-not (nil? filtered-user-email-maps)
          (second (re-find #"user:(.*?):email" filtered-user-email-maps)))))))

;; -------------------------
;; Bets

(def bet-field-to-redis-key (field-to-redis-key "bet"))

(defn bet-fields-to-redis-keys
  "Maps each key to a Redis dialect key such as bet:id:field"
  [id bet-keys]
  (map #(bet-field-to-redis-key id %) bet-keys))

(def random-ticker (util/rand-str 6))

(def random-bet-id (random-entity-id "bet"))

(defn default-bet-map
  "Generates an bet default map"
  []
  {:active true
   :ticker nil
   :creator nil
   :expiration nil
   :result nil
   :created-at (l/local-now)
   :updated-at (l/local-now)
   :contract-value nil
   :buy-orders []
   :sell-orders []
   :trades []})

(defn crete-bet-key-value-list
  "Creates a mset ready key value list for a new bet"
  ([id description creator expiration]
   (crete-bet-key-value-list id (random-ticker) description creator 100 expiration))
  ([id description creator contract-value expiration] 
   (crete-bet-key-value-list id (random-ticker) description creator contract-value expiration))
  ([id ticker description creator contract-value expiration]
   (let [merged-map (into (default-bet-map)
                          {:ticker ticker
                           :description description
                           :creator creator
                           :contract-value contract-value
                           :expiration expiration})]
     (default-entity-to-list (zipmap (bet-fields-to-redis-keys id (keys merged-map))
                                     (vals merged-map))))))

(defn create-bet!
  "Creates and store new bet"
  ([description creator expiration]
   (create-bet! (random-ticker) description creator 100 expiration))
  ([description creator contract-value expiration] 
   (create-bet! (random-ticker) description creator contract-value expiration))
  ([ticker description creator contract-value expiration]
   (if (db-get (car/key :user creator :active))
     (let [id (random-user-id)]
       (do (wcar* (apply car/mset (crete-bet-key-value-list id 
                                                            ticker 
                                                            description 
                                                            creator 
                                                            contract-value 
                                                            expiration)))
           id)))))

(defn get-bet-data
  "Retrieves an bet's data as a map if it exists"
  [id]
  (into {:id id}
        (let [bet-keys (keys (default-bet-map))]
          (zipmap bet-keys 
                  (wcar* (apply car/mget (bet-fields-to-redis-keys id bet-keys)))))))

;; -------------------------
;; Orders

;; TODO

;; -------------------------
;; Test area

; (random-user-id)

; (car/key :teste :tamanho :hoje)
; (car/key :teste :tamanho :ontem)


; (create-user! "shing" "llddlol@bol.com" "12345555")

; (crete-user-key-value-list 1234 "blipblop" "lllol@bol.com" "12345")

; (crete-bet-key-value-list 1234 "What did the fox say?" 1234 (t/date-time 2019 12 13 11 0 0))

; (def bet-id (create-bet! "What did the fox say?" user-id (t/date-time 2019 12 13 11 0 0)))

; (bet-field-to-redis-key 1123 "creator")

(get-bet-data bet-id)

; (random-ticker)
; (wcar* (apply car/mset (default-entity-to-list (into default-user
;                                                      {:name "blipblop"
;                                                       :email "lllol@bol.com"
;                                                       :hash (hash-and-salt "12345")}))))

; (wcar* (car/get "user"))


; (get-all-user-names)
; (get-all-user-emails)

; (def user-id (get-user-id-by-name "shing"))
; (db-set! (car/key :user user-id :active) true)
; (db-get (car/key :user user-id :active))
(get-user-data user-id)

; (keys (default-user-map))
