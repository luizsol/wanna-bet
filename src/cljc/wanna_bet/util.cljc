(ns wanna-bet.util)

;; -------------------------
;; Auth

(defn validate-user-name-characters
  "Only lowercase letters and nubers are allowed as usernames"
  [s]
  (nil? (re-seq #"[^0-9a-z]+" s)))

(defn validate-user-name-len
  "Usernames must be at least 5 letters long and at maximux 20 letters long"
  [s]
  (and (>= (count s) 5) (<= (count s) 20)))

(defn validate-user
  [s]
  (and (validate-user-name-characters s) (validate-user-name-len s)))

;; Source https://stackoverflow.com/questions/33736473/how-to-validate-email-in-clojure
(defn validate-email
  [email]
  (let [pattern #"[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"]
    (and (string? email) (re-matches pattern email))))

;; -------------------------
;; Bets

;; source https://gist.github.com/rboyd/5053955
(defn rand-str [len] (apply str (take len (repeatedly #(char (+ (rand 26) 65))))))
