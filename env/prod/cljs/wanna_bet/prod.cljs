(ns wanna-bet.prod
  (:require [wanna-bet.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
