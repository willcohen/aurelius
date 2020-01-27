;; Copyright (c) 2019, 2020 Will Cohen
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns aurelius.census
  (:require [aurelius.db :as aurelius.db]
            [aurelius.util :as u]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [hugsql.core :as hugsql]
            [geo.io :as geo.io]
            [geo.jts :as geo.jts]
            [ovid.feature :as feature :refer [Featurelike]]))

(set! *warn-on-reflection* true)

(defn add-qcew-monthly-data
  [r month]
  (assoc r
         :month (u/quarter->month (:year r) (:qtr r) month)
         :emplvl ((keyword
                   (clojure.string/join ["month" month "_emplvl"])) r)
         :lq_emplvl ((keyword
                      (clojure.string/join
                       ["lq_month" month "_emplvl"])) r)
         :oty_emplvl_chg ((keyword
                           (clojure.string/join
                            ["oty_month" month "_emplvl_chg"])) r)
         :oty_emplvl_pct_chg ((keyword
                               (clojure.string/join
                                ["oty_month" month "_emplvl_pct_chg"])) r)))

(defn remove-qcew-monthly-data
  [r]
  (dissoc r :year :qtr
            :month1_emplvl :month2_emplvl :month3_emplvl
            :lq_month1_emplvl :lq_month2_emplvl :lq_month3_emplvl
            :oty_month1_emplvl_chg
            :oty_month2_emplvl_chg
            :oty_month3_emplvl_chg
            :oty_month1_emplvl_pct_chg
            :oty_month2_emplvl_pct_chg
            :oty_month3_emplvl_pct_chg))

(defn qcew-monthly-data
  [r month]
  (-> r
      (add-qcew-monthly-data month)
      remove-qcew-monthly-data))

(defn qcew-qtr->month
  [r]
  (map #(qcew-monthly-data r (inc %)) (range 3)))

