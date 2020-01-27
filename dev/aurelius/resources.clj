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

(ns aurelius.resources
  (:require [aurelius.sql :as aurelius.sql]
            [aurelius.util :as aurelius.util]
            [clojure.tools.logging :as log]
            [next.jdbc :as jdbc]
            [aurelius.db :as aurelius.db]))

(defn create-sqlite-resource
  []
  (jdbc/execute!
   aurelius.db/adb
   ["SELECT 1"]))

(defn delete-sqlite-resource-if-exists
  []
  (aurelius.util/dfie
   (aurelius.util/path "resources" "aurelius.db")))


(defn add-qcew-luts
  [db]
  (aurelius.sql/add-qcew-lut
   db :industry_titles :industry_code :industry_title
   "https://data.bls.gov/cew/doc/titles/industry/industry_titles.csv")
  (aurelius.sql/add-qcew-lut
   db :area_titles :area_fips :area_title
   "https://data.bls.gov/cew/doc/titles/area/area_titles.csv")
  (aurelius.sql/add-qcew-lut
   db :ownership_titles :own_code :own_title
   "https://data.bls.gov/cew/doc/titles/ownership/ownership_titles.csv")
  (aurelius.sql/add-qcew-lut
   db :size_titles :size_code :size_title
   "https://data.bls.gov/cew/doc/titles/size/size_titles.csv")
  (aurelius.sql/add-qcew-lut
   db :agglvl_titles :agglvl_code :agglvl_title
   "https://data.bls.gov/cew/doc/titles/agglevel/agglevel_titles.csv"))

(defn add-fips-areas
  [db]
  (aurelius.sql/add-fips-areas
   db :fips_areas
   "https://www2.census.gov/programs-surveys/popest/geographies/2018/all-geocodes-v2018.xlsx"))

