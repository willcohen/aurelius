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

(ns aurelius.sql
  (:require [aurelius.util :as aurelius.util]
            [clojure.tools.logging :as log]
            [honeysql.core :as hsql]
            [honeysql.helpers :as hsql.helpers]
            [honeysql-postgres.format :as psql.format]
            [honeysql-postgres.helpers :as psql.helpers]
            [next.jdbc :as jdbc]))

(set! *warn-on-reflection* true)

;; SQL

(defn dt
  "Drops a database table."
  ([table-name]
   (hsql/format
    (psql.helpers/drop-table table-name)))
  ([db table-name]
   (jdbc/execute! db (dt table-name))))

(defn dtie
  "Drops a database table if exists."
  ([table-name]
   (hsql/format
    (psql.helpers/drop-table :if-exists table-name)))
  ([db table-name]
   (jdbc/execute! db (dtie table-name))))

;;; Census

(defn query-qcew-raw
  ([table-name area-fips industry-code]
   (-> (hsql.helpers/select :*)
       (hsql.helpers/from table-name)
       (hsql.helpers/where
        [:in :area-fips area-fips]
        [:in :industry-code industry-code])
       hsql/format))
  ([db table-name area-fips industry-code]
   (jdbc/execute! db (query-qcew-raw table-name area-fips industry-code))))


(defn get-two-digit-naics-raw
  ([table-name]
   (-> (hsql.helpers/select :industry-code)
       (hsql.helpers/modifiers :distinct)
       (hsql.helpers/from table-name)
       (hsql.helpers/where
        [:or
         [:= (hsql/call :length :industry_code)
          2]
         [:and
          [:= (hsql/call :length :industry_code)
           5]
          [:= (hsql/call :substr :industry_code (hsql/inline 3) (hsql/inline 1))
           "-"]]])
       hsql/format))
  ([db table-name]
   (jdbc/execute! db (get-two-digit-naics-raw table-name))))


(defn insert-qcew-title
  ([table-name code-name title-name record]
   (-> (hsql.helpers/insert-into table-name)
       (hsql.helpers/values [{code-name (code-name record)
                             title-name (title-name record)}])
       hsql/format))
  ([db table-name code-name title-name record]
   (jdbc/execute!
    db (insert-qcew-title
        table-name code-name title-name record))))

(defn bulk-insert-qcew-titles
  ([table-name code-name title-name titles]
   (map (partial
         insert-qcew-title table-name code-name title-name)
        titles))
  ([db table-name code-name title-name titles]
   (jdbc/with-transaction [tx db]
     (doall
      (map (partial jdbc/execute! tx)
           (bulk-insert-qcew-titles
            table-name code-name title-name titles))))))

(defn add-qcew-lut
  [db table-name code-column title-column url]
  (dtie db table-name)
  (jdbc/execute!
   db
   (-> (psql.helpers/create-table table-name)
       (psql.helpers/with-columns
         [[:id :integer
           (hsql/call :primary-key) (hsql/raw "autoincrement")]
          [code-column :text]
          [title-column :text]])
       hsql/format))
  (bulk-insert-qcew-titles
   db table-name code-column title-column
   (aurelius.util/csv-url->maps url)))

(defn insert-fips-title
  ([table-name record]
   (-> (hsql.helpers/insert-into table-name)
       (hsql.helpers/values [record])
       hsql/format))
  ([db table-name record]
   (jdbc/execute!
    db (insert-fips-title table-name record))))

(defn bulk-insert-fips-titles
  ([table-name titles]
   (map (partial insert-fips-title table-name) titles))
  ([db table-name titles]
   (jdbc/with-transaction [tx db]
     (doall
      (map (partial jdbc/execute! tx)
           (bulk-insert-fips-titles table-name titles))))))

(defn get-fips-titles
  [url]
  (let [wb (aurelius.util/ss-url->workbook url)
        s (aurelius.util/workbook-get-sheet-at wb 0)
        rows (vec (aurelius.util/sheet->rows s))
        subset-rows (drop 4 rows) ; First 4 rows are headers
        cells (into [] (map aurelius.util/row->cells subset-rows))
        dropped-first-row (drop 1 cells)
        new-title-row ; Use keywords for new first row
        (into
         [[:summary_level
           :state_code_fips
           :county_code_fips
           :county_subdivision_code_fips
           :place_code_fips
           :consolidated_city_code_fips
           :area_name]] dropped-first-row)]
      (aurelius.util/csv->maps new-title-row)))

(defn add-fips-areas
  ([table-name]
   (-> (psql.helpers/create-table table-name)
       (psql.helpers/with-columns
         [[:id :integer
           (hsql/call :primary-key) (hsql/raw "autoincrement")]
          [:summary-level :text]
          [:state-code-fips :text]
          [:county-code-fips :text]
          [:county-subdivision-code-fips :text]
          [:place-code-fips :text]
          [:consolidated-city-code-fips :text]
          [:area-name :text]])
       hsql/format))
  ([db table-name url]
   (dtie db table-name)
   (jdbc/execute! db (add-fips-areas table-name))
   (bulk-insert-fips-titles
    db table-name (get-fips-titles url))))
