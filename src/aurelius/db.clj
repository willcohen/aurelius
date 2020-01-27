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

(ns aurelius.db
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [com.stuartsierra.component :as component]
            [geo.io :as geo.io]
            [next.jdbc :as jdbc]
            [next.jdbc.connection :as connection]
            [next.jdbc.result-set :as rc])
  (:import (com.zaxxer.hikari HikariDataSource)
           (org.postgresql.util PGobject)))

(set! *warn-on-reflection* true)

;; Coerce PostGIS datatypes to JTS in JDBC results

(extend-protocol rc/ReadableColumn

  PGobject
  (read-column-by-label [x _]
    (try (geo.io/read-wkb-hex (.getValue x))
         (catch Exception _ x)))
  (read-column-by-index [x _2 _3]
    (try (geo.io/read-wkb-hex (.getValue x))
         (catch Exception _ x))))

;; Create Database component

(defrecord Database [db-spec ^HikariDataSource datasource]
  component/Lifecycle
  (start [this]
    (if datasource
      this ; already started
      (assoc this :datasource
             (connection/->pool HikariDataSource db-spec))))
  (stop [this]
    (if datasource
      (do
        (.close datasource)
        (assoc this :datasource nil))
      this))) ; already stopped


(def default-postgres-options
  {:dbtype "postgresql"
   :dbname "postgres"
   :host "localhost"
   :port 5432
   :username "postgres"
   :password "postgres"})

(defn postgres-database [options]
  (map->Database {:db-spec (merge default-postgres-options options)}))

;; Create internal sqlite datasource

(def adb
  (when-not *compile-files*
    (jdbc/get-datasource
     {:dbtype "sqlite"
      :dbname (io/resource "aurelius.db")})))
