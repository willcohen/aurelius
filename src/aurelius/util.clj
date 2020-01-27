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

(ns aurelius.util
  (:require [byte-streams :as byte-streams]
            [clojure.data.csv :as csv]
            [clojure.tools.logging :as log]
            [hato.client :as hato.client]
            [java-time :as jt])
  (:import (java.io File)
           (java.nio.file Files Path Paths)
           (java.nio.file.attribute FileAttribute)
           (org.apache.poi.ss.usermodel
            Cell CellType Row Sheet WorkbookFactory Workbook)
           (org.slf4j.bridge SLF4JBridgeHandler)
           (org.threeten.extra YearQuarter)))

(set! *warn-on-reflection* true)

(defn hpost
  ([uri]
   (hpost uri {}))
  ([uri options]
   (hato.client/post uri (merge-hc options))))

(defn hput
  ([uri]
   (hput uri {}))
  ([uri options]
   (hato.client/put uri (merge-hc options))))

(defn hpatch
  ([uri]
   (hpatch uri {}))
  ([uri options]
   (hato.client/patch uri (merge-hc options))))

(defn hdelete
  ([uri]
   (hdelete uri {}))
  ([uri options]
   (hato.client/delete uri (merge-hc options))))

(defn hhead
  ([uri]
   (hhead uri {}))
  ([uri options]
   (hato.client/head uri (merge-hc options))))

(defn hoptions
  ([uri]
   (hoptions uri {}))
  ([uri options]
   (hato.client/options uri (merge-hc options))))

;; CSV

(defn csv->maps [csv]
  (map zipmap (->> (first csv)
                   (map keyword)
                   repeat)
       (rest csv)))

(defn csv-url->maps
  [url]
  (csv->maps
   (csv/read-csv
    (:body (hget url)))))

;; Apache POI

(defn read-ss
  [f]
  (WorkbookFactory/create f))

(defn ss-url->workbook
  [url]
  (-> url
      (hget {:as :byte-array})
      :body
      byte-streams/to-input-stream
      read-ss))

(defn workbook-get-sheet-at
  [^Workbook wb n]
  (.getSheetAt wb n))

(defn workbook->sheets
  [^Workbook wb]
  (map (partial workbook-get-sheet-at wb)
       (range (.getNumberOfSheets wb))))

(defn sheet-get-row
  [^Sheet s n]
  (.getRow s n))

(defn sheet->rows
  [^Sheet s]
  (try (let [f (.getFirstRowNum s)
             l (.getLastRowNum s)
             n (- l f)
             nums (map (fn [r] (+ r f)) (range n))]
         (into [] (map (partial sheet-get-row s) nums)))
       (catch Exception _ nil)))

(defn get-cell-type
  [^Cell c]
  (-> c
      .getCellType
      .toString
      CellType/valueOf))

(defn cell->value
  [^Cell c]
  (try (let [ct (get-cell-type c)]
         (cond (= ct CellType/BLANK) nil
               (= ct CellType/BOOLEAN) (.getBooleanCellValue c)
               (= ct CellType/ERROR) (.getErrorCellValue c)
               (= ct CellType/FORMULA) (.getCellFormula c)
               (= ct CellType/NUMERIC) (.getNumericCellValue c)
               (= ct CellType/STRING) (.getStringCellValue c)
               (= ct CellType/_NONE) nil))
       (catch Exception _ nil)))

(defn row-get-cell
  [^Row r n]
  (cell->value (.getCell r n)))

(defn row->cells
  [^Row r]
  (try
    (let [f (.getFirstCellNum r)
          l (.getLastCellNum r)
          n (- l f)
          nums (map (fn [c] (+ c f)) (range n))]
      (into [] (map (partial row-get-cell r) nums)))
    (catch Exception _ nil)))

