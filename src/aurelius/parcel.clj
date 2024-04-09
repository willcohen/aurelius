;; Copyright (c) 2024 Will Cohen
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

(ns aurelius.parcel
  (:require [aurelius.jts :as aurelius.jts]
            [clojure.string :as clojure.string]
            [clojure.tools.logging :as log]
            [geo.jts :as geo.jts]
            [ovid.feature :as feature])
  (:import  (org.locationtech.jts.geom Geometry
                                       LineSegment)))

(set! *warn-on-reflection* true)

(defn get-rectangle-dimensions
  [^Geometry g]
  (let [coords (.getCoordinates g)]
    (when (= 5 (count coords))
      (let [dim1 (.getLength ^LineSegment (geo.jts/line-segment (nth coords 0) (nth coords 1)))
            dim2 (.getLength ^LineSegment (geo.jts/line-segment (nth coords 1) (nth coords 2)))]
        (if (> dim1 dim2)
          {:rect_length dim1 :rect_width dim2 :rect_area (.getArea g)}
          {:rect_length dim2 :rect_width dim1 :rect_area (.getArea g)})))))

(defn get-rectangle-and-dims
  ([feature rectangle-type]
   (let [r (case rectangle-type
            :area (aurelius.jts/get-minimum-area-rectangle feature)
            :width (aurelius.jts/get-minimum-width-rectangle feature))
         dims-orig (get-rectangle-dimensions (feature/geometry r))
         rect-area-prop (/ (:rect_area dims-orig) (.getArea ^Geometry (feature/geometry feature)))
         dims (merge dims-orig
                     {:rect_area_prop rect-area-prop
                      :rectangular? (if (<= rect-area-prop 1.2)
                                      "true"
                                      "false")})]
     {:rect r :dims dims}))
  ([feature]
   (get-rectangle-and-dims feature :area)))

(defn replace-with-rectangle
  ([feature rectangle-type]
   (let [g (get-rectangle-and-dims feature rectangle-type)
         r (:rect g)
         dims (:dims g)]
     (if (= 2 (.getDimension ^Geometry (feature/geometry r)))
       (-> feature
           (feature/assoc-geometry (feature/geometry r))
           (feature/update-properties #(merge % dims)))
       feature)))
  ([feature]
   (replace-with-rectangle feature :area)))

(defn add-rectangle-dims
  ([feature rectangle-type]
   (let [g (get-rectangle-and-dims feature rectangle-type)
         r (:rect g)
         dims (:dims g)]
     (if (= 2 (.getDimension ^Geometry (feature/geometry r)))
       (-> feature
           (feature/update-properties #(merge % dims)))
       feature)))
  ([feature]
   (add-rectangle-dims feature :area)))

(defn dims-with-parid
  ([feature rectangle-type parid-column]
   (let [g (get-rectangle-and-dims feature rectangle-type)]
     (merge {parid-column (parid-column (feature/properties feature))} (:dims g))))
  ([feature parid-column]
   (dims-with-parid feature :area parid-column)))

(defn find-intersecting-features
  "Find all features from a set of features that touch a feature f."
  [f features]
  (let [fn (aurelius.jts/intersects? f)]
    (filter #(not (nil? %)) (map #(if (fn %) % nil) features))))


(defn setback-left-right
  "Determine direction edge needs to offset to shift into the parcel.
   Left means a positive distance, right means a negative distance value.
   Default increment of 0.01"
  ([edge parcel]
   (setback-left-right edge parcel 0.01))
  ([edge parcel increment]
   (loop [setback increment]
     (let [left (aurelius.jts/offset-line-string edge setback :line)
           right (aurelius.jts/offset-line-string edge (- 0 setback) :line)]
       (cond (or (nil? left)
                 (nil? right))
             (do (log/info "Edge cannot be offset, returning nil for direction.")
                 nil)
             (aurelius.jts/intersects? left parcel)
             :left
             (aurelius.jts/intersects? right parcel)
             :right
             :else
             (recur (inc setback)))))))

(defn setback-to-building
  "Determine distance from edge to building.
   Default increment of 1"
  ([edge parcel building]
   (setback-to-building edge parcel building 1 5000))
  ([edge parcel building increment overflow]
   (let [offset-direction (setback-left-right edge parcel increment)
         directional-increment (case offset-direction
                                 nil nil
                                 :left increment
                                 :right (- 0 increment))]
     (cond
       ; If offset-direction is nil, then return nil.
       (nil? offset-direction)
       nil
       ; If building touches edge, setback is 0.
       (aurelius.jts/intersects? edge building)
       0
       ; Otherwise, loop inwards until it touches.
       :else
       (loop [setback 0]
         (cond
           (aurelius.jts/intersects? building
                             (aurelius.jts/offset-line-string (feature/geometry edge)
                                                              (+ setback directional-increment)
                                                              :line))
           ; If the shifted-in setback touches, return the current setback.
           (abs setback)
           ; If setback overflows, return nil.
           (>= (abs setback) overflow)
           nil
           :else
           (recur (long (+ setback directional-increment)))))))))

(defn min-setbacks
  "Given a number of calculated setbacks across a parcel's edges,
   determine each broader edge's minimum setback, conservatively."
  [edges]
  (conj {:front nil :side nil :rear nil}
        (if (= '() edges)
          nil
          (let [grouped (partition-by #(:edge-type (feature/properties %)) edges)
                max-setback (fn [setbacks]
                              (cond (= 0 (count setbacks))
                                    0
                                    (= 1 (count setbacks))
                                    (first setbacks)
                                    :else
                                    (apply min setbacks)))
                setbacks (map (fn [setback-type-set]
                                {(keyword (:edge-type (feature/properties
                                                       (first setback-type-set))))
                                 (max-setback
                                  (filter #(not (nil? %))
                                          (map (fn [s]
                                                 (:setback-distance (feature/properties s)))
                                               setback-type-set)))})
                              grouped)]
            (apply merge setbacks)))))

(defn sort-frontages
  [edges]
  (let [partitioned (partition-by #(:edge-type (feature/properties %))
                                  (sort-by #(:edge-type (feature/properties %)) edges))
        sort-fn (fn [edge-set] (reverse (sort-by #(:edge-length (feature/properties %)) edge-set)))
        sorted (into [] (map sort-fn partitioned))]
    sorted))

(defn frontage-attributes
  [frontages]
  (let [frontage-count-fn (fn [frontage-set] (count frontage-set))
        frontage-type-fn (fn [frontage-set] (:edge-type (feature/properties (first frontage-set))))
        frontages-fn (fn [frontage-set]
                       (let [n (frontage-count-fn frontage-set)
                             type (frontage-type-fn frontage-set)
                             indices (range n)
                             frontage-maker-fn
                             (fn [type idx length]
                               {(keyword (clojure.string/join [type idx])) length})]
                         (apply merge (map #(frontage-maker-fn type % (:edge-length (feature/properties (nth frontage-set %)))) indices))))]
    (apply merge (map frontages-fn frontages))))
