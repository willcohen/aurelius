;; Copyright (c) 2019 Will Cohen
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

(ns aurelius.jts
  (:require [geo.jts :as geo.jts]
            [ovid.feature :as feature :refer [Featurelike]])
  (:import (org.locationtech.jts.algorithm MinimumDiameter)
           (org.locationtech.jts.operation.buffer
            BufferOp
            BufferParameters)
           (org.locationtech.jts.operation.union CascadedPolygonUnion)))

;; Internal wrapper functions for convenience

(defn- wfg
  "Wrap a function that operates on a Shapelike so that it can operate on a Featurelike."
  [feat f & args]
  (feature/update-geometry feat #(apply f (cons % args))))

(defn- wj
  "Wrap a function that operates on a Geometry so that it can operate on a Featurelike"
  [feat f & args]
  (apply wfg (cons (feature/jts-geometry feat) (cons f args))))


;; Predicates

(defn intersects?
  [feature-1 feature-2]
  (.intersects (feature/to-jts feature-1)
               (feature/to-jts feature-2)))

(defn touches?
  [feature-1 feature-2]
  (.touches (feature/to-jts feature-1)
            (feature/to-jts feature-2)))

(defn intersects?
  [feature-1 feature-2]
  (.intersects (feature/to-jts feature-1)
               (feature/to-jts feature-2)))

(defn within?
  [feature-1 feature-2]
  (.within (feature/to-jts feature-1)
           (feature/to-jts feature-2)))

(defn covered-by?
  [feature-1 feature-2]
  (.coveredBy (feature/to-jts feature-1)
              (feature/to-jts feature-2)))

(defn covers?
  [feature-1 feature-2]
  (.covers (feature/to-jts feature-1)
           (feature/to-jts feature-2)))

(defn equals-topo?
  [feature-1 feature-2]
  (.equalsTopo (feature/to-jts feature-1)
               (feature/to-jts feature-2)))

;; Values

(defn get-area
  [feature]
  (.getArea (feature/to-jts feature)))

(defn get-length
  [feature]
  (.getLength (feature/to-jts feature)))

(defn distance
  [feature-1 feature-2]
  (.distance (feature/to-jts feature-1)
             (feature/to-jts feature-2)))

;; Return modified feature

(defn get-boundary
  [feature]
  (wj feature #(.getBoundary %)))

(defn centroid
  [feature]
  (wj feature geo.jts/centroid))

(defn buffer
  ([feature dist]
   (wj
    feature
    (fn [geom dist] (.buffer geom (double dist)))
    dist))
  ([feature dist params]
   (wj
    feature
    (fn [geom dist ^BufferParameters params]
      (BufferOp/bufferOp geom (double dist) params))
    dist params)))

(defn one-sided-buffer
  [feature dist]
  (buffer feature (double dist) (doto (BufferParameters.)
                                  (.setSingleSided true))))
(defn left-buffer
  [feature dist]
  (one-sided-buffer feature (Math/abs dist)))

(defn right-buffer
  [feature dist]
  (one-sided-buffer feature (- (Math/abs dist))))

(defn difference
  [feature-1 feature-2]
  (wj feature-1
         (fn [g1 g2] (.difference g1 g2))
         (feature/to-jts feature-2)))

(defn intersection
  [feature-1 feature-2]
  (wj feature-1
         (fn [g1 g2] (.intersection g1 g2))
         (feature/to-jts feature-2)))

(defn get-minimum-rectangle
  [feat]
  (wj feat #(MinimumDiameter/getMinimumRectangle %)))


;; Return single geometry

(defn union
  [features]
  (CascadedPolygonUnion/union (into [] (map feature/to-jts) features)))
