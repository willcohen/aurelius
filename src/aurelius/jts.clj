;; Copyright (c) 2019, 2020, 2024 Will Cohen
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
  (:require [clojure.math.numeric-tower :as math]
            [clojure.tools.logging :as log]
            [geo.jts :as geo.jts]
            [geo.crs :as geo.crs]
            [geo.spatial :as spatial :refer [Shapelike]]
            [ovid.feature :as feature :refer [Featurelike
                                              -to-shape
                                              -to-jts
                                              -to-feature
                                              -geometry
                                              -properties
                                              -assoc-geometry
                                              -update-geometry
                                              -assoc-properties
                                              -update-properties]]
            [geo.geohash :as geo.geohash])
  (:import (ch.hsr.geohash GeoHash WGS84Point)
           (com.uber.h3core.util LatLng)
           (org.locationtech.jts.algorithm MinimumDiameter MinimumAreaRectangle)
           (org.locationtech.jts.geom Geometry
                                      GeometryFactory
                                      PrecisionModel)
           (org.locationtech.jts.geom.prep PreparedGeometry
                                           PreparedGeometryFactory)
           (org.locationtech.jts.operation.buffer
            BufferOp
            BufferParameters
            OffsetCurveBuilder)
           (org.locationtech.jts.operation.union CascadedPolygonUnion)
           (org.locationtech.spatial4j.shape Shape)
           (org.locationtech.spatial4j.shape.impl
            GeoCircle PointImpl RectangleImpl)
           (org.locationtech.spatial4j.shape.jts JtsGeometry
                                                 JtsPoint)))

(set! *warn-on-reflection* true)

(def ^PreparedGeometryFactory pgf (PreparedGeometryFactory.))

(defprotocol Preparable
  (-prepare [this]
    "Internal helper for prepare")
  (-unprepare [this]
    "Internal helper for unprepare")
  (-contains? [this] [this feat]
    "Internal helper for contains?")
  (-contains-properly? [this] [this feat]
    "Internal helper for contains-properly?")
  (-covered-by? [this] [this feat]
    "Internal helper for covered-by?")
  (-covers? [this] [this feat]
    "Internal helper for covers?")
  (-crosses? [this] [this feat]
    "Internal helper for crosses?")
  (-disjoint? [this] [this feat]
    "Internal helper for disjoint?")
  (-intersects? [this] [this feat]
    "Internal helper for intersects?")
  (-overlaps? [this] [this feat]
    "Internal helper for overlaps?")
  (-touches? [this] [this feat]
    "Internal helper for touches?")
  (-within? [this] [this feat]
    "Internal helper for within?"))

(defn- g-contains?
  [^Geometry g1 ^Geometry g2]
  (.contains g1 g2))

(defn- p-contains?
  [^PreparedGeometry p ^Geometry g]
  (.contains p g))

(defn- p-contains-properly?
  [^PreparedGeometry p ^Geometry g]
  (.containsProperly p g))

(defn- g-covered-by?
  [^Geometry g1 ^Geometry g2]
  (.coveredBy g1 g2))

(defn- p-covered-by?
  [^PreparedGeometry p ^Geometry g]
  (.coveredBy p g))

(defn- g-covers?
  [^Geometry g1 ^Geometry g2]
  (.covers g1 g2))

(defn- p-covers?
  [^PreparedGeometry p ^Geometry g]
  (.covers p g))

(defn- g-crosses?
  [^Geometry g1 ^Geometry g2]
  (.crosses g1 g2))

(defn- p-crosses?
  [^PreparedGeometry p ^Geometry g]
  (.crosses p g))

(defn- g-disjoint?
  [^Geometry g1 ^Geometry g2]
  (.disjoint g1 g2))

(defn- p-disjoint?
  [^PreparedGeometry p ^Geometry g]
  (.disjoint p g))

(defn- g-intersects?
  [^Geometry g1 ^Geometry g2]
  (.intersects g1 g2))

(defn- p-intersects?
  [^PreparedGeometry p ^Geometry g]
  (.intersects p g))

(defn- g-overlaps?
  [^Geometry g1 ^Geometry g2]
  (.overlaps g1 g2))

(defn- p-overlaps?
  [^PreparedGeometry p ^Geometry g]
  (.overlaps p g))

(defn- g-touches?
  [^Geometry g1 ^Geometry g2]
  (.touches g1 g2))

(defn- p-touches?
  [^PreparedGeometry p ^Geometry g]
  (.touches p g))

(defn- g-within?
  [^Geometry g1 ^Geometry g2]
  (.within g1 g2))

(defn- p-within?
  [^PreparedGeometry p ^Geometry g]
  (.within p g))

(extend-protocol Preparable
  Geometry
  (-prepare [this] (.create pgf this))
  (-unprepare [this] this)
  (-contains?
    ([this] (fn [f] (p-contains? (-prepare this) (-unprepare (feature/to-jts f)))))
    ([this feat] (g-contains? this (-unprepare (feature/to-jts feat)))))
  (-contains-properly?
    ([this] (fn [f] (-contains-properly? this f)))
    ([this feat] (p-contains-properly? (-prepare this) (-unprepare (feature/to-jts feat)))))
  (-covered-by?
    ([this] (fn [f] (p-covered-by? (-prepare this) (-unprepare (feature/to-jts f)))))
    ([this feat] (g-covered-by? this (-unprepare (feature/to-jts feat)))))
  (-covers?
    ([this] (fn [f] (p-covers? (-prepare this) (-unprepare (feature/to-jts f)))))
    ([this feat] (g-covers? this (-unprepare (feature/to-jts feat)))))
  (-crosses?
    ([this] (fn [f] (p-crosses? (-prepare this) (-unprepare (feature/to-jts f)))))
    ([this feat] (g-crosses? this (-unprepare (feature/to-jts feat)))))
  (-disjoint?
    ([this] (fn [f] (p-disjoint? (-prepare this) (-unprepare (feature/to-jts f)))))
    ([this feat] (g-disjoint? this (-unprepare (feature/to-jts feat)))))
  (-intersects?
    ([this] (fn [f] (p-intersects? (-prepare this) (-unprepare (feature/to-jts f)))))
    ([this feat] (g-intersects? this (-unprepare (feature/to-jts feat)))))
  (-overlaps?
    ([this] (fn [f] (p-overlaps? (-prepare this) (-unprepare (feature/to-jts f)))))
    ([this feat] (g-overlaps? this (-unprepare (feature/to-jts feat)))))
  (-touches?
    ([this] (fn [f] (p-touches? (-prepare this) (-unprepare (feature/to-jts f)))))
    ([this feat] (g-touches? this (-unprepare (feature/to-jts feat)))))
  (-within?
    ([this] (fn [f] (p-within? (-prepare this) (-unprepare (feature/to-jts f)))))
    ([this feat] (g-within? this (-unprepare (feature/to-jts feat)))))

  PreparedGeometry
  (-prepare [this] this)
  (-unprepare [this] (.getGeometry this))
  (-contains?
    ([this] (fn [f] (p-contains? this (-unprepare (feature/to-jts f)))))
    ([this feat] (p-contains? this (-unprepare (feature/to-jts feat)))))
  (-contains-properly?
    ([this] (fn [f] (p-contains-properly? this (-unprepare (feature/to-jts f)))))
    ([this feat] (p-contains-properly? this (-unprepare (feature/to-jts feat)))))
  (-covered-by?
    ([this] (fn [f] (p-covered-by? this (-unprepare (feature/to-jts f)))))
    ([this feat] (p-covered-by? this (-unprepare (feature/to-jts feat)))))
  (-covers?
    ([this] (fn [f] (p-covers? this (-unprepare (feature/to-jts f)))))
    ([this feat] (p-covers? this (-unprepare (feature/to-jts feat)))))
  (-crosses?
    ([this] (fn [f] (p-crosses? this (-unprepare (feature/to-jts f)))))
    ([this feat] (p-crosses? this (-unprepare (feature/to-jts feat)))))
  (-disjoint?
    ([this] (fn [f] (p-disjoint? this (-unprepare (feature/to-jts f)))))
    ([this feat] (p-disjoint? this (-unprepare (feature/to-jts feat)))))
  (-intersects?
    ([this] (fn [f] (p-intersects? this (-unprepare (feature/to-jts f)))))
    ([this feat] (p-intersects? this (-unprepare (feature/to-jts feat)))))
  (-overlaps?
    ([this] (fn [f] (p-overlaps? this (-unprepare (feature/to-jts f)))))
    ([this feat] (p-overlaps? this (-unprepare (feature/to-jts feat)))))
  (-touches?
    ([this] (fn [f] (p-touches? this (-unprepare (feature/to-jts f)))))
    ([this feat] (p-touches? this (-unprepare (feature/to-jts feat)))))
  (-within?
    ([this] (fn [f] (p-within? this (-unprepare (feature/to-jts f)))))
    ([this feat] (p-within? this (-unprepare (feature/to-jts feat)))))
  
  GeoCircle
  (-prepare [this] (-prepare (feature/to-jts this)))
  (-unprepare [this] this)
  (-contains?
    ([this] (-contains? (feature/to-jts this)))
    ([this feat] (-contains? (feature/to-jts this) feat)))
  (-contains-properly?
    ([this] (-contains-properly? (feature/to-jts this)))
    ([this feat] (-contains-properly? (feature/to-jts this) feat)))
  (-covered-by?
    ([this] (-covered-by? (feature/to-jts this)))
    ([this feat] (-covered-by? (feature/to-jts this) feat)))
  (-covers?
    ([this] (-covers? (feature/to-jts this)))
    ([this feat] (-covers? (feature/to-jts this) feat)))
  (-crosses?
    ([this] (-crosses? (feature/to-jts this)))
    ([this feat] (-crosses? (feature/to-jts this) feat)))
  (-disjoint?
    ([this] (-disjoint? (feature/to-jts this)))
    ([this feat] (-disjoint? (feature/to-jts this) feat)))
  (-intersects?
    ([this] (-intersects? (feature/to-jts this)))
    ([this feat] (-intersects? (feature/to-jts this) feat)))
  (-overlaps?
    ([this] (-overlaps? (feature/to-jts this)))
    ([this feat] (-overlaps? (feature/to-jts this) feat)))
  (-touches?
    ([this] (-touches? (feature/to-jts this)))
    ([this feat] (-touches? (feature/to-jts this) feat)))
  (-within?
    ([this] (-within? (feature/to-jts this)))
    ([this feat] (-within? (feature/to-jts this) feat)))

  RectangleImpl
  (-prepare [this] (-prepare (feature/to-jts this)))
  (-unprepare [this] this)
  (-contains?
    ([this] (-contains? (feature/to-jts this)))
    ([this feat] (-contains? (feature/to-jts this) feat)))
  (-contains-properly?
    ([this] (-contains-properly? (feature/to-jts this)))
    ([this feat] (-contains-properly? (feature/to-jts this) feat)))
  (-covered-by?
    ([this] (-covered-by? (feature/to-jts this)))
    ([this feat] (-covered-by? (feature/to-jts this) feat)))
  (-covers?
    ([this] (-covers? (feature/to-jts this)))
    ([this feat] (-covers? (feature/to-jts this) feat)))
  (-crosses?
    ([this] (-crosses? (feature/to-jts this)))
    ([this feat] (-crosses? (feature/to-jts this) feat)))
  (-disjoint?
    ([this] (-disjoint? (feature/to-jts this)))
    ([this feat] (-disjoint? (feature/to-jts this) feat)))
  (-intersects?
    ([this] (-intersects? (feature/to-jts this)))
    ([this feat] (-intersects? (feature/to-jts this) feat)))
  (-overlaps?
    ([this] (-overlaps? (feature/to-jts this)))
    ([this feat] (-overlaps? (feature/to-jts this) feat)))
  (-touches?
    ([this] (-touches? (feature/to-jts this)))
    ([this feat] (-touches? (feature/to-jts this) feat)))
  (-within?
    ([this] (-within? (feature/to-jts this)))
    ([this feat] (-within? (feature/to-jts this) feat)))

  PointImpl
  (-prepare [this] (-prepare (feature/to-jts this)))
  (-unprepare [this] this)
  (-contains?
    ([this] (-contains? (feature/to-jts this)))
    ([this feat] (-contains? (feature/to-jts this) feat)))
  (-contains-properly?
    ([this] (-contains-properly? (feature/to-jts this)))
    ([this feat] (-contains-properly? (feature/to-jts this) feat)))
  (-covered-by?
    ([this] (-covered-by? (feature/to-jts this)))
    ([this feat] (-covered-by? (feature/to-jts this) feat)))
  (-covers?
    ([this] (-covers? (feature/to-jts this)))
    ([this feat] (-covers? (feature/to-jts this) feat)))
  (-crosses?
    ([this] (-crosses? (feature/to-jts this)))
    ([this feat] (-crosses? (feature/to-jts this) feat)))
  (-disjoint?
    ([this] (-disjoint? (feature/to-jts this)))
    ([this feat] (-disjoint? (feature/to-jts this) feat)))
  (-intersects?
    ([this] (-intersects? (feature/to-jts this)))
    ([this feat] (-intersects? (feature/to-jts this) feat)))
  (-overlaps?
    ([this] (-overlaps? (feature/to-jts this)))
    ([this feat] (-overlaps? (feature/to-jts this) feat)))
  (-touches?
    ([this] (-touches? (feature/to-jts this)))
    ([this feat] (-touches? (feature/to-jts this) feat)))
  (-within?
    ([this] (-within? (feature/to-jts this)))
    ([this feat] (-within? (feature/to-jts this) feat)))

  JtsGeometry
  (-prepare [this] (-prepare (feature/to-jts this)))
  (-unprepare [this] this)
  (-contains?
    ([this] (-contains? (feature/to-jts this)))
    ([this feat] (-contains? (feature/to-jts this) feat)))
  (-contains-properly?
    ([this] (-contains-properly? (feature/to-jts this)))
    ([this feat] (-contains-properly? (feature/to-jts this) feat)))
  (-covered-by?
    ([this] (-covered-by? (feature/to-jts this)))
    ([this feat] (-covered-by? (feature/to-jts this) feat)))
  (-covers?
    ([this] (-covers? (feature/to-jts this)))
    ([this feat] (-covers? (feature/to-jts this) feat)))
  (-crosses?
    ([this] (-crosses? (feature/to-jts this)))
    ([this feat] (-crosses? (feature/to-jts this) feat)))
  (-disjoint?
    ([this] (-disjoint? (feature/to-jts this)))
    ([this feat] (-disjoint? (feature/to-jts this) feat)))
  (-intersects?
    ([this] (-intersects? (feature/to-jts this)))
    ([this feat] (-intersects? (feature/to-jts this) feat)))
  (-overlaps?
    ([this] (-overlaps? (feature/to-jts this)))
    ([this feat] (-overlaps? (feature/to-jts this) feat)))
  (-touches?
    ([this] (-touches? (feature/to-jts this)))
    ([this feat] (-touches? (feature/to-jts this) feat)))
  (-within?
    ([this] (-within? (feature/to-jts this)))
    ([this feat] (-within? (feature/to-jts this) feat)))


  JtsPoint
  (-prepare [this] (-prepare (feature/to-jts this)))
  (-unprepare [this] this)
  (-contains?
    ([this] (-contains? (feature/to-jts this)))
    ([this feat] (-contains? (feature/to-jts this) feat)))
  (-contains-properly?
    ([this] (-contains-properly? (feature/to-jts this)))
    ([this feat] (-contains-properly? (feature/to-jts this) feat)))
  (-covered-by?
    ([this] (-covered-by? (feature/to-jts this)))
    ([this feat] (-covered-by? (feature/to-jts this) feat)))
  (-covers?
    ([this] (-covers? (feature/to-jts this)))
    ([this feat] (-covers? (feature/to-jts this) feat)))
  (-crosses?
    ([this] (-crosses? (feature/to-jts this)))
    ([this feat] (-crosses? (feature/to-jts this) feat)))
  (-disjoint?
    ([this] (-disjoint? (feature/to-jts this)))
    ([this feat] (-disjoint? (feature/to-jts this) feat)))
  (-intersects?
    ([this] (-intersects? (feature/to-jts this)))
    ([this feat] (-intersects? (feature/to-jts this) feat)))
  (-overlaps?
    ([this] (-overlaps? (feature/to-jts this)))
    ([this feat] (-overlaps? (feature/to-jts this) feat)))
  (-touches?
    ([this] (-touches? (feature/to-jts this)))
    ([this feat] (-touches? (feature/to-jts this) feat)))
  (-within?
    ([this] (-within? (feature/to-jts this)))
    ([this feat] (-within? (feature/to-jts this) feat)))

  LatLng
  (-prepare [this] (-prepare (feature/to-jts this)))
  (-unprepare [this] this)
  (-contains?
    ([this] (-contains? (feature/to-jts this)))
    ([this feat] (-contains? (feature/to-jts this) feat)))
  (-contains-properly?
    ([this] (-contains-properly? (feature/to-jts this)))
    ([this feat] (-contains-properly? (feature/to-jts this) feat)))
  (-covered-by?
    ([this] (-covered-by? (feature/to-jts this)))
    ([this feat] (-covered-by? (feature/to-jts this) feat)))
  (-covers?
    ([this] (-covers? (feature/to-jts this)))
    ([this feat] (-covers? (feature/to-jts this) feat)))
  (-crosses?
    ([this] (-crosses? (feature/to-jts this)))
    ([this feat] (-crosses? (feature/to-jts this) feat)))
  (-disjoint?
    ([this] (-disjoint? (feature/to-jts this)))
    ([this feat] (-disjoint? (feature/to-jts this) feat)))
  (-intersects?
    ([this] (-intersects? (feature/to-jts this)))
    ([this feat] (-intersects? (feature/to-jts this) feat)))
  (-overlaps?
    ([this] (-overlaps? (feature/to-jts this)))
    ([this feat] (-overlaps? (feature/to-jts this) feat)))
  (-touches?
    ([this] (-touches? (feature/to-jts this)))
    ([this feat] (-touches? (feature/to-jts this) feat)))
  (-within?
    ([this] (-within? (feature/to-jts this)))
    ([this feat] (-within? (feature/to-jts this) feat)))

  GeoHash
  (-prepare [this] (-prepare (feature/to-jts this)))
  (-unprepare [this] this)
  (-contains?
    ([this] (-contains? (feature/to-jts this)))
    ([this feat] (-contains? (feature/to-jts this) feat)))
  (-contains-properly?
    ([this] (-contains-properly? (feature/to-jts this)))
    ([this feat] (-contains-properly? (feature/to-jts this) feat)))
  (-covered-by?
    ([this] (-covered-by? (feature/to-jts this)))
    ([this feat] (-covered-by? (feature/to-jts this) feat)))
  (-covers?
    ([this] (-covers? (feature/to-jts this)))
    ([this feat] (-covers? (feature/to-jts this) feat)))
  (-crosses?
    ([this] (-crosses? (feature/to-jts this)))
    ([this feat] (-crosses? (feature/to-jts this) feat)))
  (-disjoint?
    ([this] (-disjoint? (feature/to-jts this)))
    ([this feat] (-disjoint? (feature/to-jts this) feat)))
  (-intersects?
    ([this] (-intersects? (feature/to-jts this)))
    ([this feat] (-intersects? (feature/to-jts this) feat)))
  (-overlaps?
    ([this] (-overlaps? (feature/to-jts this)))
    ([this feat] (-overlaps? (feature/to-jts this) feat)))
  (-touches?
    ([this] (-touches? (feature/to-jts this)))
    ([this feat] (-touches? (feature/to-jts this) feat)))
  (-within?
    ([this] (-within? (feature/to-jts this)))
    ([this feat] (-within? (feature/to-jts this) feat)))

  WGS84Point
  (-prepare [this] (-prepare (feature/to-jts this)))
  (-unprepare [this] this)
  (-contains?
    ([this] (-contains? (feature/to-jts this)))
    ([this feat] (-contains? (feature/to-jts this) feat)))
  (-contains-properly?
    ([this] (-contains-properly? (feature/to-jts this)))
    ([this feat] (-contains-properly? (feature/to-jts this) feat)))
  (-covered-by?
    ([this] (-covered-by? (feature/to-jts this)))
    ([this feat] (-covered-by? (feature/to-jts this) feat)))
  (-covers?
    ([this] (-covers? (feature/to-jts this)))
    ([this feat] (-covers? (feature/to-jts this) feat)))
  (-crosses?
    ([this] (-crosses? (feature/to-jts this)))
    ([this feat] (-crosses? (feature/to-jts this) feat)))
  (-disjoint?
    ([this] (-disjoint? (feature/to-jts this)))
    ([this feat] (-disjoint? (feature/to-jts this) feat)))
  (-intersects?
    ([this] (-intersects? (feature/to-jts this)))
    ([this feat] (-intersects? (feature/to-jts this) feat)))
  (-overlaps?
    ([this] (-overlaps? (feature/to-jts this)))
    ([this feat] (-overlaps? (feature/to-jts this) feat)))
  (-touches?
    ([this] (-touches? (feature/to-jts this)))
    ([this feat] (-touches? (feature/to-jts this) feat)))
  (-within?
    ([this] (-within? (feature/to-jts this)))
    ([this feat] (-within? (feature/to-jts this) feat)))

  clojure.lang.PersistentArrayMap
  (-prepare [this] (feature/update-geometry this -prepare))
  (-unprepare [this] (feature/update-geometry this -unprepare))
  (-contains?
    ([this] (-contains? (feature/to-jts this)))
    ([this feat] (-contains? (feature/to-jts this) feat)))
  (-contains-properly?
    ([this] (-contains-properly? (feature/to-jts this)))
    ([this feat] (-contains-properly? (feature/to-jts this) feat)))
  (-covered-by?
    ([this] (-covered-by? (feature/to-jts this)))
    ([this feat] (-covered-by? (feature/to-jts this) feat)))
  (-covers?
    ([this] (-covers? (feature/to-jts this)))
    ([this feat] (-covers? (feature/to-jts this) feat)))
  (-crosses?
    ([this] (-crosses? (feature/to-jts this)))
    ([this feat] (-crosses? (feature/to-jts this) feat)))
  (-disjoint?
    ([this] (-disjoint? (feature/to-jts this)))
    ([this feat] (-disjoint? (feature/to-jts this) feat)))
  (-intersects?
    ([this] (-intersects? (feature/to-jts this)))
    ([this feat] (-intersects? (feature/to-jts this) feat)))
  (-overlaps?
    ([this] (-overlaps? (feature/to-jts this)))
    ([this feat] (-overlaps? (feature/to-jts this) feat)))
  (-touches?
    ([this] (-touches? (feature/to-jts this)))
    ([this feat] (-touches? (feature/to-jts this) feat)))
  (-within?
    ([this] (-within? (feature/to-jts this)))
    ([this feat] (-within? (feature/to-jts this) feat)))

  clojure.lang.PersistentHashMap
  (-prepare [this] (feature/update-geometry this -prepare))
  (-unprepare [this] (feature/update-geometry this -unprepare))
  (-contains?
    ([this] (-contains? (feature/to-jts this)))
    ([this feat] (-contains? (feature/to-jts this) feat)))
  (-contains-properly?
    ([this] (-contains-properly? (feature/to-jts this)))
    ([this feat] (-contains-properly? (feature/to-jts this) feat)))
  (-covered-by?
    ([this] (-covered-by? (feature/to-jts this)))
    ([this feat] (-covered-by? (feature/to-jts this) feat)))
  (-covers?
    ([this] (-covers? (feature/to-jts this)))
    ([this feat] (-covers? (feature/to-jts this) feat)))
  (-crosses?
    ([this] (-crosses? (feature/to-jts this)))
    ([this feat] (-crosses? (feature/to-jts this) feat)))
  (-disjoint?
    ([this] (-disjoint? (feature/to-jts this)))
    ([this feat] (-disjoint? (feature/to-jts this) feat)))
  (-intersects?
    ([this] (-intersects? (feature/to-jts this)))
    ([this feat] (-intersects? (feature/to-jts this) feat)))
  (-overlaps?
    ([this] (-overlaps? (feature/to-jts this)))
    ([this feat] (-overlaps? (feature/to-jts this) feat)))
  (-touches?
    ([this] (-touches? (feature/to-jts this)))
    ([this feat] (-touches? (feature/to-jts this) feat)))
  (-within?
    ([this] (-within? (feature/to-jts this)))
    ([this feat] (-within? (feature/to-jts this) feat))))

(extend-protocol Shapelike
  PreparedGeometry
  (to-shape [this] (spatial/to-shape (-unprepare this)))
  (to-jts
    ([this] this)
    ([this srid] (-prepare (geo.jts/transform-geom (-unprepare this) srid)))
    ([this c1 c2] (-prepare (geo.jts/transform-geom (-unprepare this) c1 c2)))
    ([this c1 c2 geometry-factory]
     (-prepare
      (geo.jts/transform-geom (-unprepare this)
                              c1 c2 geometry-factory)))))

(extend-protocol Featurelike
  PreparedGeometry
  (-to-shape [this] (-to-shape this))
  (-to-jts
    ([this] this)
    ([this srid] (-to-jts this srid))
    ([this c1 c2] (-to-jts this c1 c2))
    ([this c1 c2 geometry-factory]
     (-to-jts this c1 c2 geometry-factory)))
  (-to-feature
    ([this] (-to-feature this {}))
    ([this properties] {:geometry this :properties properties}))
  (-geometry [this] this)
  (-properties [this] {})
  (-assoc-geometry [this s] s)
  (-update-geometry
    ([this f] (f this))
    ([this f & args]
     (-update-geometry
      this (fn [x] (apply f (cons x args))))))
  (-assoc-properties [this p] (feature/to-feature this p))
  (-update-properties [this f] (-update-properties (-to-feature this) f)))

;; Getting to and from PreparedGeometries

(defn prepare
  "When called with a PreparedGeometry-based Featurelike, returns that
  same Featurelike. When called with a non-PreparedGeometry-based
  Featurelike, coerces its geometry to a PreparedGeometry."
  [feat]
  (-prepare feat))

(defn unprepare
  "When called with a PreparedGeometry-based Featurelike, coerces its
  geometry to its corresponding Geometry. When called with a
  non-PreparedGeometry-based Featurelike, returns that same
  Featurelike."
  [feat]
  (-unprepare feat))

(defn ^Geometry unprepare-jts
  "Returns an unprepared JTS geometry of a Featurelike."
  [feat]
  (unprepare (-to-jts feat)))

(defn prepared-feature?
  "Returns true if geometry of Featurelike is PreparedGeometry."
  [feat]
  (instance? PreparedGeometry (feature/geometry feat)))

;; Preparable predicates

(defn jts-contains?
  "A predicate to test for JTS contains. Called 'jts-contains' instead of
  'contains' due to clojure.core/contains? naming collision.
  When called with a single Featurelike, coerces that Featurelike into
  a PreparedGeometry, and returns a 1-arity function that will test
  against another Featurelike."
  ([feature]
   (-contains? feature))
  ([feature-1 feature-2]
   (-contains? feature-1 feature-2)))

(defn contains-properly?
  "A predicate to test for JTS containsProperly.
  When called with a single Featurelike, coerces that Featurelike into
  a PreparedGeometry, and returns a 1-arity function that will test
  against another Featurelike."
  ([feature]
   (-contains-properly? feature))
  ([feature-1 feature-2]
   (-contains-properly? feature-1 feature-2)))

(defn covered-by?
  "A predicate to test for JTS coveredBy.
  When called with a single Featurelike, coerces that Featurelike into
  a PreparedGeometry, and returns a 1-arity function that will test
  against another Featurelike."
  ([feature]
   (-covered-by? feature))
  ([feature-1 feature-2]
   (-covered-by? feature-1 feature-2)))

(defn covers?
  "A predicate to test for JTS covers.
  When called with a single Featurelike, coerces that Featurelike into
  a PreparedGeometry, and returns a 1-arity function that will test
  against another Featurelike."
  ([feature]
   (-covers? feature))
  ([feature-1 feature-2]
   (-covers? feature-1 feature-2)))

(defn crosses?
  "A predicate to test for JTS crosses.
  When called with a single Featurelike, coerces that Featurelike into
  a PreparedGeometry, and returns a 1-arity function that will test
  against another Featurelike."
  ([feature]
   (-crosses? feature))
  ([feature-1 feature-2]
   (-crosses? feature-1 feature-2)))

(defn disjoint?
  "A predicate to test for JTS disjoint.
  When called with a single Featurelike, coerces that Featurelike into
  a PreparedGeometry, and returns a 1-arity function that will test
  against another Featurelike."
  ([feature]
   (-disjoint? feature))
  ([feature-1 feature-2]
   (-disjoint? feature-1 feature-2)))

(defn intersects?
  "A predicate to test for JTS intersects.
  When called with a single Featurelike, coerces that Featurelike into
  a PreparedGeometry, and returns a 1-arity function that will test
  against another Featurelike."
  ([feature]
   (-intersects? feature))
  ([feature-1 feature-2]
   (-intersects? feature-1 feature-2)))

(defn overlaps?
  "A predicate to test for JTS overlaps.
  When called with a single Featurelike, coerces that Featurelike into
  a PreparedGeometry, and returns a 1-arity function that will test
  against another Featurelike."
  ([feature]
   (-overlaps? feature))
  ([feature-1 feature-2]
   (-overlaps? feature-1 feature-2)))

(defn touches?
  "A predicate to test for JTS touches.
  When called with a single Featurelike, coerces that Featurelike into
  a PreparedGeometry, and returns a 1-arity function that will test
  against another Featurelike."
  ([feature]
   (-touches? feature))
  ([feature-1 feature-2]
   (-touches? feature-1 feature-2)))

(defn within?
  "A predicate to test for JTS within.
  When called with a single Featurelike, coerces that Featurelike into
  a PreparedGeometry, and returns a 1-arity function that will test
  against another Featurelike."
  ([feature]
   (-within? feature))
  ([feature-1 feature-2]
   (-within? feature-1 feature-2)))

;; Internal wrapper functions for convenience

(defn wfg
  "Wrap a function that operates on a Shapelike so that it can operate
  on a Featurelike."
  [feat f & args]
  (feature/update-geometry feat #(apply f (cons % args))))

(defn wj
  "Wrap a function that operates on a Geometry so that it can operate
  on a Featurelike"
  [feat f & args]
  (apply wfg (cons (unprepare (feature/jts-geometry feat)) (cons f args))))

;; Additional predicates

(defn equals-topo?
  [feature-1 feature-2]
  (.equalsTopo (unprepare-jts feature-1)
               (unprepare-jts feature-2)))

;; Return one modified feature by comparing two features

(defn- difference-jts
  [^Geometry g1 ^Geometry g2]
  (.difference g1 g2))

(defn- difference-unoptimized
  [feature-1 feature-2]
  (wj feature-1
      difference-jts
      (unprepare-jts feature-2)))

(defn difference
  ([feat]
   (fn [f] (difference (-prepare feat) f)))
  ([feature-1 feature-2]
   (cond (or (contains-properly? feature-1 feature-2)
             (intersects? feature-1 feature-2))
         (difference-unoptimized feature-1 feature-2)
         :else
         feature-1)))

(defn- intersection-jts
  [^Geometry g1 ^Geometry g2]
  (.intersection g1 g2))

(defn- intersection-unoptimized
  [feature-1 feature-2]
  (wj feature-1
      intersection-jts
      (unprepare-jts feature-2)))

(defn intersection
  ([feat]
   (fn [f] (intersection (prepare feat) f)))
  ([feature-1 feature-2]
   (cond (contains-properly? feature-1 feature-2)
         (feature/assoc-geometry feature-1 (feature/geometry feature-2))
         (intersects? feature-1 feature-2)
         (intersection-unoptimized feature-1 feature-2)
         :else
         (feature/assoc-geometry feature-1 nil))))

;; Values

(defn get-area
  [feature]
  (.getArea (unprepare-jts feature)))

(defn get-length
  [feature]
  (.getLength (unprepare-jts feature)))

(defn distance
  [feature-1 feature-2]
  (.distance (unprepare-jts feature-1)
             (unprepare-jts feature-2)))

;; Return modified single feature

(defn- get-boundary-jts
  [^Geometry g]
  (.getBoundary g))

(defn get-boundary
  [feature]
  (wj feature get-boundary-jts))

(defn centroid
  [feature]
  (wj feature geo.jts/centroid))

(defn- buffer-jts
  ([^Geometry geom dist]
   (.buffer geom (double dist)))
  ([^Geometry geom dist ^BufferParameters params]
   (BufferOp/bufferOp geom (double dist) params)))

(defn buffer
  ([feature dist]
   (wj feature buffer-jts dist))
  ([feature dist params]
   (wj feature buffer-jts dist params)))

(defn single-sided-params
  []
  (doto (BufferParameters.)
    (.setSingleSided true)))

(defn one-sided-buffer
  [feature dist]
  (buffer feature dist (single-sided-params)))

(defn left-buffer
  [feature dist]
  (one-sided-buffer feature (math/abs dist)))

(defn right-buffer
  [feature dist]
  (one-sided-buffer feature (- (math/abs dist))))

(defn- get-minimum-area-rectangle-jts
  [^Geometry g]
  (MinimumAreaRectangle/getMinimumRectangle g))

(defn get-minimum-area-rectangle
  [feat]
  (wj feat get-minimum-area-rectangle-jts))

(defn- get-minimum-width-rectangle-jts
  [^Geometry g]
  (MinimumDiameter/getMinimumRectangle g))

(defn get-minimum-width-rectangle
  [feat]
  (wj feat get-minimum-width-rectangle-jts))


;; Offset either a polygon or a line

(def ^OffsetCurveBuilder ocb
  (OffsetCurveBuilder. (PrecisionModel.) (BufferParameters.)))

(defn ^"[Lorg.locationtech.jts.geom.Coordinate;" get-offset-curve
  [l distance]
  (.getOffsetCurve
   ocb
   (.getCoordinates ^Geometry (feature/geometry l)) distance))

(defn offset-line-fn
  [l distance]
  (.createLineString ^GeometryFactory (geo.crs/get-geometry-factory l)
                     (get-offset-curve l distance)))

(defn offset-polygon-fn
  [l distance]
  (.createPolygon
           (geo.crs/get-geometry-factory l)
           (geo.jts/coordinate-sequence
            (into
             []
             (conj (into [] (concat (.getCoordinates
                                     ^Geometry (feature/geometry l))
                                    (reverse (get-offset-curve l distance))))
                   (first (.getCoordinates
                           ^Geometry (feature/geometry l))))))))


(defn offset-line-string
  "geom-type can be :polygon or :line.
  A :line type returns a LineString offset by distance.
  A :polygon type returns a Polygon made up of the original
  line connected to the offset LineString in a closed ring."
  [l distance geom-type]
  (try (case geom-type
           :polygon (offset-polygon-fn l distance)
           :line (offset-line-fn l distance))
      (catch Exception _ nil)))


;; Return one modified geometry by comparing many features

(defn union
  [features]
  (CascadedPolygonUnion/union
   (into [] (map feature/to-jts) features)))

