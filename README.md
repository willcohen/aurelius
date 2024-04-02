# aurelius

[![Clojars Project](https://img.shields.io/clojars/v/aurelius.svg)](https://clojars.org/aurelius)

Meditations on geospatial analysis

This library seeks to enable transducible analysis of geospatial features.

## Usage

Pre-alpha work in progress.

## Development

To be able to generate the internal sqlite, use the `dev` profile:

``` shell
clj -A:dev
```


To update the sqlite, from the dev `aurelius.resources` namespace, run these commands from the REPL:

``` clojure
(delete-sqlite-resource-if-exists) ; to delete a previous sqlite
(create-sqlite-resource) ; to create a new empty sqlite
(add-qcew-luts aurelius.db/adb) ; to add QCEW lookup tables
(add-fips-areas aurelius.db/adb) ; to add Census FIPS areas
```

## Implemented

### aurelius.census

* Conversion of QCEW quarterly results into monthly rows for analysis

### aurelius.conversions

* Common units of conversion

### aurelius.db

* Automatic conversion of PostGIS geometries into JTS objects
* Database component to start and stop connections to PostGIS
* Connection to internal sqlite database

### aurelius.jts

* Partial implementation of common JTS spatial operations that work on
`Featurelike`s from [ovid](https://github.com/willcohen/ovid).
* Use of `PreparedGeometry` to improve speed of JTS operations.

### aurelius.resources (dev)

* Import of QCEW and Census FIPS definitions into an internal sqlite database.

### aurelius.sql

* Management of database tables
* Import and queries for census tables

### aurelius.util

* Formatting of java.time objects
* Helpers for file input/output
* Use of JDK 11 HTTP client
* Read of CSVs
* Read of XLS and XLSX spreadsheets

## Requirements

Requires JDK 11 or higher.

## License

```
Copyright (c) 2019, 2020, 2024 Will Cohen

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```
