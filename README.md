<!-- README.md is generated from README.Rmd. Please edit that file -->
osmbus
======

osmbus is an R package which exports [OpenStreetMap](https://www.openstreetmap.org) bus lines to GPX.

Example
=======

``` r
library(osmbus)
write_gpx(id_rel = "3201308") # get the data online
write_gpx(id_rel = "bus1.osm") # local file (contains full data about the relation)
```
