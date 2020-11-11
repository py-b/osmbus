#' Export bus line to GPX
#'
#' @inheritParams extract_data
#' @param path the directory where to write the GPX file.
#' @param filename name of the file. If `NULL` (default), a name based on the
#'   tags `route`, `ref` and `id` of the OSM relation.
#' @param osm_info include informations about OpenStreetMap nodes (id and
#'   version).
#'
#' @return The data used to export the file, invisibly. This is the same as the
#'   result of [`extract_data`].
#'
#' @importFrom dplyr %>%
#' @importFrom xml2 xml_new_document
#' @importFrom xml2 xml_add_child
#' @importFrom xml2 xml_find_first
#' @importFrom xml2 xml_add_sibling
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 write_xml
#'
#' @export

write_gpx <- function(id_rel,
                      path = ".",
                      filename = NULL,
                      osm_info = TRUE,
                      overpass_url = "http://overpass-api.de/api/interpreter",
                      quiet = FALSE) {

  ## Extract data ##

  data_list <-
    extract_data(
      id_rel,
      overpass_url = overpass_url,
      quiet = quiet
    )

  minlat <- as.character(data_list$bounds["minlat"])
  minlon <- as.character(data_list$bounds["minlon"])
  maxlat <- as.character(data_list$bounds["maxlat"])
  maxlon <- as.character(data_list$bounds["maxlon"])

  rel_attr <- data_list$rel_attr
  rel_tags <- data_list$rel_tags

  wpt_base <- data_list$stop_base
  trkpt_base <- data_list$trkpt_base

  ## Generate xml ##

  gpx <- xml_new_document(version = "1.0") %>% xml_add_child("gpx")

  # metadata

  gpx %>%
    xml_add_child("metadata") %>%
    xml_add_child(
      .value = "osm_relation",
      id = rel_attr["id"],
      version = rel_attr["version"],
      timestamp = rel_attr["timestamp"]
    )

  for (tag in names(rel_tags)) {
    gpx %>%
      xml_find_first(".//osm_relation") %>%
      xml_add_child(.value = tag, rel_tags[tag])
  }

  gpx %>%
    xml_find_first(".//metadata") %>%
    xml_add_child(
        .value = "bounds",
        minlat = minlat,
        minlon = minlon,
        maxlat = maxlat,
        maxlon = maxlon
    ) %>%
    xml_add_sibling(.value = "wpt_count", data_list$stop_count) %>%
    xml_add_sibling(.value = "trkpt_count", data_list$trkpt_count) %>%
    xml_add_sibling(.value = "trk_km", data_list$trk_km) %>%
    xml_add_sibling(
      .value = "copyright", "OpenStreetMap contributors",
      attribution = "http://www.openstreetmap.org/copyright",
      license = "http://opendatacommons.org/licenses/odbl/1-0/"
    )

  # wpt

  for (i in 1:data_list$stop_count) {

    params_add_wpt <-
      list(
        .x     = gpx,
        .value = "wpt",
        lat    = as.character(wpt_base$lat[i]),
        lon    = as.character(wpt_base$lon[i])
      )

    if (osm_info) {
      params_add_wpt$osm_node <- wpt_base$id[i]
      params_add_wpt$version  <- wpt_base$version[i]
    }

    do.call(xml_add_child, params_add_wpt) %>%
    xml_add_child(
      .value = "name",
      wpt_base$name[i]
    )

  }

  # trk

  gpx %>%
    xml_add_child("trk") %>%
    xml_add_child("trkseg")

  for (i in 1:data_list$trkpt_count) {

    params_add_trkpt <-
      list(
        .x     = xml_find_first(gpx, ".//trkseg"),
        .value = "trkpt",
        lat    = as.character(trkpt_base$lat[i]),
        lon    = as.character(trkpt_base$lon[i])
      )

    if (osm_info) {
      params_add_trkpt$osm_node <- trkpt_base$id[i]
      params_add_trkpt$version  <- trkpt_base$version[i]
    }

    do.call(xml_add_child, params_add_trkpt)

  }

  ## Write disk ##

  dir.exists(path) || dir.create(path)
  if (is.null(filename)) {
    filename <- paste0(rel_tags["route"], "-", rel_tags["ref"], "_", rel_attr["id"], ".gpx")
  }
  write_xml(
    gpx,
    file.path(path, filename),
    fileEncoding = "UTF-8"
  )

  if (!quiet) message("  File written to disk : ", shQuote(filename))

  invisible(data_list)

}
