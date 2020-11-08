#### ECRITURE GPX ####

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
                      overpass_url = getOption("osmbus.overpass_url"),
                      quiet = FALSE) {

  ## Extraction données ##

  if (is.null(getOption("osmbus.overpass_url"))) {
    overpass_url <- "http://overpass-api.de/api/"
  }

  if (!quiet) cat("Relation", id_rel)
  data_list <-
    extract_full(
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

  if (!quiet) cat(",", rel_tags["name"])

  ## Construction xml ##

  gpx <- xml_new_document(version = "1.0") %>% xml_add_child("gpx")

  # metadata

  gpx %>%
    xml_add_child("metadata") %>%
    xml_add_child(
      .value = "osm_relation",
      id = rel_attr["id"],
      version = rel_attr["version"],
      timestamp = rel_attr["timestamp"]
    ) %>%
    invisible()

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
    ) %>%
    invisible()

  # wpt

  for (i in 1:data_list$stop_count) {
    gpx %>%
      xml_add_child(
        .value = "wpt",
        lat = paste(wpt_base$lat[i]),
        lon = paste(wpt_base$lon[i])
      ) %>%
      xml_add_child(.value = "name", wpt_base$name[i])
  }


  # trk

  gpx %>%
    xml_add_child("trk") %>%
    xml_add_child("trkseg") %>%
    invisible()

  for (i in 1:data_list$trkpt_count) {
    gpx %>%
    xml_find_all(".//trkseg") %>%
    xml_add_child(
      "trkpt",
      lat = paste(trkpt_base$lat[i]),
      lon = paste(trkpt_base$lon[i]),
      osm_node = trkpt_base$id[i],
      version = trkpt_base$version[i]
    )
  }

  ## Écriture disque ##

  dir.exists(path) || dir.create(path)
  filename <- paste0(rel_tags["route"], "-", rel_tags["ref"], "_", rel_attr["id"], ".gpx")
  write_xml(
    gpx,
    file.path(path, filename),
    fileEncoding = "UTF-8"
  )

  if (!quiet) cat(" [Done]\n")

}
