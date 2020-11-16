#' Get the data about an OSM transport line into R
#'
#' Fetch data on OpenStreetMap server, processes it and stores it into a list.
#'
#' @param id_rel the identifier of the OpenStreetMap relation.
#' @param overpass_url instance of the Overpass API to use to retrieve data (see
#'   [this page](https://wiki.openstreetmap.org/wiki/Overpass_API#Public_Overpass_API_instances)
#'   for a list of available instances).
#' @param quiet `TRUE` to desactivate information messages.
#'
#' @return A list containing the following elements :
#'   - `bounds` : the bounding box of the data ;
#'   - `rel_tags` : the tags of the relation in the OpenStreetMap database ;
#'   - `rel_attr` : metadata of the relation, such as id, version, date of
#'     last modification... ;
#'   - `stop_count` : number of stops ;
#'   - `trkpt_count`: numbre of points of the track ;
#'   - `trk_km` : length of the track, in kilometers ;
#'   - `stop_base` : data.frame with informations about stops ;
#'   - `trkpt_base` : data.frame with informations about trackpoints (in
#'      particular the distance between too consecutive points, in meters).
#'
#' @section Details:
#' The data coming from the OSM server must be clean for the processing to
#' succeed.
#'
#' As stated in the OSM documentation, the ways in the relation should be listed
#' beginning with the way at the initial stop position and ending with the way
#' at the terminal stop, in the right order. The package will automatically
#' reverse some ways to produce a continuous track.
#'
#' If roundabouts (or any circular ways) are part of the track, osmbus will find
#' its way through them, i.e. it will select the only nodes used by the transport
#' vehicle from entrance to exit.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' extract_data(id_rel = "123767")
#' }

extract_data <- function(id_rel,
                         overpass_url = "http://overpass-api.de/api/interpreter",
                         quiet = FALSE) {

  if (length(id_rel) != 1) stop("provide only one `id_rel`")
  if (!grepl("^\\d+$", id_rel)) stop("`id_rel` must contain only digits")

  ## Download data ##

  overpass_query <- sprintf(
    "[out:xml]; (relation(id:%s);); (._;>;); out meta;",
    id_rel
  )

  download_url <- paste0(
    overpass_url,
    "?data=",
    utils::URLencode(overpass_query, reserved = TRUE),
    "&target=compact"
  )

  if (!quiet) message("Downloading relation <", id_rel, "> from OpenStreetMap")
  tmp_xml <- file.path(tempdir(), paste0("osmbus-", id_rel, ".xml"))
  utils::download.file(
    url = download_url,
    destfile = tmp_xml,
    quiet = TRUE
  )

  ## Process data ##

  extract_file_data(tmp_xml, quiet = quiet)

}


#### Auxiliary functions ####

#' Extract xml data from a file on disk
#'
#' @param xml_file xml containing data (fetched from OSM).
#' @inheritParams extract_data
#'
#' @return A list containing the following elements :
#'   - `bounds` : the bounding box of the data ;
#'   - `rel_tags` : the tags of the relation in the OpenStreetMap database ;
#'   - `rel_attr` : metadata of the relation, such as id, version, date of
#'     last modification... ;
#'   - `stop_count` : number of stops ;
#'   - `trkpt_count`: numbre of points of the track ;
#'   - `trk_km` : length of the track, in kilometers ;
#'   - `stop_base` : data.frame with informations about stops ;
#'   - `trkpt_base` : data.frame with informations about trackpoints (in
#'      particular the distance between too consecutive points, in meters)
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr left_join
#' @importFrom dplyr tibble as_tibble
#' @importFrom xml2  read_xml
#' @importFrom xml2  xml_find_all
#' @importFrom xml2  xml_attrs
#' @importFrom xml2  xml_find_first
#' @importFrom purrr map_chr
#' @importFrom purrr map
#'
#' @keywords internal

extract_file_data <- function(xml_file, quiet = FALSE) {

  full <- read_xml(xml_file)
  osm_nodes <- xml_find_all(full, ".//node")

  # check there is data
  if (!length(osm_nodes)) stop("relation not found or empty.")

  # relation attributes
  rel_attr <- full %>% xml_find_first(".//relation") %>% xml_attrs()

  # relation tags
  liste_tags <-
    full %>%
    xml_find_all(".//relation/tag") %>%
    xml_attrs()
  rel_tags <- stats::setNames(
    liste_tags %>% map_chr("v"),
    liste_tags %>% map_chr("k")
  )

  # check relation tags
  if (is.na(rel_tags["type"]) || rel_tags["type"] != "route") {
    stop("the relation must be tagged `type=route`.")
  }
  routes <- c("aerialway", "bus", "ferry", "monorail", "subway",
              "train", "tram", "trolleybus")
  if (is.na(rel_tags["route"]) || !rel_tags["route"] %in% routes) {
    stop(
      "the \"route\" tag of the relation must be one of \n  ",
       toString(shQuote(routes))
    )
  }

  if (!quiet) {
    line_name <- rel_tags["name"]
    msg_name <- if (is.na(line_name)) "none" else shQuote(line_name)
    message("  Line name : ", msg_name)
  }

  ## Geometry ##

  # coordinates and version of all downloaded nodes (tibble)
  coord_nd <-
    do.call(
      rbind,
      xml_attrs(osm_nodes)
    ) %>%
    as_tibble() %>%
    subset(select = c("id", "lat", "lon", "version"))

  coord_nd$lat <- as.numeric(coord_nd$lat)
  coord_nd$lon <- as.numeric(coord_nd$lon)

  # ways in the track (character vector)
  id_way_trace <-
    full %>%
    xml_find_all(".//relation/member[@role='']") %>%
    xml_attrs() %>%
    map_chr("ref")

  # ordered and unduplicated track nodes (vector)
  tryCatch(

    trace <-
      map(id_way_trace, list_nd, full) %>%
      merge_ways(),

    error = function(e) {
      stop(
        "probably discontinuous track ? Check OSM data.\n",
        "  [original error message] ", e$message
      )
    }

  )

  # coordinates of nodes of the track (data_frame)
  trkpt_base <- left_join(
    tibble(id = trace),
    coord_nd,
    by = "id"
  )

  # relation stops (vector)
  stop_roles <- c("stop", "stop_entry_only", "stop_exit_only")
  xpath_attr_query <- paste0("@role='", stop_roles, "'", collapse = " or ")
  stop_id <-
    full %>%
    xml_find_all(sprintf(".//relation/member[%s]", xpath_attr_query)) %>%
    xml_attrs() %>%
    map_chr("ref")

  # stop names (vector)
  stop_names <- map_chr(stop_id, nd_name, full)

  # stop coordinates (data_frame)
  stop_base <- left_join(
    tibble(
      id = stop_id,
      name = stop_names
    ),
    coord_nd,
    by = "id"
  )

  ## Metadata ##

  stop_count <- nrow(stop_base)
  trkpt_count <- nrow(trkpt_base)

  # bounding box
  bounds <- c(
    minlat = min(coord_nd$lat),
    minlon = min(coord_nd$lon),
    maxlat = max(coord_nd$lat),
    maxlon = max(coord_nd$lon)
  )

  # replace ":" by "_" (namespace gpx)
  # workaround, a best way would be a use the standalone xml attribute
  names(rel_tags) <- gsub(":", "_", names(rel_tags))

  # distance to previous point
  trkpt_base$d_last <- numeric(trkpt_count)
  for (i in 2:trkpt_count) {
    trkpt_base$d_last[i] <-
      distance_m(
        trkpt_base$lon[i - 1], trkpt_base$lat[i - 1],
        trkpt_base$lon[i], trkpt_base$lat[i]
      )
  }

  trk_km <- round(sum(trkpt_base$d_last) / 1000, 3)

  ## Ouptut ##

  list(
   # metadata
    bounds = bounds,
    rel_tags = rel_tags,
    rel_attr = rel_attr,
    stop_count = stop_count,
    trkpt_count = trkpt_count,
    trk_km = trk_km,
   # geometry
    stop_base = stop_base,
    trkpt_base = trkpt_base
  )

}

#' @importFrom dplyr %>%
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attrs
#' @importFrom xml2 xml_find_first

# node list of a way (x = downloaded xml data)
list_nd <- function(way, x) {
  x %>%
    xml_find_first(sprintf(".//way[@id='%s']", way)) %>%
    xml_find_all(".//nd") %>%
    xml_attrs() %>%
    unlist(use.names = FALSE)
}

#' @importFrom dplyr %>%
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_find_first

# name of a node nd (x = downloaded xml data)
nd_name <- function(nd, x) {
  res <-
    x %>%
    xml_find_first(sprintf(".//node[@id='%s']", nd)) %>%
    xml_find_first(".//tag[@k='name']") %>%
    xml_attr("v")
  if (is.na(res)) res <- ""
  res
}
