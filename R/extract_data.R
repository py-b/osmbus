#' Download data and procces it to be exported
#'
#' @param id_rel the identifier of the OpenStreetMap relation.
#' @param overpass_url instance of the Overpass API to use to retrieve data.
#' @param quiet `TRUE` to desactivate information messages.
#'
#' @importFrom dplyr  %>%
#' @importFrom dplyr  mutate_at
#' @importFrom dplyr  left_join
#' @importFrom xml2   read_xml
#' @importFrom xml2   xml_find_all
#' @importFrom xml2   xml_attrs
#' @importFrom xml2   xml_find_first
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom purrr  map_chr
#' @importFrom purrr  map
#'
#' @export

extract_data <- function(id_rel,
                         overpass_url = "http://overpass-api.de/api/interpreter",
                         quiet = FALSE) {

  ## Télécharge données ##

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

  if (!quiet) message("Downloading data from OpenStreetMap")
  dest <- file.path(tempdir(), paste0(id_rel, ".xml"))
  utils::download.file(
    url = download_url,
    destfile = dest,
    quiet = TRUE
  )

  full <- read_xml(dest)
  osm_nodes <- xml_find_all(full, ".//node")

  # verif présence data :
  if (!length(osm_nodes)) stop("no data found for this relation.")

  ## Géométrie ##

  # coordonnées et version de tous les noeuds du fichier full (data_frame)
  coord_nd <-
    do.call(
      rbind,
      xml_attrs(osm_nodes)
    ) %>%
    as_tibble() %>%
    subset(select = c("id", "lat", "lon", "version")) %>%
    mutate_at(
      c("lat", "lon"),
      as.numeric
    )

  # ways constituant le tracé (vecteur)
  id_way_trace <-
    full %>%
    xml_find_all(".//relation/member[@role='']") %>%
    xml_attrs() %>%
    map_chr("ref")

  # noeuds constituant le tracé ordonnés + sans doublon (vecteur)
  trace <-
    map(id_way_trace, list_nd, full) %>%
    merge_all_ways()

  # coord des noeuds constituant le tracé (data_frame)
  trkpt_base <- left_join(
    tibble(id = trace),
    coord_nd,
    by = "id"
  )

  # stop de la relation (vecteur)
  stop_id <-
    full %>%
    xml_find_all(".//relation/member[@role='stop']") %>%
    xml_attrs() %>%
    map_chr("ref")

  # nom des stop (vecteur)
  stop_names <- map_chr(stop_id, nd_name, full)

  # coord des stop (data_frame)
  stop_base <- left_join(
    tibble(
      id = stop_id,
      name = stop_names
    ),
    coord_nd,
    by = "id"
  )

  ## Meta données ##

  # nombre de stop, nombre de points du tracé
  stop_count <- nrow(stop_base)
  trkpt_count <- nrow(trkpt_base)

  #bbox
  bounds <- c(
    minlat = min(coord_nd$lat),
    minlon = min(coord_nd$lon),
    maxlat = max(coord_nd$lat),
    maxlon = max(coord_nd$lon)
  )

  # attributs de la relation
  rel_attr <- full %>% xml_find_first(".//relation") %>% xml_attrs()

  # tags de la relation
  liste_tags <-
    full %>%
    xml_find_all(".//relation/tag") %>%
    xml_attrs()
  rel_tags <- stats::setNames(
    liste_tags %>% map_chr("v"),
    liste_tags %>% map_chr("k")
  )
  # remplace : par _ (namespace gpx), solution provisoire
  names(rel_tags) <- gsub(":", "_", names(rel_tags))

  # distance au point précédent
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
   # méta données
    bounds = bounds,
    rel_tags = rel_tags,
    rel_attr = rel_attr,
    stop_count = stop_count,
    trkpt_count = trkpt_count,
    trk_km = trk_km,
   # géométrie
    stop_base = stop_base,
    trkpt_base = trkpt_base
  )

}


#### Fonctions auxiliaires ####

#' @importFrom dplyr %>%
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attrs
#' @importFrom xml2 xml_find_first

# liste des noeuds constituant un way (x=fichier xml full)
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

# name d'un noeud nd (x=fichier xml full)
nd_name <- function(nd, x) {
  x %>%
    xml_find_first(sprintf(".//node[@id='%s']", nd)) %>%
    xml_find_first(".//tag[@k='name']") %>%
    xml_attr("v")
}
