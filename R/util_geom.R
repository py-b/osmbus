#' @importFrom utils head tail

distance_m <- function(lon1, lat1, lon2, lat2) {
  #distance en m entre deux points de coordonnées en WGS84
  earth_radius <- 6371230 # 6371 km
  lon1_rad <- lon1 * pi / 180
  lat1_rad <- lat1 * pi / 180
  lon2_rad <- lon2 * pi / 180
  lat2_rad <- lat2 * pi / 180
  acos(
    sin(lat1_rad) * sin(lat2_rad) +
      cos(lat1_rad) * cos(lat2_rad) * cos(lon1_rad - lon2_rad)
  ) * earth_radius
}

circ <- function(way) head(way, 1) == tail(way, 1)

rotate_circ <- function(way, offset) {
  if (!circ(way)) stop("Input should be circular")
  v1 <- way[-1]
  n <- length(v1)
  new_order <- (1:n + (offset - 1)) %% n
  new_order[new_order == 0] <- n
  c(v1[new_order], v1[new_order][1])
}

rm_following_double <- function(v) {
  garde <- v[-1] != v[-length(v)]
  v[c(garde, TRUE)]
}

merge_2_ways <- function(way1,
                         way2,
                         rm.double = TRUE) {

  if (!length(way1) || !length(way2)) stop("empty way")

  h1 <- head(way1, 1)
  h2 <- head(way2, 1)
  t1 <- tail(way1, 1)
  t2 <- tail(way2, 1)

  if      (t1 == h2) res <- c(way1, way2)
  else if (t1 == t2) res <- c(way1,  rev(way2))
  else if (h1 == h2) res <- c(rev(way1), way2)
  else if (h1 == t2) res <- c(rev(way1), rev(way2))
  else stop("ways not connected by their ends")

  if (rm.double) res <- rm_following_double(res)

  res

}

roundabout_part <- function(way_in,
                            roundabout,
                            way_out = way_in) {

  stopifnot(
    circ(roundabout),
    is.character(way_in),
    is.character(roundabout),
    is.character(way_out)
  )

  node_in  <- intersect(way_in,  roundabout)
  node_out <- intersect(way_out, roundabout)

  stopifnot(
    length(node_in) == 1,
    length(node_out) == 1
  )

  # rotate roundabout so that first node is entrance
  if (roundabout[1] != node_in) {
    offset <- which(roundabout == node_in) - 1
    roundabout <- rotate_circ(roundabout, offset)
  }

  # part of roundabout to use
  debut <- which(roundabout == node_in)
  fin <-  which(roundabout == node_out)
  roundabout[min(debut):max(fin)]

}

merge_ways <- function(ways) {

  if (circ(ways[[1]]) || circ(ways[[length(ways)]])) {
    stop("incorrect data, first or last way cannot be circular")
  }

  # calcule portion de giratoire empruntés
  pos_roundabouts <- which(sapply(ways, circ))
  for (i in pos_roundabouts) {
    ways[[i]] <-
      roundabout_part(
        ways[[i - 1]],
        ways[[i]],
        ways[[i + 1]]
      )
  }

  Reduce(merge_2_ways, ways)

}
