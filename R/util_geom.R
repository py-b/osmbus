#### FONCTIONS ####

distance_m <- function(lon1, lat1, lon2, lat2) {
  #distance en m entre deux points de coordonnées en WGS84
  earth_radius <- 6378137 #6378km
  lon1_rad <- lon1 * pi / 180
  lat1_rad <- lat1 * pi / 180
  lon2_rad <- lon2 * pi / 180
  lat2_rad <- lat2 * pi / 180
  acos(
    sin(lat1_rad) * sin(lat2_rad) +
      cos(lat1_rad) * cos(lat2_rad) * cos(lon1_rad - lon2_rad)
  ) * earth_radius
}

circ <- function(v1) v1[1] == utils::tail(v1, 1)

rotate_circ <- function(v, offset) {
  if (!circ(v)) stop("Input should be circular")
  v1 <- v[-1]
  n <- length(v1)
  new_order <- (1:n + (offset - 1)) %% n
  new_order[new_order == 0] <- n
  c(v1[new_order], v1[new_order][1])
}

rm_following_double <- function(v) {
  garde <- v[-1] != v[-length(v)]
  v[c(garde, TRUE)]
}
# rm_following_double(1:10)
# rm_following_double(sample(1:4, 15, replace = TRUE))
# rm_following_double(c(1,1,2,3,3,4,5,5))
# rm_following_double(c(1:5, 5:0, 0:3, 3:-1))
# rm_following_double(rep(c(1:5, 5:1), each = 3))

merge_ways <- function(v1,
                       v2,
                       rm.double = TRUE) {

  if (length(v1) == 0 | length(v2) == 0) return(character(0))

  h1 <- v1[1]
  h2 <- v2[1]
  t1 <- utils::tail(v1, 1)
  t2 <- utils::tail(v2, 1)

  if (t1 == h2) res <- c(v1, v2)
  else if (t1 == t2) res <- c(v1,  rev(v2))
  else if (h1 == h2) res <- c(rev(v1), v2)
  else if (h1 == t2) res <- c(rev(v1), rev(v2))
  else return(character(0))

  if (rm.double) res <- rm_following_double(res)

  res

}


# portion_gir ------------------------------------------------------------

portion_gir <- function(way_in,
                        gir,
                        way_out = way_in) {

  node_in <- intersect(way_in, gir)
  node_out <- intersect(way_out, gir)

  # rotation gir pour que le premier point soit l'entree
  if (gir[1] != node_in) {
    gir <- rotate_circ(gir, which(gir == node_in) - 1)
  }

  # portion du giratoire à traverser
  debut <- which(gir == node_in)
  fin <-  which(gir == node_out)
  gir[min(debut):max(fin)]

}

# portion_gir(
#   c("2", "4"),
#   c("3", "2", "5", "1", "3"),
#   c("1", "6")
# )
#
#   #            < *3* <
#   #          /        \
#   #         /          \
#   #  4 -<- 2            1 ->- 6
#   #         \          /
#   #          \        /
#   #           -> 5 ->
#
# portion_gir(
#   c("2", "4"),
#   c("2", "5", "1", "3", "2"),
#   c("1", "6")
# )
#
#   #            -- 3 --
#   #          /        \
#   #         /          \
#   # 4 --- *2*           1 ---- 6
#   #         \          /
#   #          \        /
#   #           -- 5 --
#
# portion_gir(
#   c("2", "4"),
#   c("1", "3", "2", "5", "1"),
#   c("1", "6")
# )
#
#   #            -- 3 --
#   #          /        \
#   #         /          \
#   # 4 ---- 2            *1* ---- 6
#   #         \          /
#   #          \        /
#   #           -- 5 --
#
# portion_gir(
#   c("2", "4"),
#   c("5", "1", "3", "2", "5"),
#   c("1", "6")
# )
#
# # ressort par l'entree
# portion_gir(
#   c("2", "4"),
#   c("3", "2", "5", "1", "3")
# )
#
# portion_gir(
#   c("1", "6"),
#   c("5", "1", "3", "2", "5")
# )

# merge 2 ----------------
# incluant traversee giratoire complet

merge_all_ways <- function(ways) {

  stopifnot(!circ(ways[[1]]) & !circ(ways[[length(ways)]]))

  # calcule portion de giratoire empruntés
  pos_giratoires <- which(sapply(ways, circ))
  for (i in pos_giratoires) {
    ways[[i]] <-
      portion_gir(
        ways[[i - 1]],
        ways[[i]],
        ways[[i + 1]]
      )
  }

  Reduce(merge_ways, ways)

}
