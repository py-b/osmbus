# distance_m --------------------------------------------------------------

context(":::distance_m")

test_that("distances", {

  expect_equal(
    distance_m(1, 0, 1, 0),
    0
  )

  # Brest - Strasbourg (901 km)
  expect_equal(
    round(distance_m(-4.486009, 48.390528, 7.750713, 48.584614) / 1000),
    901
  )

  # Louvre Pyramid side (35.4 m)
  expect_equal(
    round(distance_m(2.3357192, 48.8612197, 2.3361712, 48.8611065), 1),
    35.4
  )

})

# circ --------------------------------------------------------------------

context(":::circ")

test_that("circ", {

  expect_true (circ(c("1", "2", "3", "1")))
  expect_false(circ(c("1", "2", "3", "2")))

})

context(":::rotate_circ")

test_that("rotates circular way from n positions", {

  expect_error(
    rotate_circ(c("1", "2", "3", "2"))
  )

  roundabout <- c("1", "2", "3", "1")

  expect_equal(
    rotate_circ(roundabout, 1),
    c("2", "3", "1", "2")
  )

  expect_equal(
    rotate_circ(roundabout, 2),
    c("3", "1", "2", "3")
  )

  expect_equal(
    rotate_circ(roundabout, 3),
    roundabout
  )

})

# rm_following_double -----------------------------------------------------

context(":::rm_following_double")

test_that("removes following doubles", {

  expect_equal(
    rm_following_double(1:10),
    1:10
  )

  expect_equal(
    rm_following_double(c(1, 1, 2, 3, 3, 4, 5, 5)),
    c(1, 2, 3, 4, 5)
  )

  expect_equal(
    rm_following_double(c(1:5, 5:0, 0:3, 3:-1)),
    c(1:5, 4:0, 1:3, 2:-1)
  )

  expect_equal(
    rm_following_double(rep(c(1:5, 5:1), each = 3)),
    c(1:5, 4:1)
  )

})

# merge_2_ways ------------------------------------------------------------

context(":::merge_2_ways")

test_that("merge 2 ways", {

  expect_error(
    merge_2_ways(
      c("1", "2", "3"),
      NULL
    ),
    "empty"
  )

  expect_error(
    merge_2_ways(
      c("1", "2", "3"),
      c("2", "10", "11")
    ),
    "ways not connected by their ends"
  )

  # need 0 reverse
  expect_equal(
    merge_2_ways(
      c("1", "2", "3"),
      c("3", "4", "5", "6")
    ),
    c("1", "2", "3", "4", "5", "6")
  )

  # need 1 reverse (v1)
  expect_equal(
    merge_2_ways(
      c("1", "2", "3"),
      c("6", "5", "4", "3")
    ),
    c("1", "2", "3", "4", "5", "6")
  )

  # need 1 reverse (v2)
  expect_equal(
    merge_2_ways(
      c("3", "2", "1"),
      c("3", "4", "5", "6")
    ),
    c("1", "2", "3", "4", "5", "6")
  )

  # need 2 reverses
  expect_equal(
    merge_2_ways(
      c("3", "2", "1"),
      c("6", "5", "4", "3")
    ),
    c("1", "2", "3", "4", "5", "6")
  )

})

# roundabout_part ---------------------------------------------------------

context(":::roundabout_part")

test_that("simple_case", {

  expect_equal(
    roundabout_part(
      c("2", "4"),
      c("2", "5", "1", "3", "2"),
      c("1", "6")
    ),
    c("2", "5", "1")
  )
      #            -- 3 --
      #          /        \
      #         /          \
      # 4 --- *2*           1 ---- 6
      #         \          /
      #          \        /
      #           -- 5 --
})

test_that("with rotation", {

  expect_equal(
    roundabout_part(
      c("2", "4"),
      c("3", "2", "5", "1", "3"),
      c("1", "6")
    ),
    c("2", "5", "1")
  )
      #            < *3* <
      #          /        \
      #         /          \
      #  4 -<- 2            1 ->- 6
      #         \          /
      #          \        /
      #           -> 5 ->

  expect_equal(
    roundabout_part(
      c("2", "4"),
      c("1", "3", "2", "5", "1"),
      c("1", "6")
    ),
    c("2", "5", "1")
  )
      #            <- 3 --
      #          /        \
      #         /          \
      # 4 ---- 2            *1* ---- 6
      #         \          /
      #          \        /
      #           -- 5 ->
  expect_equal(
    roundabout_part(
      c("2", "4"),
      c("5", "1", "3", "2", "5"),
      c("1", "6")
    ),
    c("2", "5", "1")
  )
      #            <- 3 --
      #          /        \
      #         /          \
      # 4 ---- 2            1 ---- 6
      #         \          /
      #          \        /
      #           --*5*->
})

test_that("entrance same as exit", {

  expect_equal(
    roundabout_part(
      c("2", "4"),
      c("3", "2", "5", "1", "3")
    ),
    c("2", "5", "1", "3", "2")
  )

  expect_equal(
    roundabout_part(
      c("1", "6"),
      c("5", "1", "3", "2", "5")
    ),
    c("1", "3", "2", "5", "1")
  )

})

test_that("anti_clockwise", {

  expect_equal(
    roundabout_part(
      c("2", "4"),
      rev(c("3", "2", "5", "1", "3")),
      c("1", "6")
    ),
    c("2", "3", "1")
  )
      #            > *3* >
      #          /        \
      #         /          \
      #  4 -<- 2            1 ->- 6
      #         \          /
      #          \        /
      #           -< 5 -<

})

# merge_ways --------------------------------------------------------------

      #            < *3* <
      #          /        \
      #         /          \
      #  4 -<- 2            1 ->- 6 -<- 7 -<- 8 ->- 9 ->- 10
      #         \          /
      #          \        /
      #           -> 5 ->

test_that("first or last way is not circular", {


  expect_error(
    merge_ways(
      list(
        c("3", "2", "5", "1", "3"), # (roundabout)
        c("1", "6"),
        c("8", "7", "6")
      )
    ),
    "first or last way cannot be circular"
  )

})

test_that("straight line", {

  expect_equal(
    merge_ways(
      list(
        c("1", "6"),
        c("8", "7", "6"),
        c("8", "9", "10")
      )
    ),
    c("1", "6", "7", "8", "9", "10")
  )

})

test_that("including roudabout", {

  expect_equal(
    merge_ways(
      list(
        c("2", "4"),
        c("3", "2", "5", "1", "3"),  # (roundabout)
        c("1", "6"),
        c("8", "7", "6"),
        c("8", "9", "10")
      )
    ),
    c("4", "2", "5", "1", "6", "7", "8", "9", "10")
  )

})
