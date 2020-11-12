context("extract_data")

test_that("id_rel is correct", {

  expect_error(
    extract_data(c("1", "2")),
    "only one"
  )

  expect_error(
    extract_data("relation/12151"),
    "digits"
  )

})

test_that("exists but not transport line", {

  expect_error(
    extract_data("1"),
    "not found or empty"
  )

  expect_error(
    extract_data("300751"), # type = multipolygon
    "the relation must be tagged `type=route`."
  )

  expect_error(
    extract_data("5370153"), # route = hiking
    "\"route\" tag of the relation must be one of"
  )

})
