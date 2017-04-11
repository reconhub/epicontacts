context("3D Graph")

test_that("graph3D produces json that is not null", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
    id="case.id", to="case.id", from="infector",
    directed=TRUE)

  g <- graph3D(x)
  expect_is(g$x, "json")
  expect_gt(length(g$x), 0)

  g <- graph3D(x, annot = FALSE)
  expect_is(g$x, "json")
  expect_gt(length(g$x), 0)

})
