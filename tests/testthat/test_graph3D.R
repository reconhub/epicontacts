context("3D Graph")

test_that("graph3D produces json that is not null", {
  
  skip_on_cran()
  
  x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
    id="case.id", to="case.id", from="infector",
    directed=TRUE)
  
  g <- graph3D(x)
  expect_is(g$x, "json")
  expect_more_than(length(g$x), 0)
  
})