context("Plotting epicontacts Objects")

test_that("Plot argument matching works", {
  
  skip_on_cran()
  
  x <- make_epicontacts(ebola.sim$linelist, ebola.sim$contacts,
    id="case.id", to="case.id", from="infector",
    directed=FALSE)
  
  expect_error(plot(x, y = "foobar"))
  
})
