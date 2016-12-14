context("Plotting epicontacts Objects")

test_that("Returns properly grouped plot", {
  
  skip_on_cran()
  
  x <- make_epicontacts(ebola.sim$linelist, ebola.sim$contacts,
    id="case.id", to="case.id", from="infector",
    directed=FALSE)
  
  netplot <- vis_epicontacts(x, group = "gender")
  
  expect_equal(netplot$x$byselection$variable, "gender")
  
})

test_that("Returns error when grouping specification is not in line list", {
  
  skip_on_cran()
  
  x <- make_epicontacts(ebola.sim$linelist, ebola.sim$contacts,
    id="case.id", to="case.id", from="infector",
    directed=FALSE)
  
  expect_error(vis_epicontacts(x, group = "sex"))
  
})

test_that("Returns error when annotation specification is not in line list", {
  
  skip_on_cran()
  
  x <- make_epicontacts(ebola.sim$linelist, ebola.sim$contacts,
    id="case.id", to="case.id", from="infector",
    directed=FALSE)
  
  expect_error(vis_epicontacts(x, annot = "sex"))
  
})
