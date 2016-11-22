context("Plotting epi_contacts Objects")

test_that("Returns properly grouped plot", {
  
  skip_on_cran()
  
  x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
    id="case.id", to="case.id", from="infector",
    directed=FALSE)
  
  netplot <- vis_epi_contacts(x, group = "gender")
  
  expect_equal(netplot$x$byselection$variable, "gender")
  
})

test_that("Returns error when grouping specification is not in line list", {
  
  skip_on_cran()
  
  x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
    id="case.id", to="case.id", from="infector",
    directed=FALSE)
  
  expect_error(vis_epi_contacts(x, group = "sex"))
  
})

test_that("Returns error when annotation specification is not in line list", {
  
  skip_on_cran()
  
  x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
    id="case.id", to="case.id", from="infector",
    directed=FALSE)
  
  expect_error(vis_epi_contacts(x, annot = "sex"))
  
})