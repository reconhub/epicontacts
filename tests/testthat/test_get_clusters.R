context("Retrieving clusters from linelist with get_clusters")

test_that("igraph functions perform as expected", {
  
  skip_on_cran()
  
  x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
    id="case.id", to="case.id", from="infector",
    directed=TRUE)
  
  net <- as.igraph.epi_contacts(x)
  expect_is(net, "igraph")
  
})