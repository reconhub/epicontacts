context("Testing igraph conversion")

test_that("Name column check behaves as expected", {

  skip_on_cran()

  ebola_sim$linelist$name <- rep("name", nrow(ebola_sim$linelist))

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                        id = "case_id",
                        to = "case_id",
                        from = "infector",
                        directed=FALSE)

  x <- thin(x[1:100], 2)

  net <- as.igraph.epicontacts(x)

  expect_equal(igraph::vertex_attr(net)$epicontacts_name, x$linelist$name)

})


test_that("missing data will be added to the linelist", {
  
  skip_on_cran()
  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                        id = "case_id",
                        to = "case_id",
                        from = "infector",
                        directed = FALSE)
  x <- thin(x[1:100], 2)

  x$contacts[6:9, ] <- NA
  expect_warning(net <- as.igraph.epicontacts(x), "NA")
  expect_is(net, "igraph")
  expect_identical(igraph::vertex_attr(net)$id, c(get_id(x, "linelist"), "NA"))

})
