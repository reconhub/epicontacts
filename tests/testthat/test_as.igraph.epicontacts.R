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
