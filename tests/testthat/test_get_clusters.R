context("Retrieving clusters from linelist with get_clusters")

test_that("igraph functions perform as expected", {
  
  skip_on_cran()
  
  x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
    id="case.id", to="case.id", from="infector",
    directed=TRUE)
  
  net <- as.igraph.epi_contacts(x)
  expect_is(net, "igraph")
  
})

test_that("construction of net nodes works", {
  
  skip_on_cran()
  
  x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
    id="case.id", to="case.id", from="infector",
    directed=TRUE)
  
  net <- as.igraph.epi_contacts(x)
  cs <- igraph::clusters(net)
  cs_size <- data.frame(cluster_member = seq_along(cs$csize),
    cluster_size = cs$csize)
  
  net_nodes <- data.frame(id =igraph::V(net)$id,
    cluster_member = cs$membership,
    stringsAsFactors = FALSE)
  
  net_nodes <- dplyr::left_join(net_nodes, cs_size, by = "cluster_member")
  expect_named(net_nodes, c("id", "cluster_member","cluster_size"), ignore.order = TRUE)
  
})