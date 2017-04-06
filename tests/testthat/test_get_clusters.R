context("Retrieving clusters from linelist with get_clusters")

test_that("igraph functions perform as expected", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist,
                        ebola_sim$contacts,
                        id = "case.id",
                        to = "case.id",
                        from = "infector",
                        directed=TRUE)

  net <- as.igraph.epicontacts(x)

  expect_is(net, "igraph")

})






test_that("construction of net nodes works", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist,
                        ebola_sim$contacts,
                        id = "case.id",
                        to = "case.id",
                        from = "infector",
                        directed = TRUE)

  net <- as.igraph.epicontacts(x)
  cs <- igraph::clusters(net)
  cs_size <- data.frame(cluster_member = seq_along(cs$csize),
    cluster_size = cs$csize)

  net_nodes <- data.frame(id =igraph::V(net)$id,
    cluster_member = cs$membership,
    stringsAsFactors = FALSE)

  net_nodes <- dplyr::left_join(net_nodes, cs_size, by = "cluster_member")

  expect_named(net_nodes,
               c("id", "cluster_member","cluster_size"),
               ignore.order = TRUE)

})






test_that("get_clusters returns epicontacts object", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist,
                        ebola_sim$contacts,
                        id = "case.id",
                        to = "case.id",
                        from = "infector",
                        directed = TRUE)

  y <- get_clusters(x)

  expect_is(y, "epicontacts")
})






test_that("get_clusters returns data.frame", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist,
                        ebola_sim$contacts,
                        id = "case.id",
                        to = "case.id",
                        from = "infector",
                        directed = TRUE)

  y <- get_clusters(x, output = "data.frame")

  expect_is(y, "data.frame")

})

