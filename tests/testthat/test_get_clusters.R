context("Retrieving clusters from linelist with get_clusters")

test_that("igraph functions perform as expected", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist,
                        ebola_sim$contacts,
                        id = "case_id",
                        to = "case_id",
                        from = "infector",
                        directed=TRUE)

  net <- as.igraph.epicontacts(x)

  expect_is(net, "igraph")

})






test_that("construction of net nodes works", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist,
                        ebola_sim$contacts,
                        id = "case_id",
                        to = "case_id",
                        from = "infector",
                        directed = TRUE)

  net <- as.igraph.epicontacts(x)
  cs <- igraph::clusters(net)
  cs_size <- data.frame(cluster_member = seq_along(cs$csize),
                        cluster_size = cs$csize)

  net_nodes <- data.frame(id = igraph::V(net)$id,
                          cluster_member = cs$membership,
                          stringsAsFactors = FALSE)

  net_nodes <- merge(net_nodes, cs_size, by.x = "cluster_member")

  expect_named(net_nodes,
               c("id", "cluster_member", "cluster_size"),
               ignore.order = TRUE)

})






test_that("clusters are identified correctly", {

  skip_on_cran()

  ## make clusters of different sizes
  cs_size <- c(5, 10, 17, 3, 5)
  contacts <- data.frame(from = seq_len(sum(cs_size)-1),
                         to = seq(2, sum(cs_size)))
  contacts <- contacts[-head(cumsum(cs_size), -1),]
  linelist <- data.frame(id = seq_len(sum(cs_size)))
  
  x <- make_epicontacts(linelist, contacts)  
  clust <- get_clusters(x, 'data.frame')

  ## check that cluster size and membership are correct
  expect_equal(rep(cs_size, times = cs_size),
               clust$cluster_size)
  expect_equal(rep(seq_along(cs_size), times = cs_size),
               as.numeric(clust$cluster_member))

})









test_that("get_clusters returns epicontacts object", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist,
                        ebola_sim$contacts,
                        id = "case_id",
                        to = "case_id",
                        from = "infector",
                        directed = TRUE)

  y <- get_clusters(x)

  expect_is(y, "epicontacts")
  
})






test_that("get_clusters returns data.frame", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist,
                        ebola_sim$contacts,
                        id = "case_id",
                        to = "case_id",
                        from = "infector",
                        directed = TRUE)

  y <- get_clusters(x, output = "data.frame")

  expect_is(y, "data.frame")

})


test_that("get_clusters errors as expected", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist,
                        ebola_sim$contacts,
                        id = "case_id",
                        to = "case_id",
                        from = "infector",
                        directed = TRUE)

  # add bogus cluster info
  x$linelist$cluster_member <- sample(LETTERS, nrow(x$linelist), replace = TRUE)

  msg <- paste0("'cluster_member' is already in the linelist. Set 'override =",
                " TRUE' to write over it, else assign a different member_col name.")
  expect_error(get_clusters(x), msg)

  # more bogus cluster info
  x$linelist$cluster_size <- 1:nrow(x$linelist)

  msg <- paste0("'cluster_member' and 'cluster_size' are already in the linelist. ",
                "Set 'override = TRUE' to write over them, else assign different ",
                "cluster column names.")
  expect_error(get_clusters(x), msg)

})

test_that("get_clusters override behavior works", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist,
                        ebola_sim$contacts,
                        id = "case_id",
                        to = "case_id",
                        from = "infector",
                        directed = TRUE)

  foo <- get_clusters(x)

  x$linelist$cluster_member <- sample(LETTERS, nrow(x$linelist), replace = TRUE)
  x$linelist$cluster_size <- 1:nrow(x$linelist)

  bar <- get_clusters(x, override = T)

  expect_equal(foo, bar)

})



test_that("get_clusters works on different classes", {

  skip_on_cran()

  ## igraph returned cluster information with case ids coerced to character,
  ## causing errors when this information was merged with integer case ids in
  ## the linelist. the fix coerces cluster case id to the same class as the
  ## linelist case ids.

  ## load data
  linelist <- readRDS("rds/test_linelist.rds")
  contacts <- readRDS("rds/test_contacts.rds")

  ## possible classes
  classes <- c("factor", "integer", "numeric", "date", "character")
  comb <- expand.grid(id = classes,
                      from = paste0(classes, "_from"),
                      to = paste0(classes, "_to"),
                      stringsAsFactors = FALSE)

  ## test that get_cluster doesn't errors for any combination of classes
  ## for 'id', 'from' and 'to'
  test_get_cluster <- function(id, from, to, linelist, contacts) {
    net <- make_epicontacts(linelist, contacts, id, from, to)
    expect_error(get_clusters(net), NA)
  }

  ## test whether get_cluster throws any errors
  mapply(test_get_cluster, comb$id, comb$from, comb$to,
         MoreArgs = list(linelist, contacts))
  
})
