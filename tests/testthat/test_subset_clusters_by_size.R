context("Subsetting clusters by size")

test_that("Return errors / warnings when expected", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                         id="case_id", to="case_id", from="infector",
                         directed=FALSE)

  expect_error(subset_clusters_by_size(x),
               "You must enter either cs, cs_min, or cs_max")

  expect_message(subset_clusters_by_size(x,cs=5,cs_min=2,cs_max=7),
                 "Using cs_min and cs_max to subset data")

})





test_that("Returns clusters with the correct size", {

    skip_on_cran()

    x <- make_epicontacts(ebola_sim$linelist,
                           ebola_sim$contacts, id="case_id",
                           to="case_id", from="infector",
                           directed=FALSE)

    y <- subset_clusters_by_size(x, cs_max = 10)

    net <- as.igraph.epicontacts(y)
    clusters <- igraph::clusters(net)
    expect_true(all(clusters$csize <= 10))

    y <- subset_clusters_by_size(x,cs_min=5)
    net <- as.igraph.epicontacts(y)
    clusters <- igraph::clusters(net)
    expect_true(all(clusters$csize >= 5))

    y <- subset_clusters_by_size(x, cs=5)
    net <- as.igraph.epicontacts(y)
    clusters <- igraph::clusters(net)
    expect_true(all(clusters$csize == 5))

    y <- subset_clusters_by_size(x, cs=5, cs_min=8, cs_max=15)
    net <- as.igraph.epicontacts(y)
    clusters <- igraph::clusters(net)
    expect_true(all(clusters$csize >= 8))
    expect_true(all(clusters$csize <= 15))

})
