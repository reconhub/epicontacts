context("Subsetting epicontacts by node and edge attributes")

test_that("Return errors / warnings when expected", {

    skip_on_cran()

    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                          id = "case_id",
                          to = "case_id", from = "infector",
                          directed=FALSE)

    not_epicontacts <- list("linelist" = ebola_sim$linelist,
                            "contacts" = ebola_sim$contacts,
                            "directed" = FALSE)
    expect_error(
        subset.epicontacts(not_epicontacts,
                           node_attribute = list("gender" = "f"),
                           edge_attribute = list("source" = "funeral")),
        "x is not an 'epicontacts' object")

    expect_error(
        subset.epicontacts(x,
                           node_attribute = list("gende" = "f"),
                           edge_attribute = list("source" = "funeral")),
        "gende is not an attribute found in dataset")

    expect_error(
        subset.epicontacts(x,
                           node_attribute = list("gender" = "f"),
                           edge_attribute = list("sourc" = "funeral")),
        "sourc is not an attribute found in dataset")

    expect_error(
        subset.epicontacts(x,
                           node_attribute = list("gender" = "n"),
                           edge_attribute = list("source" = "funeral")),
        "Value for gender is not found in dataset")

    expect_error(
        subset.epicontacts(x,
                           node_attribute = list("gender" = "f"),
                           edge_attribute = list("source" = "funera")),
        "Value for source is not found in dataset")

    expect_error(
        subset.epicontacts(x,
                           node_attribute = c("gender"),
                           edge_attribute = list("source" = "funeral")),
        "node_attribute is not a list")

    expect_error(
        subset.epicontacts(x,
                           node_attribute = list("gender"),
                           edge_attribute = c("source" = "funeral")),
        "edge_attribute is not a list")

    expect_error(
        subset.epicontacts(
            x,
            node_attribute =
            list("gender" = "f",
                 "date_of_infection" = c("2014-04-08", "2015-03-28")),
            edge_attribute = list("source" = "funeral")),
        "date_of_infection must be provided as a date object")

    expect_warning(
        subset.epicontacts(
            x,
            node_attribute = list("gender" = "f",
                "date_of_infection" =
                as.Date(c("2014-04-08","2015-03-28","2014-04-08"))),
            edge_attribute = list("source" = "funeral")),
        "More than two date values provided for date_of_infection, using first two")

    expect_warning(
        subset.epicontacts(x),
        "No subsetting attributes provided, returning input object")

})






test_that("Returns epicontacts object subsetted correctly", {

    skip_on_cran()

    x <- make_epicontacts(ebola_sim$linelist,
                          ebola_sim$contacts,
                          id = "case_id",
                          to = "case_id", from = "infector",
                          directed = FALSE)

    dates <- as.Date(c("2014-04-08","2015-03-28"))

    y <- subset.epicontacts(
        x,
        node_attribute = list("gender" = "f",
            "date_of_infection" = dates),
        edge_attribute = list("source" = "funeral"))

    expect_is(y, "epicontacts")
    expect_true(
        all(y$linelist$gender == "f") &&
        min(y$linelist$date_of_infection) >=  dates[1] &&
        max(y$linelist$date_of_infection) <= dates[2] &&
        all(y$contacts$source == "funeral"))


    id <- names(which.max(get_degree(x, "out")))

    ## check subset with thinning
    ## check that all ids in contact and linelist are in the same cluster as id,
    ## and check that no ids from other clusters are in contact or linelist.
    ## with thinning this means all cases must also be in the linelist
    z <- thin(subset(x, cluster_id = id), 2)
    clust <- get_clusters(x, output = "data.frame")
    clust_id <- clust$cluster_member[match(id, clust$id)]
    are_in_clust_cont <- sort(unique(unlist(z$contacts[1:2],
                                            use.names = FALSE)))
    are_in_clust_ll <- sort(z$linelist$id)
    should_in_clust <- sort(clust$id[clust$cluster_member == clust_id])
    should_in_clust <- should_in_clust[should_in_clust %in% x$linelist$id]
    expect_equal(should_in_clust, are_in_clust_cont)
    expect_equal(should_in_clust, are_in_clust_ll)

    ## check without thinning
    ## in this case there can be cases in the contacts and not in the linelist
    w <- subset(x, cluster_id = id)
    should_in_clust <- sort(clust$id[clust$cluster_member == clust_id])
    are_in_clust <- sort(unique(unlist(w$contacts[1:2], use.names = FALSE)))
    expect_equal(should_in_clust, are_in_clust)

    ## check k subsetting
    nocoords <- grep("(lat|lon)", names(z$linelist), perl = TRUE, invert = TRUE) - 1
    k_sub <- z[k = nocoords]

    ## check correct columns have been subsetted
    expect_equal(names(z$linelist)[nocoords + 1], names(k_sub$linelist))

    ## check contacts haven't been changed
    expect_equal(z$contacts, k_sub$contacts)

    ## compare to reference
    expect_equal_to_reference(k_sub, file = "rds/z.rds")
    
    zz <- subset(x, cs = 10)
    expect_equal_to_reference(zz[k = nocoords], file = "rds/zz.rds")
    expect_true(all(get_clusters(zz, "data.frame")$cluster_size == 10L))

})
