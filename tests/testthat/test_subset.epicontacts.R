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
    z <- thin(subset(x, cluster_id = id), 2)
    nocoords <- grep("(lat|lon)", names(z$linelist), perl = TRUE, invert = TRUE) - 1
    expect_equal_to_reference(z[k = nocoords], file = "rds/z.rds")


    zz <- subset(x, cs = 10)
    expect_equal_to_reference(zz[k = nocoords], file = "rds/zz.rds")
    expect_true(all(get_clusters(zz, "data.frame")$cluster_size == 10L))


})
