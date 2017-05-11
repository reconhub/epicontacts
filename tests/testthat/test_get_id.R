context("Extracting IDs from epi_contact")

test_that("IDs are extracted fine", {
    skip_on_cran()

    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                           id="case_id", to="case_id", from="infector",
                           directed=TRUE)

    ## get identifiers
    id1 <- get_id(x)
    id2 <- get_id(x, "contacts")
    id3 <- get_id(x, "all")
    id4 <- get_id(x, "common")
    id5 <- get_id(x, "from")
    id6 <- get_id(x, "to")

    expect_is(id1, "character")
    expect_is(id2, "character")
    expect_is(id3, "character")
    expect_is(id4, "character")
    expect_is(id5, "character")
    expect_is(id6, "character")

    expect_true(length(id4) < length(id3))
    expect_equal(union(id1,id2), id3)
    expect_equal(intersect(id1,id2), id4)
    expect_equal(unique(c(id5,id6)), id2)

    msg <- "x is not an 'epicontacts' object"
    expect_error(get_id("toto"), msg)

})





test_that("Errors / warnings happen when they should", {
    skip_on_cran()

    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                           id="case_id", to="case_id", from="infector",
                           directed=FALSE)

    expect_error(get_id(x, "tamere"),
                 ".*should be one of.*")

    expect_warning(get_id(x, "from"),
                   "x is not directed; 'from' has no particular meaning")

    expect_warning(get_id(x, "to"),
                   "x is not directed; 'to' has no particular meaning")
})
