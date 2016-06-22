context("Subsetting epi_contacts by IDs")

test_that("Data with specified IDs are extracted fine", {
    x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
                           id="case.id", to="case.id", from="infector",
                           directed=TRUE)

    ## get identifiers
    x1 <- x[get_id(x, "linelist")[1:10],]
    x2 <- x[get_id(x, "contacts")[1:10],]
    x3 <- x[get_id(x, "contacts")[1:10], contacts="either"]
    x4 <- x[get_id(x, "from")[1:3], contacts="from"]

    expect_is(x1, "epi_contacts")
    expect_is(x2, "epi_contacts")
    expect_is(x3, "epi_contacts")
    expect_is(x4, "epi_contacts")

    expect_equal(nrow(x1$linelist),10)
    expect_equal(nrow(x2$linelist),7)
    expect_equal(nrow(x3$contacts),16)
    expect_equal(nrow(x4$linelist),2)
    expect_equal(nrow(x4$contacts),4)
})

test_that("Errors / warnings happen when they should", {
    x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
                           id="case.id", to="case.id", from="infector",
                           directed=FALSE)

    expect_error(x[i=get_id(x),, contacts="tamere"],
                 ".*should be one of.*")

    expect_warning(x[1,],
                   ".*logicals and integers cannot be used to subset epi_contacts objects.*")
})


