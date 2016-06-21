context("Converting data to epi_contact using read_contacts")

test_that("Class and content are fine", {
    x <- read_contacts(ebola.sim$linelist)
    expect_is(x, "epi_contacts")
    expect_is(x, "list")
    expect_is(x$linelist$id, "character")
})

test_that("Errors happen when they should", {
    expect_error(read_contacts(ebola.sim$linelist[c(1,1,2,3,3,4,5,6),], ebola.sim$contacts),
                 "Duplicated IDs detected in the linelist")
})
