context("Converting data to epi_contact using read_contacts")

test_that("Class and content are fine", {
    if(require(outbreaks)){
        x <- read_contacts(ebola.sim$linelist)
        expect_is(x, "epi_contacts")
        expect_is(x, "list")
    }
})

test_that("Errors happen when they should", {
    if(require(outbreaks)){
        expect_error(read_contacts(ebola.sim$linelist[c(1,1,2,3,3,4,5,6),], ebola.sim$contacts),
                     "Duplicated IDs detected in the linelist")
    }
})
