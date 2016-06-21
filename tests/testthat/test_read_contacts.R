context("Converting data to epi_contact using read_contacts")

## test_that("Class and content are fine", {
## })

test_that("Errors happen when they should"){
    if(require(outbreaks)){
        expect_error(read_contacts(ebola.sim$linelist, ebola.sim$contacts),
                     "Duplicated IDs detected in the linelist")
    }
}
