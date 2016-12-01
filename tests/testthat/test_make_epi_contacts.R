context("Converting data to epi_contacts using make_epi_contacts")

test_that("Class and content are fine", {
    x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts)
    expect_is(x, "epi_contacts")
    expect_is(x$linelist$id, "character")
})

test_that("Errors happen when they should", {
    expect_error(make_epi_contacts(ebola.sim$linelist[c(1,1,2,3,3,4,5,6),], ebola.sim$contacts), "Duplicated IDs detected in the linelist")
    expect_error(make_epi_contacts(linelist = NULL, contact = ebola.sim$contacts))
    expect_error(make_epi_contacts(linelist = ebola.sim$linelist, contact = NULL))
    expect_error(make_epi_contacts(linelist = ebola.sim$linelist, contact = data.frame(from = 1:100)))
    expect_error(make_epi_contacts(linelist = ebola.sim$linelist, contact = data.frame(from = NA)))
    
})


test_that("Reordering of columns works", {
    ## reverse data order
    linelist <- ebola.sim$linelist[,rev(seq_len(ncol(ebola.sim$linelist)))]
    contacts <- ebola.sim$contacts[,rev(seq_len(ncol(ebola.sim$contacts)))]

    ## make object
    x <- make_epi_contacts(linelist, contacts,
                           id="case.id", to="case.id", from="infector")

    ## tests
    expect_equal(names(x$linelist)[1], "id")
    expect_equal(x$linelist$id, ebola.sim$linelist$case.id)
    expect_equal(names(x$contacts)[c(1,2)], c("from","to"))
    expect_equal(x$contacts$from, ebola.sim$contacts$infector)
    expect_equal(x$contacts$to, ebola.sim$contacts$case.id)

})