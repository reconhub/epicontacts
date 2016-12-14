context("Converting data to epicontacts using make_epicontacts")

test_that("Class and content are fine", {
    x <- make_epicontacts(ebola.sim$linelist, ebola.sim$contacts)
    expect_is(x, "epicontacts")
    expect_is(x$linelist$id, "character")
})

test_that("Errors happen when they should", {
    expect_error(make_epicontacts(ebola.sim$linelist[c(1,1,2,3,3,4,5,6),], ebola.sim$contacts), "Duplicated IDs detected in the linelist")
    expect_error(make_epicontacts(linelist = NULL, contacts = ebola.sim$contacts))
    expect_error(make_epicontacts(linelist = ebola.sim$linelist, contacts = NULL))
    expect_error(make_epicontacts(linelist = ebola.sim$linelist, contacts = data.frame(from = 1:100)))
    expect_error(make_epicontacts(linelist = ebola.sim$linelist, contacts = data.frame(from = NA)))
    
})


test_that("Reordering of columns works", {
    ## reverse data order
    linelist <- ebola.sim$linelist[,rev(seq_len(ncol(ebola.sim$linelist)))]
    contacts <- ebola.sim$contacts[,rev(seq_len(ncol(ebola.sim$contacts)))]

    ## make object
    x <- make_epicontacts(linelist, contacts,
                           id="case.id", to="case.id", from="infector")

    ## tests
    expect_equal(names(x$linelist)[1], "id")
    expect_equal(x$linelist$id, ebola.sim$linelist$case.id)
    expect_equal(names(x$contacts)[c(1,2)], c("from","to"))
    expect_equal(x$contacts$from, ebola.sim$contacts$infector)
    expect_equal(x$contacts$to, ebola.sim$contacts$case.id)

})
