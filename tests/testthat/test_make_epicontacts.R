context("Converting data to epicontacts using make_epicontacts")

test_that("Class and content are fine", {
    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts)
    expect_is(x, "epicontacts")
    expect_is(x$linelist$id, "character")
})

test_that("Errors happen when they should", {
    expect_error(make_epicontacts(ebola_sim$linelist[c(1,1,2,3,3,4,5,6),], ebola_sim$contacts), "Duplicated IDs detected in the linelist")
    expect_error(make_epicontacts(linelist = NULL, contacts = ebola_sim$contacts))
    expect_error(make_epicontacts(linelist = ebola_sim$linelist, contacts = NULL))
    expect_error(make_epicontacts(linelist = ebola_sim$linelist, contacts = data.frame(from = 1:100)))
    expect_error(make_epicontacts(linelist = ebola_sim$linelist, contacts = data.frame(from = NA)))
    
})


test_that("Reordering of columns works", {
    ## reverse data order
    linelist <- ebola_sim$linelist[,rev(seq_len(ncol(ebola_sim$linelist)))]
    contacts <- ebola_sim$contacts[,rev(seq_len(ncol(ebola_sim$contacts)))]

    ## make object
    x <- make_epicontacts(linelist, contacts,
                           id="case.id", to="case.id", from="infector")

    ## tests
    expect_equal(names(x$linelist)[1], "id")
    expect_equal(x$linelist$id, ebola_sim$linelist$case.id)
    expect_equal(names(x$contacts)[c(1,2)], c("from","to"))
    expect_equal(x$contacts$from, ebola_sim$contacts$infector)
    expect_equal(x$contacts$to, ebola_sim$contacts$case.id)

})
