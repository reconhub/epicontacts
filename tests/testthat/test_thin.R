context("Thin")

test_that("Thin ouputs are correct", {
    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                           id = "case_id", to = "case_id", from = "infector",
                           directed = TRUE)

    y <- x[j = "916d0a", contacts = "from"]


    ## check expected errors
    expect_error(thin(1), "x is not an epicontacts object")
    msg <- paste0("Wrong values for 'what'; accepted values are:\n",
                      "'linelist', 'contact', 1, 2")
    expect_error(thin(x, 3), msg)
    expect_error(thin(x, "foo"), msg)

    ## check outputs
    expect_equal(nrow(thin(x)$linelist), 4352)
    expect_equal(dim(thin(x)$contacts), dim(x$contacts))
    expect_equal(nrow(thin(x, 2)$contacts), 2161)

    expect_equal(length(get_id(y, "contacts")),
                 nrow(thin(y)$linelist))

    expect_true(all(get_id(thin(x)) %in% get_id(x, "contacts")))
    expect_true(all(get_id(thin(x,2), "contacts") %in%
                    get_id(x)))

    ## check with numerical ids
    y <- make_epicontacts(data.frame(id = 20:40),
                          data.frame(from = 21:30,
                                     to = 31:40))

    expect_true(all(get_id(thin(y)) %in% get_id(y, "contacts")))
    expect_true(all(get_id(thin(y, 2), "contacts") %in% get_id(y)))

})
