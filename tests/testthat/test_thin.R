context("Thin")

test_that("Thin ouputs are correct", {
    x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
                           id = "case.id", to = "case.id", from = "infector",
                           directed = TRUE)

    y <- x[j = "916d0a", contacts = "from"]


    ## check expected errors
    expect_error(thin(1), "x is not an epi_contacts object")
    msg <- paste0("Wrong values for 'what'; accepted values are:\n",
                      "'linelist', 'contact', 1, 2")       
    expect_error(thin(x, 3), msg)
    expect_error(thin(x, "foo"), msg)

    ## check outputs
    expect_equal(nrow(thin(x)$linelist), 4352)
    expect_equal(dim(thin(x)$contacts), dim(x$contacts))
    expect_equal(dim(thin(x, 2)$contacts), dim(x$contacts))

    expect_equal(length(get_id(y, "contacts")),
                 nrow(thin(y)$linelist))

})
