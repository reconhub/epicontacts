context("Handling: [ operator")

test_that("Various subsetting using [", {
  
  ## Add list elements for testing
  linelist <- ebola_sim$linelist
  linelist$latlon <- replicate(nrow(linelist),
                               runif(2, -90, 90),
                               simplify = FALSE)

    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                           id = "case_id", to = "case_id", from = "infector",
                           directed = TRUE)

    ## make subsets
    x0 <- x["toto", "tata"]
    x1 <- x[get_id(x, "linelist")[1:10]]
    x2 <- x[j = get_id(x, "contacts")[1:10], contacts = "either"]
    x3 <- x[j = get_id(x, "contacts")[1:10], contacts = "both"]
    x4 <- x[j = get_id(x, "from")[1:3], contacts="from"]
    x5 <- x[j = get_id(x, "to")[1:3], contacts="to"]
    x6 <- x[1:3, 1:3, k = 2:1, l = 1]
    x7 <- x[k = FALSE, l = FALSE]

    expect_is(x0, "epicontacts")
    expect_is(x1, "epicontacts")
    expect_is(x2, "epicontacts")
    expect_is(x3, "epicontacts")
    expect_is(x4, "epicontacts")
    expect_is(x5, "epicontacts")
    expect_is(x6, "epicontacts")
    expect_is(x7, "epicontacts")

    expect_equal(nrow(x0$linelist), 0)
    expect_equal(nrow(x0$contacts), 0)

    expect_equal(nrow(x2$linelist), nrow(x$linelist))
    expect_equal(nrow(x2$contacts), 16)

    expect_equal(nrow(x3$linelist), nrow(x$linelist))
    expect_true(all(get_id(x3, "contacts") %in% get_id(x, "contacts")[1:10]))
    expect_equal(nrow(x3$contacts), 3)

    expect_equal(nrow(x4$linelist), nrow(x$linelist))
    expect_true(all(get_id(x4, "from") %in% get_id(x, "from")[1:3]))
    expect_equal(nrow(x4$contacts), 4)

    expect_equal(nrow(x5$linelist), nrow(x$linelist))
    expect_true(all(get_id(x5, "to") %in% get_id(x, "to")[1:3]))
    expect_equal(nrow(x5$contacts), 3)

    expect_equal(dim(x6$linelist), c(3,3))
    expect_equal(dim(x6$contacts), c(3,3))
    expect_identical(names(x6$linelist), names(x$linelist)[c(1,3,2)])

    expect_equal(dim(x7$linelist), c(nrow(x$linelist), 1))
    expect_equal(dim(x7$contacts), c(nrow(x$contacts), 2))

})

test_that("Errors / warnings happen when they should", {
    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                           id = "case_id", to = "case_id", from = "infector",
                           directed = FALSE)

    expect_error(x[j = get_id(x)[1:100], contacts="tamere"],
                 ".*should be one of.*")
})


