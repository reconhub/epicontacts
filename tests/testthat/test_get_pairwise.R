context("Test get_pairwise")

test_that("pairwise analysis: gender", {
    skip_on_cran()

    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                           id = "case_id", to = "case_id", from = "infector",
                           directed = TRUE)

    pair <- get_pairwise(x, "gender")
    pair2 <- get_pairwise(x, 8)

    expect_that( pair, is_a("character") )
    expect_that( length(pair), equals(3800) )
    expect_that( pair[1], is_a("character") )

    expect_identical(pair, pair2)

})



test_that("provide false characters", {
    skip_on_cran()

    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                           id = "case_id", to = "case_id", from = "infector",
                           directed = TRUE)

    expect_error(get_pairwise(x, "gende"))
    expect_that(length(table(get_pairwise(x, "gender", hard_NA = FALSE))) >
                length(table(get_pairwise(x, "gender", hard_NA = TRUE))), expect_true())

})





test_that("expected errors", {
    skip_on_cran()

    expect_error(get_pairwise(NULL),
                 "x is not an 'epicontacts' object")
})




test_that("different types of attributes", {
    skip_on_cran()

    ## need to add a numeric entry to the linelist
    ebola_sim$num <- as.numeric(ebola_sim$linelist$generation)
    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                           id = "case_id", to = "case_id", from = "infector",
                           directed = TRUE)

    pair.doi <- get_pairwise(x, "date_of_infection")
    expect_is(pair.doi, "integer")
    expect_equal(min(pair.doi, na.rm = TRUE), 0L)

    pair.gen <- get_pairwise(x, "generation")
    expect_is(pair.gen, "integer")
    expect_equal(min(pair.gen, na.rm = TRUE), 1)

})
