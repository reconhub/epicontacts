context("Test get_pairwise")

test_that("pairwise analysis: gender", {
    skip_on_cran()

    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                           id = "case_id", to = "case_id", from = "infector",
                           directed = TRUE)

    pair <- get_pairwise(x, "gender")
    pair2 <- get_pairwise(x, 8)

    expect_that( pair, is_a("character") )
    expect_equal( length(pair), 3800 )
    expect_that( pair[1], is_a("character") )

    expect_identical(pair, pair2)

})



test_that("provide false characters", {
    skip_on_cran()

    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                           id = "case_id", to = "case_id", from = "infector",
                           directed = TRUE)

    expect_error(get_pairwise(x, "gende"))
    expect_true(length(table(get_pairwise(x, "gender", hard_NA = FALSE))) >
                length(table(get_pairwise(x, "gender", hard_NA = TRUE))))

})





test_that("expected errors", {
    skip_on_cran()

    expect_error(get_pairwise(NULL),
                 "x is not an 'epicontacts' object")
})




test_that("different types of attributes", {
    skip_on_cran()

    test_net <- readRDS("rds/test_net.rds")

    ## date
    pair_date <- get_pairwise(test_net, "date")
    expect_is(pair_date, "integer")

    ## integer
    pair_integer <- get_pairwise(test_net, "integer")
    expect_is(pair_integer, "integer")

    ## numeric
    pair_numeric <- get_pairwise(test_net, "numeric")
    expect_is(pair_numeric, "numeric")

    ## character
    pair_character <- get_pairwise(test_net, "character")
    expect_is(pair_character, "character")

    ## factor
    pair_factor <- get_pairwise(test_net, "factor")
    expect_is(pair_factor, "character")

})




test_that("test get pairwise values", {
    skip_on_cran()

    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                           id = "case_id", to = "case_id", from = "infector",
                           directed = TRUE)

    val1 <- get_pairwise(x, 'lat')
    man1 <- abs(x$linelist$lat[match(x$contacts$from, x$linelist$id)] -
                x$linelist$lat[match(x$contacts$to, x$linelist$id)])
    expect_equal(val1, man1)

    val2 <- get_pairwise(x, 'date_of_infection')
    man2 <- abs(x$linelist$date_of_infection[match(x$contacts$from, x$linelist$id)] -
                x$linelist$date_of_infection[match(x$contacts$to, x$linelist$id)])
    expect_equal(val2, as.integer(man2))

})
