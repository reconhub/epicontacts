context("Test get_pairwise")
library(epicontacts)
library(testthat)

test_that("check gender", {

    x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
                           id="case.id", to="case.id", from="infector",
                           directed=TRUE)

    pair <- get_pairwise(x, "gender")

    expect_that( pair, is_a("character") )
    expect_that( length(pair), equals(3800) )
    expect_that( pair[1], is_a("character") ) })

test_that("provide false characters", {
    x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
                           id="case.id", to="case.id", from="infector",
                           directed=TRUE)

     expect_error(get_pairwise(x, "gende"))
     expect_that(length(table(get_pairwise(x, "gender", hard_NA=F))) >
                     length(table(get_pairwise(x, "gender", hard_NA=T))), is_true())
     })
