context("Colors and palettes")

test_that("Test color palettes", {
    skip_on_cran()

    expect_true(all(is_color(cases_pal(1))))
    expect_true(all(is_color(cases_pal(100))))
    expect_true(all(is_color(spectral(1))))
    expect_true(all(is_color(spectral(100))))

    msg <- "n is not a number"
    expect_error(cases_pal("asd"), msg)

})






test_that("Test transp", {
    skip_on_cran()

    col <- cases_pal(100)
    expect_true(all(is_color(transp(col))))

})






test_that("Test fac2col", {
    skip_on_cran()

    x <- sample(letters[1:5], 50, replace = TRUE)
    expect_true(all(is_color(fac2col(x))))
    expect_length(fac2col(x), length(x))

    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                          id = "case_id", to = "case_id", from = "infector",
                          directed = FALSE)
    x <- thin(x[1:100], 2)

    ## check manual labelling
    pal <- c(Death = "blue", Recover = "green")
    col <- fac2col(x$linelist$outcome, pal, legend = TRUE)

    ## check color order is correct
    expect_equal(unname(pal[col$leg_lab]), col$leg_col)
    col <- fac2col(x$linelist$outcome, as.list(pal), legend = TRUE)
    expect_equal(unname(pal[col$leg_lab]), col$leg_col)

    ## check errors
    expect_error(fac2col(x$linelist$gender, pal, legend = TRUE),
                 paste0("col_pal/edge_col_pal must specify a color for",
                        " all elements in node_color/edge_color"))
    expect_error(fac2col(x$linelist$outcome, unname(pal), legend = TRUE),
                 paste0("col_pal/edge_col_pal must be a function",
                        " or named character vector/list"))
    wrong_pal <- c(Death = "bluee", Recover = "green")
    expect_error(fac2col(x$linelist$outcome, wrong_pal),
                 paste0("all values in col_pal/edge_col_pal must be colors"))


})
