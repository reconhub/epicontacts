context("Printing of epicontacts objects")

test_that("Printing objects works", {
    skip_on_cran()
    skip_on_travis()

    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                           id="case_id", to="case_id", from="infector",
                           directed = FALSE)

    # nb rds/print.rds was created with the following:
    ##saveRDS(testthat::capture_output(print(x)), file = "rds/print.rds")
    expect_equal(capture_output(print(x)), readRDS("rds/print.rds"))
})
