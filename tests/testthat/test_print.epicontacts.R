context("Printing of epicontacts objects")

test_that("Printing objects works", {
    skip_on_cran()

    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                           id="case_id", to="case_id", from="infector",
                           directed = FALSE)


    expect_equal_to_reference(capture.output(print(x)),
                              file = "rds/print1.rds")
})
