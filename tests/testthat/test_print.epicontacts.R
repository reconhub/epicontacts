context("Printing of epicontacts objects")

test_that("Printing objects works", {
    skip_on_cran()

    x <- make_epicontacts(ebola.sim$linelist, ebola.sim$contacts,
                           id="case.id", to="case.id", from="infector",
                           directed=FALSE)
  

    expect_equal_to_reference(capture.output(print(x)),
                              file = "rds/print1.rds")
})
