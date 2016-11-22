context("Printing of epi_contacts summary objects")

test_that("Printing summaries works", {
    skip_on_cran()

    x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
                           id="case.id", to="case.id", from="infector",
                           directed=FALSE)

    res <- summary(x)

    expect_equal_to_reference(capture.output(print(res)),
                              file = "rds/print2.rds")
})
