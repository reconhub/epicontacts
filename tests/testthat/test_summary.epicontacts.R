context("Printing of epicontacts summary objects")

test_that("Printing summaries", {
    skip_on_cran()

    x <- make_epicontacts(mers_korea_2015$linelist, mers_korea_2015$contacts,
                          id = "id",
                          to = "to", from = "from",
                          directed=FALSE)

    res <- summary(x)

    expect_equal_to_reference(capture.output(print(res)),
                              file = "rds/print2.rds")
    
})
