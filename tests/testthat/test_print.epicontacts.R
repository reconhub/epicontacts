context("Printing of epicontacts objects")

test_that("Printing objects works", {
    skip_on_cran()
    skip_on_travis()

    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                           id="case_id", to="case_id", from="infector",
                          directed = FALSE)

    ## strings to test for in output
    tst <- c("non directed",
             "epicontacts",
             "linelist",
             "contacts",
             as.character(x$linelist$date_of_onset[1]),
             as.character(x$linelist$date_of_hospitalisation[1]),
             x$contacts$from[1],
             x$contacts$to[1],
             format(nrow(x$linelist), big.mark = ","),
             format(nrow(x$contacts), big.mark = ","))

    for(val in tst) {
      expect_output(print(x), val)
    }
    
})
