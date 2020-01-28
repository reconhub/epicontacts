context("Printing of epicontacts summary objects")

test_that("Printing summaries", {
  
  skip_on_cran()

  expect_warning(
  {x <- make_epicontacts(mers_korea_2015$linelist, mers_korea_2015$contacts,
                          id = "id",
                          to = "to", from = "from",
                         directed=FALSE)},
  "Cycle(s) detected in the contact network: this may be unwanted",
  fixed = TRUE)

  res <- summary(x)

  expect_equal_to_reference(capture.output(print(res)),
                            file = "rds/print2.rds")

  y <- x
  y$contacts <- y$contacts[1:10, ]
  y <- thin(y)
  y$contacts$from[6:9] <- NA
  y$contacts$to[1] <- NA
  expect_output(print(summary(y)), "number missing 'from': 4") 
  expect_output(print(summary(y)), "number missing 'to': 1") 
  
})
