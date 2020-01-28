context("Printing of epicontacts summary objects")

test_that("Printing summaries", {
  
  skip_on_cran()

  linelist <- mers_korea_2015$linelist
  contacts <- mers_korea_2015$contacts
  contacts <- contacts[1:10, ]
  contacts$from[6:9] <- NA
  contacts$to[1] <- NA
  expect_warning(x <- make_epicontacts(linelist, contacts), "NA")  
  x <- thin(x)

  res <- summary(x)

  ## strings to test for in output
  tst <- c("number missing 'from': 4",
           "number missing 'to': 1",
           "number of contacts: 10",
           "contacts",
           names(x$linelist)[-1],
           names(x$contact)[-(1:2)])
           
  for(val in tst) {
    expect_output(print(summary(x)), val)
  }
  
})
