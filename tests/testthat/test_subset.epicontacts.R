context("Subsetting epicontacts by node and edge attributes")

test_that("Return errors / warnings when expected", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                         id="case.id", to="case.id", from="infector",
                         directed=FALSE)

  not.epicontacts <- list("linelist"=ebola_sim$linelist,"contacts"=ebola_sim$contacts,"directed"=FALSE)
  expect_error(subset.epicontacts(not.epicontacts,
                                   node.attribute=list("gender"="f"),
                                   edge.attribute=list("source"="funeral")),
               "x is not an 'epicontacts' object")

  expect_error(subset.epicontacts(x,
                                   node.attribute=list("gende"="f"),
                                   edge.attribute=list("source"="funeral")),
               "gende is not an attribute found in dataset")

  expect_error(subset.epicontacts(x,
                                   node.attribute=list("gender"="f"),
                                   edge.attribute=list("sourc"="funeral")),
               "sourc is not an attribute found in dataset")

  expect_error(subset.epicontacts(x,
                                   node.attribute=list("gender"="n"),
                                   edge.attribute=list("source"="funeral")),
               "Value for gender is not found in dataset")

  expect_error(subset.epicontacts(x,
                                   node.attribute=list("gender"="f"),
                                   edge.attribute=list("source"="funera")),
               "Value for source is not found in dataset")

  expect_error(subset.epicontacts(x,
                                   node.attribute=c("gender"),
                                   edge.attribute=list("source"="funeral")),
               "node.attribute is not a list")

  expect_error(subset.epicontacts(x,
                                   node.attribute=list("gender"),
                                   edge.attribute=c("source"="funeral")),
               "edge.attribute is not a list")

  expect_error(subset.epicontacts(x,
                                   node.attribute=list("gender"="f","date.of.infection"=c("2014-04-08","2015-03-28")),
                                   edge.attribute=list("source"="funeral")),
               "date.of.infection must be provided as a date object")

  expect_warning(subset.epicontacts(x,
                                     node.attribute=list("gender"="f",
                                                         "date.of.infection"=
                                                         as.Date(c("2014-04-08","2015-03-28","2014-04-08"))),
                                   edge.attribute=list("source"="funeral")),
               "More than two date values provided for date.of.infection, using first two")

  expect_warning(subset.epicontacts(x),
               "No subsetting attributes provided, returning input object")

})

test_that("Returns epicontacts object subsetted correctly", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                         id="case.id", to="case.id", from="infector",
                         directed=FALSE)

  dates <- as.Date(c("2014-04-08","2015-03-28"))

  y <- subset.epicontacts(x,
                           node.attribute=list("gender"="f",
                                               "date.of.infection"=dates),
                           edge.attribute=list("source"="funeral"))

  expect_is(y,"epicontacts")
  expect_true(all(y$linelist$gender=="f") &&
              min(y$linelist$date.of.infection) >= dates[1] &&
              max(y$linelist$date.of.infection) <= dates[2] &&
              all(y$contacts$source=="funeral"))

})
