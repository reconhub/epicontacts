context("Subsetting epi_contacts by node and edge attributes")

test_that("Return errors / warnings when expected", {

 skip_on_cran()

  x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
                         id="case.id", to="case.id", from="infector",
                         directed=FALSE)

  not.epi_contacts <- list("linelist"=ebola.sim$linelist,"contacts"=ebola.sim$contacts,"directed"=FALSE)
  expect_error(subset.epi_contacts(not.epi_contacts,
                                   node.attribute=list("gender"="f"),
                                   edge.attribute=list("source"="funeral")),
               "x is not an 'epi_contacts' object")

  expect_error(subset.epi_contacts(x,
                                   node.attribute=list("gende"="f"),
                                   edge.attribute=list("source"="funeral")),
               "gende is not an attribute found in dataset")

  expect_error(subset.epi_contacts(x,
                                   node.attribute=list("gender"="f"),
                                   edge.attribute=list("sourc"="funeral")),
               "sourc is not an attribute found in dataset")

  expect_error(subset.epi_contacts(x,
                                   node.attribute=list("gender"="n"),
                                   edge.attribute=list("source"="funeral")),
               "Value for gender is not found in dataset")

  expect_error(subset.epi_contacts(x,
                                   node.attribute=list("gender"="f"),
                                   edge.attribute=list("source"="funera")),
               "Value for source is not found in dataset")

  expect_error(subset.epi_contacts(x,
                                   node.attribute=c("gender"),
                                   edge.attribute=list("source"="funeral")),
               "node.attribute is not a list")

  expect_error(subset.epi_contacts(x,
                                   node.attribute=list("gender"),
                                   edge.attribute=c("source"="funeral")),
               "edge.attribute is not a list")

  expect_error(subset.epi_contacts(x,
                                   node.attribute=list("gender"="f","date.of.infection"=c("2014-04-08","2015-03-28")),
                                   edge.attribute=list("source"="funeral")),
               "date.of.infection must be provided as a date object")

  expect_error(subset.epi_contacts(x,
                                   node.attribute=list("gender"="f",
                                                       "date.of.infection"=as.Date(c("1990-04-08","1990-03-28"))),
                                   edge.attribute=list("source"="funeral")),
               "Dates provided for date.of.infection fall outside the range of the dataset")

  expect_warning(subset.epi_contacts(x,
                                     node.attribute=list("gender"="f",
                                     "date.of.infection"=as.Date(c("2014-04-08","2014-04-08","2014-04-08"))),
                                     edge.attribute=list("source"="funeral")),
               "More than two date values provided for date.of.infection, using first two")

  expect_warning(subset.epi_contacts(x),
                "No subsetting attributes provided, returning unmodified epi.contact object")

})

test_that("Returns epi_contacts object subsetted correctly", {

  skip_on_cran()

  x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
                         id="case.id", to="case.id", from="infector",
                         directed=FALSE)

  dates <- as.Date(c("2014-04-08","2015-03-28"))

  y <- subset.epi_contacts(x,
                           node.attribute=list("gender"="f",
                                               "date.of.infection"=dates),
                           edge.attribute=list("source"="funeral"))

  expect_is(y,"epi_contacts")
  expect_true(all(y$linelist$gender=="f") &
              min(y$linelist$date.of.infection) >= dates[1] &
              max(y$linelist$date.of.infection) <= dates[2] &
              all(y$contacts$source=="funeral"))

})
