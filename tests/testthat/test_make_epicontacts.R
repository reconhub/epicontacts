context("Converting data to epicontacts using make_epicontacts")

test_that("Class and content are fine", {
  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist,
                        ebola_sim$contacts)

  expect_is(x, "epicontacts")
  expect_is(x$linelist$id, "character")

})






test_that("Errors happen when they should", {
  skip_on_cran()

  ## linelist tests

  msg <- "linelist is NULL"
  expect_error(make_epicontacts(linelist = NULL,
                                contacts = ebola_sim$contacts),
               msg)

  msg <- "linelist is NA"
  expect_error(make_epicontacts(linelist = NA,
                                contacts = ebola_sim$contacts),
               msg)

  msg <- "linelist should have at least one row"
  expect_error(make_epicontacts(linelist = matrix(character(0)),
                                contacts = ebola_sim$contacts),
               msg)

  msg <- "Duplicated IDs detected in the linelist; culprits are: d1fafd f5c3d8"
  expect_error(make_epicontacts(ebola_sim$linelist[c(1,1,2,3,3,4,5,6),],
                                ebola_sim$contacts),
               msg)




  ## contacts tests

  msg <- "contacts is NULL"
  expect_error(make_epicontacts(linelist = ebola_sim$linelist,
                                contacts = NULL),
               msg)

  msg <- "contacts is NA"
  expect_error(make_epicontacts(linelist = ebola_sim$linelist,
                                contacts = NA),
               msg)

  msg <- "contacts should have at least one row"
  expect_error(make_epicontacts(linelist = ebola_sim$linelist,
                                contacts = data.frame(character(0))),
               msg)

  msg <- "contacts should have at least two columns"
  expect_error(make_epicontacts(linelist = ebola_sim$linelist,
                                contacts = data.frame(from = 1:100)),
               msg)

})






test_that("Reordering of columns works", {
  ## reverse data order

  linelist <- ebola_sim$linelist[, rev(seq_len(ncol(ebola_sim$linelist)))]
  contacts <- ebola_sim$contacts[, rev(seq_len(ncol(ebola_sim$contacts)))]


  ## make object

  x <- make_epicontacts(linelist, contacts,
                        id = "case_id",
                        to = "case_id",
                        from = "infector")

  ## tests
  expect_equal(names(x$linelist)[1], "id")
  expect_equal(x$linelist$id, ebola_sim$linelist$case_id)
  expect_equal(names(x$contacts)[c(1,2)], c("from","to"))
  expect_equal(x$contacts$from, ebola_sim$contacts$infector)
  expect_equal(x$contacts$to, ebola_sim$contacts$case_id)

})






test_that("Constructor works with factors", {

  skip_on_cran()

  ## make data
  contacts <- ebola_sim$contacts
  contacts$infector <- factor(contacts$infector)
  contacts$case_id <- factor(contacts$case_id)


  ## run tests

  ref <- make_epicontacts(ebola_sim$linelist,
                          ebola_sim$contacts)

  x <-  make_epicontacts(ebola_sim$linelist,
                         contacts)

  expect_identical(ref, x)

})
