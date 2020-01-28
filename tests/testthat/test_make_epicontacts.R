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






test_that("Warnings happen when they should", {
  
  skip_on_cran()

  ## test NAs in linelist IDs
  linelist <- data.frame(id = c(1, 2, NA, 3))
  contacts <- data.frame(from = c(1, 1), to = c(2, 3))
  ## remove NAs
  msg <- "Removed 1 linelist element(s) with NA IDs"
  expect_warning(x <- make_epicontacts(linelist, contacts, na_rm_linelist = TRUE),
                 msg, fixed = TRUE)
  expect_false(any(is.na(unlist(x$linelist$id))))
  ## keep NAs
  msg <- "1 NA ID in the linelist has renamed to NA_1"
  expect_warning(x <- make_epicontacts(linelist, contacts, na_rm_linelist = FALSE),
                 msg, fixed = TRUE)
  expect_false(any(is.na(unlist(x$linelist$id))))

  ## test NAs in contact IDs
  linelist <- data.frame(id = 1:4)
  contacts <- data.frame(from = c(1, 1), to = c(2, NA))
  ## remove NAs
  msg <- "Removed 1 contact(s) with NA IDs"
  expect_warning(x <- make_epicontacts(linelist, contacts, na_rm_contacts = TRUE),
                 msg, fixed = TRUE)
  expect_false(any(is.na(unlist(x$contacts))))
  ## keep NAs
  msg <- "1 NA ID in the contacts has renamed to NA_1"
  expect_warning(x <- make_epicontacts(linelist, contacts, na_rm_contacts = FALSE),
                 msg, fixed = TRUE)
  expect_false(any(is.na(unlist(x$contacts))))
  
  ## test self-contacts
  linelist <- data.frame(id = 1:4)
  contacts <- data.frame(from = 1, to = 1)
  msg <- paste0("The contact(s) listed on row(s) 1 are between ",
                "a case and itself: this may be unwanted")
  expect_warning(make_epicontacts(linelist, contacts), msg, fixed = TRUE)

  ## test duplicated contacts
  linelist <- data.frame(id = 1:4)
  contacts <- data.frame(from = c(1, 1), to = c(2, 2))
  msg <- paste0("The contact(s) listed on row(s) 2 are duplicates: ",
                "this may be unwanted")
  expect_warning(make_epicontacts(linelist, contacts), msg, fixed = TRUE)

  ## test loops
  linelist <- data.frame(id = 1:4)
  contacts <- data.frame(from = c(1, 2), to = c(2, 1))
  msg <- paste0("Cycle(s) detected in the contact network: this may be unwanted")
  expect_warning(make_epicontacts(linelist, contacts), msg, fixed = TRUE)

  ## test cycles
  linelist <- data.frame(id = 1:4)
  contacts <- data.frame(from = c(1, 2, 3), to = c(2, 3, 1))
  msg <- paste0("Cycle(s) detected in the contact network: this may be unwanted")
  expect_warning(make_epicontacts(linelist, contacts), msg, fixed = TRUE)

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






test_that("ID classes are identical between linelist and contacts", {

  skip_on_cran()

  ## load data
  linelist <- readRDS("rds/test_linelist.rds")
  contacts <- readRDS("rds/test_contacts.rds")

  ## possible classes
  classes <- c("factor", "integer", "numeric", "date", "character")
  comb <- expand.grid(id = classes,
                      from = paste0(classes, "_from"),
                      to = paste0(classes, "_to"),
                      stringsAsFactors = FALSE)

  ## try all combinations of classes for 'id', 'from' and 'to' and test that all
  ## IDs in the epicontacts object share the same class - expect warning
  ## regarding NA IDs in constructor
  test_class <- function(id, from, to, linelist, contacts) {
    net <- expect_warning(make_epicontacts(linelist, contacts, id, from, to),
                          "NA")
    all_classes <- c(class(net$linelist$id),
                     class(net$contacts$from),
                     class(net$contacts$to))
    expect_length(unique(all_classes), 1L)
  }

  ## test whether all IDs share the same class
  mapply(test_class, comb$id, comb$from, comb$to,
         MoreArgs = list(linelist, contacts))

})
