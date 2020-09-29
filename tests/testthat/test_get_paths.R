context("Test get_paths")

test_that("test that return values are correct", {

  skip_on_cran()

  net <- readRDS("rds/test_net.rds")

  ## a list of character vectors if IDs on shortest path
  path1 <- get_path(net,
                    c("qotje", "zrmrj", "yauxa"),
                    c("lxyer", "lisgc", "mqmlu"),
                    which = "shortest",
                    output = "ids")

  expect_is(path1[[1]], "character")
  expect_is(path1, "list")
  expect_length(path1, 3L)

  ## a list of list of character IDs if IDs on all paths
  path2 <- get_path(net,
                    c("qotje", "zrmrj", "yauxa"),
                    c("lxyer", "lisgc", "mqmlu"),
                    which = "all",
                    output = "ids")

  expect_is(path2[[1]], "list")
  expect_is(path2, "list")
  expect_length(path1, 3L)

  ## a vector of lengths if shortest lengths
  path3 <- get_path(net,
                    c("qotje", "zrmrj", "yauxa"),
                    c("lxyer", "lisgc", "mqmlu"),
                    which = "shortest",
                    output = "generations")

  expect_is(path3, "numeric")
  expect_length(path3, 3L)

  ## a list of lengths if all lengths
  path3 <- get_path(net,
                    c("qotje", "zrmrj", "yauxa"),
                    c("lxyer", "lisgc", "mqmlu"),
                    which = "all",
                    output = "generations")

  expect_is(path3[[1]], "numeric")
  expect_is(path3, "list")
  expect_length(path3, 3L)

  ## return list if simplify is FALSE
  path4 <- get_path(net, "NA_5", "NA_6",
                    which = "shortest",
                    output = "ids",
                    simplify = FALSE)

  expect_is(path4, "list")
  expect_length(path4, 1L)

  ## return character vector if simplify is TRUE
  path4 <- get_path(net, "NA_7", "cilrw",
                    which = "shortest",
                    output = "ids",
                    simplify = FALSE)

  expect_is(path4, "list")
  expect_length(path4, 1L)

  ## return character vector if simplify is TRUE
  path5 <- get_path(net, "NA_7", "cilrw",
                    which = "shortest",
                    output = "ids",
                    simplify = TRUE)

  expect_is(path5, "character")

})



test_that("test that correct errors are thrown", {

  skip_on_cran()

  net <- readRDS("rds/test_net.rds")

  expect_error(
    get_path(net, "error", "dyzvt"),
    "The following IDs are not found in the epicontacts object: error"
  )

  expect_error(
    get_path(net, c("asnfo", "hwqga"), "dyzvt"),
    "from and to must be the same length"
  )

})
