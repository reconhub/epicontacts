context("Computing number of contacts per case")

test_that("the both argument is working as expected", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
    id="case_id", to="case_id", from="infector",
    directed=TRUE)

  deg_both <- get_degree(x, "both")
  deg_in <- get_degree(x, "in")
  deg_out <- get_degree(x, "out")

  expect_equal((deg_in+deg_out),deg_both)


  msg <- "x is not an 'epicontacts' object"
  expect_error(get_degree("toto"), msg)

})

test_that("get_degree is producing a named vector that is not null", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
    id="case_id", to="case_id", from="infector",
    directed=TRUE)

  deg <- get_degree(x)

  expect_gt(length(deg), 0)
  expect_gt(length(names(deg)), 0)

})

test_that("degrees computed correctly", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
    id="case_id", to="case_id", from="infector",
    directed=TRUE)

  deg_in_ll <- get_degree(x, "in", only_linelist = T)
  deg_in_all <- get_degree(x, "in", only_linelist = F)
  deg_out_ll <- get_degree(x, "out", only_linelist = T)
  deg_out_all <- get_degree(x, "out", only_linelist = F)

  expect_equal(names(deg_in_ll), get_id(x, "linelist"))
  expect_equal(names(deg_in_all), get_id(x, "all"))
  expect_equal(names(deg_out_ll), get_id(x, "linelist"))
  expect_equal(names(deg_out_all), get_id(x, "all"))

  ## Manual degree calculations
  man_in_ll <- table(factor(x$contacts$to, levels = get_id(x, "linelist")))
  man_in_all <- table(factor(x$contacts$to, levels = get_id(x, "all")))
  man_out_ll <- table(factor(x$contacts$from, levels = get_id(x, "linelist")))
  man_out_all <- table(factor(x$contacts$from, levels = get_id(x, "all")))

  expect_equal(as.numeric(man_in_ll), as.numeric(deg_in_ll))
  expect_equal(as.numeric(man_in_all), as.numeric(deg_in_all))
  expect_equal(as.numeric(man_out_ll), as.numeric(deg_out_ll))
  expect_equal(as.numeric(man_out_all), as.numeric(deg_out_all))    

})
