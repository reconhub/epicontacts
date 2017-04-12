context("vis_epicontacts objects")

test_that("Plotting groups as color", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
    id = "case.id", to = "case.id", from = "infector",
    directed = FALSE)
  x <- thin(x[1:100], 2)
  y <- x
  y$directed <- TRUE

  vis1 <- vis_epicontacts(x, group = "gender")
  vis2 <- vis_epicontacts(x, group = 7,
                          selector = FALSE, editor = TRUE)
  vis3 <- vis_epicontacts(y)
  vis4 <- vis_epicontacts(y, group = "gender", selector = FALSE, editor = FALSE)
  vis5 <- vis_epicontacts(y, group = "gender", selector = TRUE, editor = TRUE)
  vis6 <- vis_epicontacts(y, group = NULL)
  vis7 <- vis_epicontacts(y, annot = NULL)

  expect_equal(vis1$x$byselection$variable, "gender")
  expect_equal_to_reference(vis1, file = "rds/vis1.rds")
  expect_equal_to_reference(vis2, file = "rds/vis2.rds")
  expect_equal_to_reference(vis3, file = "rds/vis3.rds")
  expect_equal_to_reference(vis4, file = "rds/vis4.rds")
  expect_equal_to_reference(vis5, file = "rds/vis5.rds")
  expect_equal_to_reference(vis5, file = "rds/vis6.rds")
  expect_equal_to_reference(vis5, file = "rds/vis7.rds")

})






test_that("Returns errors as planned", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
    id = "case.id", to = "case.id", from = "infector",
    directed = FALSE)
  x <- thin(x[1:100], 2)
  x <- thin(x)

  msg <- "Group 'sex' is not in the linelist"
  expect_error(vis_epicontacts(x, group = "sex"), msg)

  msg <- "Annot 'toto, caca' is not in the linelist"
  expect_error(vis_epicontacts(x, annot = c("id", "toto", "caca")),
               msg)

  msg <- "'group' must indicate a single node attribute"
  expect_error(vis_epicontacts(x, group = c(1:3)), msg)

})


