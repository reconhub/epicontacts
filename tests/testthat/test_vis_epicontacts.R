context("vis_epicontacts objects")

test_that("Plotting groups as color", {
  
  skip_on_cran()
  
  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
    id = "case.id", to = "case.id", from = "infector",
    directed = FALSE)
  x <- thin(x[1:100], 2)
  y <- x
  y$directed <- TRUE
  
  plot1 <- vis_epicontacts(x, group = "gender")
  plot2 <- vis_epicontacts(x, group = "gender",
                           selector = FALSE, editor = TRUE)
  plot3 <- vis_epicontacts(y)
 
  expect_equal(plot1$x$byselection$variable, "gender")
  expect_equal_to_reference(plot1, file = "rds/plot1.rds")
  expect_equal_to_reference(plot2, file = "rds/plot2.rds")
  expect_equal_to_reference(plot3, file = "rds/plot3.rds")

})






test_that("Returns errors as planned", {
  
  skip_on_cran()
  
  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
    id = "case.id", to = "case.id", from = "infector",
    directed = FALSE)

  msg <- "Group 'sex' is not in the linelist"
  expect_error(vis_epicontacts(x, group = "sex"), msg)

  msg <- "Annot 'toto, caca' is not in the linelist"
  expect_error(vis_epicontacts(x, annot = c("id", "toto", "caca")),
               msg)

})


