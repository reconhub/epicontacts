context("Plotting epicontacts")

test_that("Plots as expected", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                        id = "case_id",
                        to = "case_id",
                        from = "infector",
                        directed=FALSE)
  x <- thin(x[1:100], 2)

  plot1 <- plot(x)
  plot2 <- plot(x, thin = FALSE)
  plot3 <- plot(x, "gender")
  plot3bis <- plot(x, 8L)
  plot4 <- plot(x, "hospital", editor = TRUE)
  plot5 <- plot(x, "hospital", method = "graph3D")
  plot6 <- plot(x, "hospital", method = "graph3D",
                node_size = 3, edge_size = 2)

  ## Expect_equal_to_reference(plot1, file = "rds/plot1.rds")
  ## expect_equal_to_reference(plot2, file = "rds/plot2.rds")
  ## expect_equal_to_reference(plot3, file = "rds/plot3.rds")
  ## expect_equal_to_reference(plot3bis, file = "rds/plot3.rds")
  ## expect_equal_to_reference(plot4, file = "rds/plot4.rds")
  ## expect_equal_to_reference(plot5, file = "rds/plot5.rds")
  ## expect_equal_to_reference(plot6, file = "rds/plot6.rds")

})






test_that("Errors as expected", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                        id = "case_id",
                        to = "case_id",
                        from = "infector",
                        directed=FALSE)

  expect_error(plot(x, node_color = "foobar"))

})
