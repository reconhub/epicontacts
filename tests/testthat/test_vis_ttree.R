context("Plotting using vis_ttree")


test_that("Plotting groups as color", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
    id = "case_id", to = "case_id", from = "infector",
    directed = FALSE)
  x <- thin(x[1:300], 2)
  y <- x
  y$directed <- TRUE

  vis1 <- vis_epicontacts(x, node_color = "gender")
  vis2 <- vis_epicontacts(x, node_color = 7,
                          selector = FALSE, editor = TRUE)
  vis3 <- vis_epicontacts(y)
  vis4 <- vis_epicontacts(y, node_color = "gender", selector = FALSE, editor = FALSE)
  vis5 <- vis_epicontacts(y, node_color = "gender", selector = TRUE, editor = TRUE)
  vis6 <- vis_epicontacts(y, node_color = NULL)
  vis7 <- vis_epicontacts(y, annot = NULL)
  vis8 <- vis_epicontacts(y, node_color = 7, node_shape = "gender",
                          shapes = c(f = "female", m = "male"))

  expect_equal(vis1$x$byselection$variable, "gender")
  ## expect_equal_to_reference(vis1, file = "rds/vis1.rds")
  ## expect_equal_to_reference(vis2, file = "rds/vis2.rds")
  ## expect_equal_to_reference(vis3, file = "rds/vis3.rds")
  ## expect_equal_to_reference(vis4, file = "rds/vis4.rds")
  ## expect_equal_to_reference(vis5, file = "rds/vis5.rds")
  ## expect_equal_to_reference(vis6, file = "rds/vis6.rds")
  ## expect_equal_to_reference(vis7, file = "rds/vis7.rds")

  expect_equal(vis_epicontacts(x, node_color = NULL),
               vis_epicontacts(x, node_color = FALSE))
  expect_equal(vis_epicontacts(x, annot = NULL),
               vis_epicontacts(x, annot = FALSE))

})






