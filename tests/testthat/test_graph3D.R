context("3D Graph")

test_that("graph3D produces json that is not null", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
    id="case.id", to="case.id", from="infector",
    directed=TRUE)

  g <- graph3D(x)
  expect_is(g$x, "json")
  expect_gt(length(g$x), 0)
  

})

test_that("graph3D errors as expected on bad annotation and group specification", {
  
  skip_on_cran()
  
  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                        id="case.id", to="case.id", from="infector",
                        directed=TRUE)
  
  msg <- "Annot 'toto, caca' is not in the linelist"
  expect_error(graph3D(x, annot = c("id", "toto", "caca")),
               msg)
  
  msg <- "Group 'foobar' is not in the linelist"
  expect_error(graph3D(x, group = "foobar"),
               msg)
  
  msg <- "'group' must indicate a single node attribute"
  expect_error(graph3D(x, group = c(1:3)), msg)
  

  expect_equal(graph3D(x, group = 1),
               graph3D(x, group = "id"))

  expect_equal(graph3D(x, annot = NULL),
               graph3D(x, annot = FALSE))

  expect_equal(graph3D(x, annot = 1:2),
               graph3D(x, annot = c("id","generation")))
  
})

test_that("graph3D object includes annotation", {
  
  skip_on_cran()
  
  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                        id="case.id", to="case.id", from="infector",
                        directed=TRUE)
  
  g <- graph3D(x, annot = c("date.of.infection", "outcome"))
  expect_true(grepl("date.of.infection", g$x))

})
