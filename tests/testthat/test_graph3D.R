context("3D Graph")

test_that("graph3D produces list that is not null", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
    id="case_id", to="case_id", from="infector",
    directed=TRUE)

  g <- graph3D(x)
  expect_is(g, "scatterplotThree")
  expect_is(g, "htmlwidget")
  expect_is(g$x, "list")

})

test_that("graph3D errors as expected on bad annotation and group specification", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                        id="case_id", to="case_id", from="infector",
                        directed=TRUE)

  msg <- "Annot 'toto, caca' is not in the linelist"
  expect_error(graph3D(x, annot = c("id", "toto", "caca")),
               msg)

  msg <- "node_color 'foobar' is not in the linelist"
  expect_error(graph3D(x, node_color = "foobar"),
               msg)

  msg <- "'node_color' must indicate a single node attribute"
  expect_error(graph3D(x, node_color = c(1:3)), msg)


  expect_equal(graph3D(x, node_color = NULL)$x$color,
               graph3D(x, node_color = FALSE)$x$color)

  expect_equal(graph3D(x, node_color = 1)$x$color,
               graph3D(x, node_color = "id")$x$color)

  expect_equal(graph3D(x, annot = NULL)$x$labels,
               graph3D(x, annot = FALSE)$x$labels)

  expect_equal(graph3D(x, annot = 1:2)$x$labels,
               graph3D(x, annot = c("id","generation"))$x$labels)

})

test_that("graph3D object includes annotation", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                        id="case_id", to="case_id", from="infector",
                        directed=TRUE)

  g <- graph3D(x, annot = c("date_of_infection", "outcome"))
  expect_true(all(grepl("date_of_infection", g$x$labels)))

})
