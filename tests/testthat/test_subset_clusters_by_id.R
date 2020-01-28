context("Subsetting clusters by ID")

test_that("Returns clusters with the correct ids", {

    skip_on_cran()

    x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                           id = "case_id", to = "case_id", from = "infector",
                           directed = FALSE)

    test.ids <- ebola_sim$linelist$case_id[1:1000]

    y <- subset_clusters_by_id(x,id = test.ids)

    expect_equal(sum(y$linelist$id %in% test.ids), 1000)

  ## make 10 clusters of size 10
  ids <- 1:100
  linelist <- data.frame(ids = ids)
  contacts <- data.frame(from = ids[ids %% 10 != 0],
                         to = ids[ids %% 10 != 1])
  x <- make_epicontacts(linelist, contacts)

  ## test clusters are correctly identified
  y <- subset_clusters_by_id(x, c(1, 51))
  expect_equal(get_id(y, 'linelist'), c(1:10, 51:60))
  expect_equal(sort(get_id(y, 'contacts')), c(1:10, 51:60))

})
