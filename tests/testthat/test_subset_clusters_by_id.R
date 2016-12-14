context("Subsetting clusters by ID")

test_that("Returns clusters with the correct ids", {

    skip_on_cran()

    x <- make_epicontacts(ebola.sim$linelist, ebola.sim$contacts,
                           id="case.id", to="case.id", from="infector",
                           directed=FALSE)

    test.ids <- ebola.sim$linelist$case.id[1:1000]

    y <- subset_clusters_by_id(x,id=test.ids)

    expect_equal(sum(y$linelist$id %in% test.ids),1000)

})
