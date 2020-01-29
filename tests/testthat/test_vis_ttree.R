context("Plotting using vis_ttree")


test_that("Testing get_coor", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                        id = "case_id", to = "case_id", from = "infector",
                        directed = FALSE)
  x <- thin(x[1:300], 2)

  n <- nrow(x$linelist)
  
  coor1 <- get_coor(x, 'date_of_onset', axis_type = 'none')
  coor2 <- get_coor(x, 'date_of_onset', axis_type = 'single')
  coor3 <- get_coor(x, 'date_of_onset', axis_type = 'double')
  
  ## check one y-coordinate for each node, plus for axes
  expect_length(coor1$y, n)
  expect_length(coor2$y, n+1)
  expect_length(coor3$y, n+2)

  ## unique y-coordinates when position_dodge = TRUE
  coor4 <- get_coor(x, 'date_of_onset', position_dodge = TRUE)
  expect_true(all(!duplicated(coor4$y)))

})



test_that("Testing vis_ttree", {

  skip_on_cran()

  ## load data
  linelist <- readRDS("rds/test_linelist.rds")
  contacts <- readRDS("rds/test_contacts.rds")

  ## form epicontacts with character ids
  expect_warning(x <- make_epicontacts(linelist,
                                       contacts,
                                       id = 'character',
                                       from = 'character_from',
                                       to = 'character_to',
                                       directed = TRUE,
                                       na_rm_linelist = FALSE,
                                       na_rm_contacts = FALSE),
                 "NA")
  x$linelist$character <- linelist$character
  x$contacts$character_from <- contacts$character_from
  x$contacts$character_to <- contacts$character_to

  ## form epicontacts with numeric ids
  expect_warning(y <- make_epicontacts(linelist,
                                       contacts,
                                       id = 'numeric',
                                       from = 'numeric_from',
                                       to = 'numeric_to',
                                       directed = TRUE,
                                       na_rm_linelist = FALSE,
                                       na_rm_contacts = FALSE),
                 "NA")
  y$linelist$numeric <- linelist$numeric
  y$contacts$numeric_from <- contacts$numeric_from
  y$contacts$numeric_to <- contacts$numeric_to

  ## test numerical and character ids
  for(z in list(x, y)) {
    
    ## test no errors thrown when node attributes and edge attributes mapped to
    ## character, date, factor, numeric and integer
    classes <- c('character', 'date', 'factor', 'numeric', 'integer')
    for(i in classes) {
      for(j in paste0(classes, "_from")) {
        expect_error(vis_ttree(z,
                               x_axis = 'date',
                               node_color = i,
                               node_order = i,
                               root_order = i,
                               edge_color = j,
                               edge_label = j),
                     NA)
      }
    }

    ## error when mapping node_size to character
    expect_error(vis_ttree(z,
                           x_axis = 'date',
                           node_size = 'character',
                           node_color = 'character',
                           node_order = 'character',
                           root_order = 'character'),
                 'node_size cannot be mapped to character variable')

    ## error when mapping edge_width to character
    expect_error(vis_ttree(z,
                           x_axis = 'date',
                           edge_width = 'character_from'),
                 'edge_width cannot be mapped to character variable')

    ## error when mapping edge_linetype to character
    expect_error(vis_ttree(z,
                           x_axis = 'date',
                           edge_linetype = 'character_from'),
                 paste0("visNetwork only supports two linetypes; ",
                        "use binary variable or set method = 'ggplot'."))

    ## testing igraph_type
    expect_error(vis_ttree(z, x_axis = 'date', igraph_type = 'rt'), NA)
    expect_error(vis_ttree(z, x_axis = 'date', igraph_type = 'fr'), NA)
    expect_error(vis_ttree(z, x_axis = 'date', igraph_type = 'sugiyama'), NA)

    ## test custom parent_pos
    expect_error(vis_ttree(x, 'date', custom_parent_pos = function(n) n),
                 paste0("custom_parent_pos must be a function of n",
                        " returning a numeric vector of length n"))

  }

  ## check x_axis must be numeric or date
  msg <- "x_axis must indicate a Date, numeric or integer value"
  expect_error(vis_ttree(x, x_axis = 'date'), NA)
  expect_error(vis_ttree(x, x_axis = 'integer'), NA)
  expect_error(vis_ttree(x, x_axis = 'numeric'), NA)
  expect_error(vis_ttree(x, x_axis = 'character'), msg)
  expect_error(vis_ttree(x, x_axis = 'factor'), msg)

  
})



test_that("Testing vis_ggplot", {

  skip_on_cran()

  ## load data
  linelist <- readRDS("rds/test_linelist.rds")
  contacts <- readRDS("rds/test_contacts.rds")

  ## form epicontacts with character ids
  expect_warning(x <- make_epicontacts(linelist,
                                       contacts,
                                       id = 'character',
                                       from = 'character_from',
                                       to = 'character_to',
                                       directed = TRUE,
                                       na_rm_linelist = FALSE,
                                       na_rm_contacts = FALSE),
                 "NA")
  x$linelist$character <- linelist$character
  x$contacts$character_from <- contacts$character_from
  x$contacts$character_to <- contacts$character_to

  ## form epicontacts with numeric ids
  expect_warning(y <- make_epicontacts(linelist,
                                       contacts,
                                       id = 'numeric',
                                       from = 'numeric_from',
                                       to = 'numeric_to',
                                       directed = TRUE,
                                       na_rm_linelist = FALSE,
                                       na_rm_contacts = FALSE),
                 "NA")
  y$linelist$numeric <- linelist$numeric
  y$contacts$numeric_from <- contacts$numeric_from
  y$contacts$numeric_to <- contacts$numeric_to

  ## test numerical and character ids
  for(z in list(x, y)) {
    
    ## test no errors thrown when node attributes and edge attributes mapped to
    ## character, date, factor, numeric and integer
    classes <- c('character', 'date', 'factor', 'numeric', 'integer')
    for(i in classes) {
      for(j in paste0(classes, "_from")) {
        expect_error(vis_ggplot(z,
                                x_axis = 'date',
                                node_color = i,
                                node_order = i,
                                root_order = i,
                                edge_color = j,
                                edge_label = j),
                     NA)
      }
    }

    ## error when mapping node_size to character
    expect_error(vis_ggplot(z,
                            x_axis = 'date',
                            node_size = 'character',
                            node_color = 'character',
                            node_order = 'character',
                            root_order = 'character'),
                 'node_size cannot be mapped to character variable')

    ## no error when mapping edge_linetype to character in ggplot
    expect_error(vis_ggplot(z,
                            x_axis = 'date',
                            edge_linetype = 'character_from'),
                 NA)

    ## testing igraph_type
    expect_error(vis_ggplot(z, x_axis = 'date', igraph_type = 'rt'), NA)
    expect_error(vis_ggplot(z, x_axis = 'date', igraph_type = 'fr'), NA)
    expect_error(vis_ggplot(z, x_axis = 'date', igraph_type = 'sugiyama'), NA)

    ## test custom parent_pos
    expect_error(vis_ggplot(x, 'date', custom_parent_pos = function(n) n),
                 paste0("custom_parent_pos must be a function of n",
                        " returning a numeric vector of length n"))

  }
  
})
