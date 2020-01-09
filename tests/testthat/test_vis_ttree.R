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
  expect_equal(length(coor1$y), n)
  expect_equal(length(coor2$y), n+1)
  expect_equal(length(coor3$y), n+2)

  ## unique y-coordinates when position_dodge = TRUE
  coor4 <- get_coor(x, 'date_of_onset', position_dodge = TRUE)
  expect_true(all(!duplicated(coor4$y)))

})



test_that("Testing vis_ttree", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                        id = "case_id", to = "case_id", from = "infector",
                        directed = FALSE)
  x <- thin(x[1:300], 2)
  x$linelist$hospital <- as.character(x$linelist$hospital)
  x$contacts$date <- as.Date(runif(nrow(x$contacts), 0, 100),
                             origin = "2000-01-01")
  x$contacts$duration <- runif(nrow(x$contacts), 0, 100)
  x$contacts$letter <- sample(letters, nrow(x$contacts), TRUE)
  y <- x

  ## insert NAs into all columns to test stability
  ins_na <- function(x, p = 0.1) {
    n <- nrow(x)
    for(i in 1:ncol(x)) {
      x[sample(1:n, rbinom(1, n, p)), i] <- NA
    }
    return(x)
  }
  
  x$linelist <- ins_na(x$linelist)
  x$contacts <- ins_na(x$contacts)

  ## numeric ids
  y$contacts$from <- match(y$contacts$from, y$linelist$id)
  y$contacts$to <- match(y$contacts$to, y$linelist$id)
  y$linelist$id <- match(y$linelist$id, y$linelist$id)
  y$linelist <- ins_na(y$linelist)
  y$contacts <- ins_na(y$contacts)

  ## test numerical and character ids
  for(z in list(x, y)) {
    
    ## test no errors thrown when node attributes and edge attributes mapped to
    ## character, date, factor and numeric
    for(i in c('id', 'date_of_onset', 'gender', 'generation')) {
      for(j in c('from', 'date', 'source', 'duration')) {
        expect_error(vis_ttree(z,
                               x_axis = 'date_of_onset',
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
                           x_axis = 'date_of_onset',
                           node_size = 'hospital',
                           node_color = 'hospital',
                           node_order = 'hospital',
                           root_order = 'hospital'),
                 'node_size cannot be mapped to character variable')

    ## error when mapping edge_width to character
    expect_error(vis_ttree(z,
                           x_axis = 'date_of_onset',
                           edge_width = 'letter'),
                 'edge_width cannot be mapped to character variable')

    ## error when mapping edge_linetype to character
    expect_error(vis_ttree(z,
                           x_axis = 'date_of_onset',
                           edge_linetype = 'letter'),
                 paste0("visNetwork only supports two linetypes; ",
                        "use binary variable or set method = 'ggplot'."))

    ## testing igraph_type
    expect_error(vis_ttree(z, x_axis = 'date_of_onset', igraph_type = 'rt'), NA)
    expect_error(vis_ttree(z, x_axis = 'date_of_onset', igraph_type = 'fr'), NA)
    expect_error(vis_ttree(z, x_axis = 'date_of_onset', igraph_type = 'sugiyama'), NA)

    ## test custom parent_pos
    expect_error(vis_ttree(x, 'date_of_onset', custom_parent_pos = function(n) n),
                 paste0("custom_parent_pos must be a function of n",
                        " returning a numeric vector of length n"))

  }
  
})



test_that("Testing vis_ggplot", {

  skip_on_cran()

  x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
                        id = "case_id", to = "case_id", from = "infector",
                        directed = FALSE)
  x <- thin(x[1:300], 2)
  x$linelist$hospital <- as.character(x$linelist$hospital)
  x$contacts$date <- as.Date(runif(nrow(x$contacts), 0, 100),
                             origin = "2000-01-01")
  x$contacts$duration <- runif(nrow(x$contacts), 0, 100)
  x$contacts$letter <- sample(letters, nrow(x$contacts), TRUE)
  y <- x

  ## insert NAs into all columns to test stability
  ins_na <- function(x, p = 0.1) {
    n <- nrow(x)
    for(i in 1:ncol(x)) {
      x[sample(1:n, rbinom(1, n, p)), i] <- NA
    }
    return(x)
  }
  
  x$linelist <- ins_na(x$linelist)
  x$contacts <- ins_na(x$contacts)

  ## numeric ids
  y$contacts$from <- match(y$contacts$from, y$linelist$id)
  y$contacts$to <- match(y$contacts$to, y$linelist$id)
  y$linelist$id <- match(y$linelist$id, y$linelist$id)
  y$linelist <- ins_na(y$linelist)
  y$contacts <- ins_na(y$contacts)

  ## test numerical and character ids
  for(z in list(x, y)) {
    
    ## test no errors thrown when node attributes and edge attributes mapped to
    ## character, date, factor and numeric
    for(i in c('id', 'date_of_onset', 'gender', 'generation')) {
      for(j in c('from', 'date', 'source', 'duration')) {
        expect_error(vis_ggplot(z,
                                x_axis = 'date_of_onset',
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
                            x_axis = 'date_of_onset',
                            node_size = 'hospital',
                            node_color = 'hospital',
                            node_order = 'hospital',
                            root_order = 'hospital'),
                 'node_size cannot be mapped to character variable')

    ## error when mapping edge_linetype to character
    expect_error(vis_ggplot(z,
                            x_axis = 'date_of_onset',
                            edge_linetype = 'letter'),
                 paste0("visNetwork only supports two linetypes; ",
                        "use binary variable or set method = 'ggplot'."))

    ## testing igraph_type
    expect_error(vis_ggplot(z, x_axis = 'date_of_onset', igraph_type = 'rt'), NA)
    expect_error(vis_ggplot(z, x_axis = 'date_of_onset', igraph_type = 'fr'), NA)
    expect_error(vis_ggplot(z, x_axis = 'date_of_onset', igraph_type = 'sugiyama'), NA)

    ## test custom parent_pos
    expect_error(vis_ggplot(x, 'date_of_onset', custom_parent_pos = function(n) n),
                 paste0("custom_parent_pos must be a function of n",
                        " returning a numeric vector of length n"))

  }
  
})
