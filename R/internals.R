
## assert_xxx functions make basic tests on 'xxx' against the data ('x', an
## epicontacts object), and return a possibly processed 'xxx'.

## author: Thibaut Jombart

assert_node_color <- function(x, node_color) {
  if (length(node_color) > 1L) {
    stop("'node_color' must indicate a single node attribute")
  }
  if (is.logical(node_color) && !node_color) {
    node_color <- NULL
  }
  if (!is.null(node_color)) {
    if (is.numeric(node_color)) {
      node_color <- names(x$linelist)[node_color]
    }

    if (!node_color %in% names(x$linelist)) {
      msg <- sprintf("node_color '%s' is not in the linelist", node_color)
      stop(msg)
    }
  }

  return(node_color)
}






assert_node_shape <- function(x, node_shape) {

  if (length(node_shape) > 1L) {
    stop("'node_shape' must indicate a single node attribute")
  }
  if (is.logical(node_shape) && !node_shape) {
    node_shape <- NULL
  }
  if (!is.null(node_shape)) {
    if (is.numeric(node_shape)) {
      node_shape <- names(x$linelist)[node_shape]
    }

    if (!node_shape %in% names(x$linelist)) {
      msg <- sprintf("node_shape '%s' is not in the linelist", node_shape)
      stop(msg)
    }
  }

  return(node_shape)
}





assert_annot <- function(x, annot) {
  if (is.logical(annot) && sum(annot) == 0L) {
    annot <- NULL
  }
  if (!is.null(annot)) {
    if (is.numeric(annot) || is.logical(annot)) {
      annot <- names(x$linelist)[annot]
    }

    if (!all(annot %in% names(x$linelist))) {
      culprits <- annot[!annot %in% names(x$linelist)]
      culprits <- paste(culprits, collapse = ", ")
      msg <- sprintf("Annot '%s' is not in the linelist", culprits)
      stop(msg)
    }
  }

  return(annot)
}






assert_edge_label <- function(x, edge_label) {
  if (length(edge_label) > 1L) {
    stop("'edge_label' must indicate a single edge attribute")
  }
  if (is.logical(edge_label) && !edge_label) {
    edge_label <- NULL
  }
  if (!is.null(edge_label)) {
    if (is.numeric(edge_label)) {
      edge_label <- names(x$contacts)[edge_label]
    }

    if (!edge_label %in% names(x$contacts)) {
      msg <- sprintf("edge_label '%s' is not in the contacts", edge_label)
      stop(msg)
    }
  }

  return(edge_label)
}






assert_edge_color <- function(x, edge_color) {
  if (length(edge_color) > 1L) {
    stop("'edge_color' must indicate a single edge attribute")
  }
  if (is.logical(edge_color) && !edge_color) {
    edge_color <- NULL
  }
  if (!is.null(edge_color)) {
    if (is.numeric(edge_color)) {
      edge_color <- names(x$contacts)[edge_color]
    }

    if (!edge_color %in% names(x$contacts)) {
      msg <- sprintf("edge_color '%s' is not in the contacts", edge_color)
      stop(msg)
    }
  }

  return(edge_color)
}
