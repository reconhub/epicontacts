
#' Color tools and palettes for epicontacts
#'
#' These functions are used for defining palettes or colors in the
#' \code{epicontacts} package. They include:
#'
#' \itemize{
#'
#' \item \code{cases_pal}: discrete color palette used for cases (comes from the
#' \code{dibbler} package)
#'
#' \item \code{spectral}: continuous color palette (comes from the
#' \code{adegenet} package)
#'
#' \item \code{transp}: makes colors transparent (comes from the
#' \code{adegenet} package)
#'
#' \item \code{fac2col}: translates a character or a factor to a color using a
#' palette (comes from the \code{adegenet} package)
#'
#' }
#'
#'
#' @export
#'
#' @rdname colors
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @param n An integer indicating the number of colors.
#'
#' @examples
#'
#' barplot(1:5, col = cases_pal(5))
#' barplot(1:50, col = cases_pal(50))
#'






#' @param col A color vector to which transparency should be added.
#'
#' @param alpha The threshold to be used for transparency: 0 for full
#' transparency, and 1 for full opacity.

transp <- function(col, alpha = .5){
    res <- apply(grDevices::col2rgb(col), 2,
                 function(c)
                 grDevices::rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
    return(res)
}






#' @export
#' @rdname colors

edges_pal <- function(n){
  if(!is.numeric(n)) stop("n is not a number")
  colors <- c("#cc6666", "#ff8566", "#ffb366","#33cccc",
              "#85e0e0", "#adc2eb", "#9f9fdf","#666699")
  return(transp(grDevices::colorRampPalette(colors)(n), .7))
}





#' @export
#' @rdname colors

cases_pal <- function(n){
  if (!is.numeric(n)) {
    stop("n is not a number")
  }
  n <- as.integer(n)
  colors <- c("#ccddff", "#79d2a6", "#ffb3b3",
              "#a4a4c1", "#ffcc00", "#ff9f80",
              "#ccff99", "#df9fbf", "#ffcc99",
              "#cdcdcd")
  if (n < length(colors)) {
    return(colors[seq_len(n)])
  } else {
    return(grDevices::colorRampPalette(colors)(n))
  }
}






#' @export
#' @rdname colors

spectral <- grDevices::colorRampPalette(
    c("#D53E4F","#F46D43","#FDAE61",
      "#FEE08B","#FFFFBF","#E6F598",
      "#ABDDA4","#66C2A5","#3288BD"))






#' @export
#' @rdname colors
#'
#' @param x A character or a factor to be converted to colors.
#'
#' @param pal A color palette. Can be a function accepting a single number n and
#'   returning a vector of n colors, or a named character vector matching all
#'   factor levels in x to a color.
#'
#' @param NA_col The color to be used for NA values.
#'
#' @param legend A logical indicating if legend info should be added to the
#'   output. If TRUE, the output will be a list, with colors in the
#'   \code{$color} component.
#'
fac2col <- function (x, pal = cases_pal, NA_col = "lightgrey", legend = FALSE){
  x <- factor(x)
  lev <- levels(x)
  nlev <- length(lev)
  res <- rep(NA_col, length(x))
  if(inherits(pal, "function")) {
    col <- pal(nlev)
    res[!is.na(x)] <- col[as.integer(x[!is.na(x)])]
    if (legend) {
      res <- list(color = res, leg_col = col, leg_lab = lev)
    }
  } else if(is.vector(pal, mode = "character") & !is.null(names(pal))) {
    if(!all(lev %in% names(pal))) {
      msg <- paste0("col_pal/edge_col_pal must specify a color for ",
                    "all elements in node_color/edge_color")
      stop(msg)
    }
    res[!is.na(x)] <- pal[x[!is.na(x)]]
    if (legend) {
      res <- list(color = res, leg_col = unname(pal[lev]), leg_lab = lev)
    }
  } else {
    stop("col_pal and edge_col_pal must be a function or named character vector")
  }
  return(res)
}

