#' Plot epicontacts objects using visNetwork
#'
#' This function plots \code{\link{epicontacts}} objects using the
#' \code{visNetwork} package. The produced object is an \code{htmlwidget} which
#' will need rendering within a web browser.
#'
#' @export
#'
#'
#' @author
#' Thibaut Jombart (\email{thibautjombart@@gmail.com})
#' VP Nagraj (\email{vpnagraj@@virginia.edu})
#'
#' @param x An \code{\link{epicontacts}} object.
#'
#' @param group An index or character string indicating which field of the
#'     linelist should be used to color the nodes.
#'
#' @param annot An index, logical, or character string indicating which fields
#'   of the linelist should be used for annotating the nodes. Logical will be
#'   recycled if necessary, so that the default \code{TRUE} effectively uses all
#'   columns of the linelist.
#'
#' @param type An index or character string indicating which field of the
#'     linelist should be used to determine the shapes of the nodes.
#'
#' @param edge_width An integer indicating the width of the edges. Defaults to
#'   3.
#'
#' @param legend A logical indicating whether a legend should be added to the
#'   plot.
#'
#' @param legend_max The maximum number of groups for a legend to be displayed.
#'
#' @param col_pal A color palette for the groups.
#'
#' @param NA_col The color used for unknown group.
#'
#' @param width The width of the output, in html compatible format (e.g. '90\%'
#'   or '800px').
#'
#' @param height The height of the output, in html compatible format
#'   (e.g. '800px').
#'
#' @param selector A logical indicating if the selector tool should be used;
#'   defaults to TRUE.
#'
#' @param editor A logical indicating if the editor tool should be used;
#'   defaults to FALSE.
#'
#' @param ... Further arguments to be passed to \code{visNetwork}.
#'
#'
#' @importFrom magrittr "%>%"
#'
#' @return The same output as \code{visNetwork}.
#'
#' @seealso \code{\link[visNetwork]{visNetwork}} in the package \code{visNetwork}.
#'
#' @examples
#' if (require(outbreaks)) {
#'
#' ## example using MERS outbreak in Korea, 2014
#' head(mers_korea_2015[[1]])
#' head(mers_korea_2015[[2]])
#'
#' x <- make_epicontacts(linelist=mers_korea_2015[[1]],
#'                        contacts = mers_korea_2015[[2]],
#'                        directed=TRUE)
#'
#' \dontrun{
#' plot(x)
#' plot(x, group = "place_infect")
#' plot(x, group = "loc_hosp", legend_max=20, annot=TRUE)
#' }
#' }

vis_epicontacts <- function(x, group = "id", annot  =  TRUE,
                            type = NULL, type_code = NULL,
                            legend = TRUE, legend_max = 10,
                            col_pal = cases_pal, NA_col = "lightgrey",
                            width = "90%", height = "700px",
                            selector = TRUE, editor = FALSE,
                            edge_width = 3, ...){

  ## In the following, we pull the list of all plotted nodes (those from the
  ## linelist, and from the contacts data.frame, and then derive node attributes
  ## for the whole lot. These attributes are in turn used for plotting: as color
  ## ('group' in visNetwork terminology) or as annotations (converted to html
  ## code).


  ## check group (node attribute used for color)
  if (length(group) > 1L) {
    stop("'group' must indicate a single node attribute")
  }
  if (is.logical(group) && !group) {
    group <- NULL
  }
  if (!is.null(group)) {
    if (is.numeric(group)) {
      group <- names(x$linelist)[group]
    }

    if (!group %in% names(x$linelist)) {
      msg <- sprintf("Group '%s' is not in the linelist", group)
      stop(msg)
    }
  }


  ## check type (node attribute used for color)
  if (length(type) > 1L) {
    stop("'type' must indicate a single node attribute")
  }
  if (is.logical(type) && !type) {
    type <- NULL
  }
  if (!is.null(type)) {
    if (is.numeric(type)) {
      type <- names(x$linelist)[type]
    }

    if (!type %in% names(x$linelist)) {
      msg <- sprintf("Type '%s' is not in the linelist", type)
      stop(msg)
    }
  }


  ## check annot (txt displayed when clicking on node)
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


  ## make a list of all nodes, and generate a data.frame of node attributes

  nodes <- data.frame(id = unique(c(x$linelist$id,
                                    x$contacts$from,
                                    x$contacts$to)))
  nodes <- suppressMessages(
    suppressWarnings(dplyr::left_join(nodes, x$linelist)))


  ## generate annotations ('title' in visNetwork terms)

  if (!is.null(annot)) {
    temp <- nodes[, annot, drop = FALSE]
    temp <- vapply(names(temp),
                   function(e) paste(e, temp[, e], sep = ": "),
                   character(nrow(nodes)))
    nodes$title <- paste("<p>",
                         apply(temp, 1, paste0, collapse = "<br>"), "</p>")
  }


  ## add node color ('group') and label

  nodes$label <- nodes$id

  if (!is.null(group)) {
    nodes$group <- as.character(nodes[, group])
    nodes$group[is.na(nodes$group)] <- "NA"
    nodes$group <- factor(nodes$group)
  }

  ## add shape info
  if (!is.null(type)) {
    if (is.null(type_code)) {
      msg <- paste("'type_code' needed if 'type' provided;",
                   "to see codes, type: codeawesome")
      stop(msg)
    }
    node_type <- as.character(unlist(x$linelist[type]))
    type_code["NA"] <- "fa-question-circle"
    node_code <- codeawesome[type_code[node_type]]
    nodes$shape <- "icon"
    nodes$icon.code <- node_code
  } else {
    nodes$borderWidth <- 2
  }

  ## add edge info

  edges <- x$contacts
  edges$width <- edge_width
  if (x$directed) {
    edges$arrows <- "to"
  }


  ## build visNetwork output

  out <- visNetwork::visNetwork(nodes, edges,
                                width = width, height = height, ...)

  ## specify group colors, add legend

  if (!is.null(group)) {
    K <- length(unique(nodes$group))
    grp_col <- col_pal(K)
    grp_col[levels(nodes$group) == "NA"] <- NA_col

    for (i in seq_len(K)) {
      out <- out %>% visNetwork::visGroups(groupname = levels(nodes$group)[i],
                                           color = grp_col[i])
    }

    if (legend && (K < legend_max)) {
      out <- out %>% visNetwork::visLegend()
    }
  }




  ## set nodes borders, edge width, and plotting options

  enabled <- list(enabled = TRUE)
  arg_selec <- if (selector) group else NULL

  out <- out %>%
    visNetwork::visOptions(highlightNearest = TRUE) %>%
    visNetwork::visOptions(selectedBy = arg_selec,
                           manipulation = editor,
                           highlightNearest = enabled)

  return(out)
}
