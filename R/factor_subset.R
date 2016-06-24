#' Subset an epi_contact object by factors
#'
#' @author Finlay Campbell
#'
#' @export
#'
#' @param x an epi_contact object to be subsetted
#' @param edge.attribute a named list defining the factor name and the factor value
#' @param node.attribute a named list defining the factor name and the factor value

factor_subset <- function(x,edge.attribute=NULL,node.attribute=NULL){

    if (!inherits(x, "epi_contacts")) {
        stop("x is not an 'epi_contacts' object")
    }

    if(!all(names(edge.attribute) %in% names(x$contacts)) & !is.null(edge.attribute)) stop("Factor is not an edge attribute")

    if(!all(names(node.attribute) %in% names(x$linelist)) & !is.null(node.attribute)) stop("Factor is not a node attribute")

    if(is.null(edge.attribute) & is.null(node.attribute)){
        warning("No factor provided, returning unmodified epi.contact object")
        return(x)
    }

    if(!(is.null(node.attribute))){

        for(i in names(node.attribute)){

            if(class(node.attribute[[i]]) %in% c("character","factor")){
                x$linelist <- filter(x$linelist,x$linelist[[i]] %in% node.attribute[i])
            }

            if(class(node.attribute[[i]]) %in% c("numeric","Date")){
                x$linelist <- filter(x$linelist,x$linelist[[i]] < node.attribute[i])
            }
        }

        node.id <- x$linelist$id
    }


    if(!(is.null(edge.attribute))){

        for(i in names(edge.attribute)){

            if(class(edge.attribute[[i]]) %in% c("character","factor")){
                x$contacts <- filter(x$contacts,x$contacts[[i]] %in% edge.attribute[i])
            }

            if(class(edge.attribute[[i]]) %in% c("numeric","date")){
                x$contacts <- filter(x$contacts,x$contacts[[i]] < edge.attribute[i])
            }
        }

        edge.id <- x$contacts[,1:2]
    }

    out <- x[node.id,edge.id$from,contacts="from"]
    out <- out[node.id,edge.id$to,contacts="to"]

    return(out)
}