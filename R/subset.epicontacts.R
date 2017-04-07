#' Subset an epicontact object by factors
#'
#' This function subsets an \code{\link{epicontacts}} object based on node,
#' edge and/or cluster attributes. Specifying node attributes will return an
#' \code{\link{epicontacts}} object containing only individuals with these
#' given attributes in the linelist. Specifying edge attributes will return
#' contacts with the attributes provided. Specifying cluster attributes will
#' return clusters of connected cases, and can be defined by ids (returning
#' clusters of cases connected to specified cases) or cluster sizes (returning
#' cluster of a specific, minimum or maximum size).
#'
#' @author Finlay Campbell (\email{f.campbell15@@imperial.ac.uk}), Nistara
#'     Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @export
#'
#' @param x an epi_contact object to be subsetted
#'
#' @param node_attribute a named list defining the node attribute name and node
#'     attribute value (as a single value or vector of values).  Dates must be
#'     provided as a vector of date objects, defining the range of dates
#'     included in the subset. If only one date is provided, only node
#'     attributes with that date will be returned.
#'
#' @param edge_attribute a named list defining the edge attribute name and edge
#'     attribute value (as a single value or vector of values).  Dates must be
#'     provided as a vector of date objects, defining the range of dates
#'     included in the subset. If only one date is provided, only edge
#'     attributes with that date will be returned.
#'
#' @param cluster_id a character vector of case identifiers; the connected
#'     components attached to these cases will be retained in the output object.
#'
#' @param cs cluster size to be used for subsetting
#'
#' @param cs_min minimum cluster size for subsetting
#'
#' @param cs_max maximum cluster size for subsetting
#'
#' @param ... further arguments passed on to other methods
#'
#' @examples
#' if (require(outbreaks)) {
#' ## build data
#'
#' x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
#'                       id = "case.id", to = "case.id",
#'                       from = "infector", directed = FALSE)
#'
#' ## subset based on node and edge attributes
#' x_subset <- subset(x, node_attribute = list("gender" = "f"),
#'                    edge_attribute = list("source" = "funeral"))
#'
#' 
#' ## subset a cluster connected to a given id
#' ## (can be a vector of ids as well)
#' ## here picking node with highest out-degree
#' 
#' id <- names(which.max(get_degree(x, "out")))
#' x_subset <- thin(subset(x, cluster_id = id), 2)
#' x_subset
#' plot(x_subset)
#' 
#' 
#' ## subset based on cluster size range
#'
#' x_subset <- subset(x, cs_min = 12, cs_max = 15)
#'
#'
#' ## subset based on single cluster size
#'
#' x_subset <- subset(x, cs = 12)
#'
#'
#' ## subset based on minimum cluster size
#'
#' x_subset <- subset(x, cs_min = 10)
#'
#'
#' ## subset based on maximum cluster size
#'
#' x_subset <- subset(x, cs_max = 9)
#'
#' 
#' }

subset.epicontacts <- function(x, node_attribute = NULL, edge_attribute = NULL,
                                cluster_id = NULL, cs = NULL, cs_min = NULL,
                                cs_max = NULL, ...){
    ## A function to check node_attribute and edge_attribute for errors;
    ## arguments are:

    ## - name_attribute is the name of the attribute being checked
    ## (e.g. "date.of.infection")

    ## - list_attributes is the named list of attributes (either node_attribute
    ## or edge_attribute)

    ## - dataset is the dataframe describing the attribute values in the data
    ## (x$linelist or x$contacts)

    check <- function(name_attribute, list_attributes, dataset) {

        if (!name_attribute %in% names(dataset))
            stop(paste(name_attribute, "is not an attribute found in dataset"))

        ## Attribute is a single node or edge attribute value (e.g. "f", if
        ## name_attribute is "gender")
        attribute <- list_attributes[[name_attribute]]

        ## Data is the set of node or attribute values found in the data (e.g. a
        ## vector of infection dates)
        data <- dataset[[name_attribute]]

        if (inherits(data, "Date")) {
            if (!inherits(attribute, "Date"))
                stop(paste(name_attribute, "must be provided as a date object"))
            if (length(attribute) == 1)
                attribute <- c(attribute,attribute)
            if (length(attribute)>2) {
                attribute <- attribute[1:2]
                warning(paste0("More than two date values provided for ",
                               name_attribute, ", using first two"))
            }

            ## make sure dates are ordered, first = most ancient
            
            attribute <- sort(attribute)

            if (min(attribute) > max(data, na.rm=TRUE) ||
                max(attribute) < min(data, na.rm=TRUE))
                stop(paste("Dates provided for",
                           name_attribute,
                           "fall outside the range of the dataset"))

        } else if (!attribute %in% data)
            stop(paste("Value for", name_attribute, "is not found in dataset"))

        return(attribute)

    }
    

    ## A function to subset a dataset (x$linelist or x$contacts) by a node or
    ## edge attribute
    
    find_id_to_keep <- function(name_attribute,list_attributes,dataset) {

        attribute <- list_attributes[[name_attribute]]
        data <- dataset[[name_attribute]]

        if (inherits(attribute,"Date")) {
            out <- data >= attribute[1] & data <= attribute[2]
        } else {
            out <- data %in% attribute
        }

        ## Treat NA as FALSE
        out[is.na(out)] <- FALSE

        return(out)
    }

    
    ## Check if epicontacts object, node_attribute and edge_attribute are
    ## provided correctly
    
    if (!inherits(x, "epicontacts")) {
        stop("x is not an 'epicontacts' object")
    }
    sub_attr <- c(edge_attribute, node_attribute,
                  cluster_id, cs, cs_min, cs_max)
    if (is.null(sub_attr)) {
        warning("No subsetting attributes provided, returning input object")
        return(x)
    }
    if (!is.null(node_attribute) &&
        !inherits(node_attribute,"list")) {
        stop("node_attribute is not a list")
    }
    if (!is.null(edge_attribute) &&
        !inherits(edge_attribute,"list")) {
        stop("edge_attribute is not a list")
    }

    names_na <- names(node_attribute)
    names_ea <- names(edge_attribute)

    ## Apply checks to node_attribute and edge_attribute
    node_attribute <- lapply(names_na, check, node_attribute, x$linelist)
    names(node_attribute) <- names_na

    edge_attribute <- lapply(names_ea, check, edge_attribute, x$contacts)
    names(edge_attribute) <- names_ea

    ## Apply the subs function across all attributes provided in node_attribute
    if (!(is.null(node_attribute))) {
        for (name_attribute in names_na) {
            to.keep <- find_id_to_keep(name_attribute,
                                       node_attribute,
                                       x$linelist)
            x <- x[i = to.keep]
        }
    }

    ## Apply the subs function across all attributes provided in edge_attribute
    if (!(is.null(edge_attribute))){
        for (name_attribute in names_ea) {
            to.keep <- find_id_to_keep(name_attribute,
                                       edge_attribute,
                                       x$contacts)
            x <- x[j = to.keep]
        }
    }

    ## Apply subset_clusters_by_id
    if (!is.null(cluster_id)) {
        x <- subset_clusters_by_id(x, cluster_id)
    }

    ## Apply subset_clusters_by_size
    if (!is.null(c(cs, cs_min, cs_max))) {
        x <- subset_clusters_by_size(x, cs, cs_min, cs_max)
    }

    return(x)
}
