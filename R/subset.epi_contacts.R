#' Subset an epi_contact object by factors
#'
#' This function subsets an \code{\link{epi_contacts}} object based on node,
#' edge and/or cluster attributes. Specifying node attributes will return an
#' \code{\link{epi_contacts}} object containing only individuals with these
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
#' @param node.attribute a named list defining the node attribute name and node
#'     attribute value (as a single value or vector of values).  Dates must be
#'     provided as a vector of date objects, defining the range of dates
#'     included in the subset. If only one date is provided, only node
#'     attributes with that date will be returned.
#'
#' @param edge.attribute a named list defining the edge attribute name and edge
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
#' x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
#' id="case.id", to="case.id", from="infector", directed=FALSE)
#'
#' ## subset based on node and edge attributes
#' x_subset <- subset(x, node.attribute=list("gender"="f"),
#'                    edge.attribute=list("source"="funeral"))
#'
#' ## subset a cluster connected to a given id
#' ## (can be a vector of ids as well)
#' id <- "cac51e"
#' x_subset <- subset(x, cluster_id=id)
#'
#' ## subset based on cluster size range
#' x_subset <- subset(x, cs_min = 12, cs_max = 15)
#'
#' ## subset based on single cluster size
#' x_subset <- subset(x, cs = 12)
#'
#' ## subset based on minimum cluster size
#' x_subset <- subset(x, cs_min = 10)
#'
#' ## subset based on maximum cluster size
#' x_subset <- subset(x, cs_max = 9)
#' }

subset.epi_contacts <- function(x,node.attribute = NULL, edge.attribute = NULL,
                                cluster_id = NULL, cs = NULL, cs_min = NULL,
                                cs_max = NULL, ...){
    ## A function to check node.attribute and edge.attribute for errors;
    ## arguments are:

    ## - name.attribute is the name of the attribute being checked
    ## (e.g. "date.of.infection")

    ## - list.attributes is the named list of attributes (either node.attribute
    ## or edge.attribute)

    ## - dataset is the dataframe describing the attribute values in the data
    ## (x$linelist or x$contacts)

    check <- function(name.attribute, list.attributes, dataset) {

        if (!name.attribute %in% names(dataset))
            stop(paste(name.attribute,"is not an attribute found in dataset"))

        ## Attribute is a single node or edge attribute value (e.g. "f", if
        ## name.attribute is "gender")
        attribute <- list.attributes[[name.attribute]]

        ## Data is the set of node or attribute values found in the data (e.g. a
        ## vector of infection dates)
        data <- dataset[[name.attribute]]

        if (inherits(data,"Date")) {
            if (!inherits(attribute,"Date"))
                stop(paste(name.attribute,"must be provided as a date object"))
            if (length(attribute)==1)
                attribute <- c(attribute,attribute)
            if (length(attribute)>2) {
                attribute <- attribute[1:2]
                warning(paste0("More than two date values provided for ",
                               name.attribute,", using first two"))
            }
            if (attribute[1] > attribute[2]) {
                attribute <- c(attribute[2], attribute[1])
                warning(paste("First date provided for",
                              name.attribute,"is greater than second,
                               using second date as lower boundary"))
            }
            if (min(attribute) > max(data, na.rm=TRUE) ||
                max(attribute) < min(data, na.rm=TRUE))
                stop(paste("Dates provided for",
                           name.attribute,
                           "fall outside the range of the dataset"))

        } else if (!attribute %in% data)
            stop(paste("Value for", name.attribute, "is not found in dataset"))

        return(attribute)

    }

    ## A function to subset a dataset (x$linelist or x$contacts) by a node or
    ## edge attribute
    to.keep <- function(name.attribute,list.attributes,dataset) {

        attribute <- list.attributes[[name.attribute]]
        data <- dataset[[name.attribute]]

        if (inherits(attribute,"Date")) {
            to.keep <- data >= attribute[1] & data <= attribute[2]
        } else {
            to.keep <- data %in% attribute
        }

        return(to.keep)
    }

    ## Check if epi_contacts object, node.attribute and edge.attribute are
    ## provided correctly
    if (!inherits(x, "epi_contacts")) stop("x is not an 'epi_contacts' object")
    sub.attr <- c(edge.attribute, node.attribute,
                  cluster_id, cs, cs_min, cs_max)
    if (is.null(sub.attr)) {
        warning("No subsetting attributes provided, returning input object")
        return(x)
    }
    if (!is.null(node.attribute) &&
        !inherits(node.attribute,"list")) {
        stop("node.attribute is not a list")
    }
    if (!is.null(edge.attribute) &&
        !inherits(edge.attribute,"list")) {
        stop("edge.attribute is not a list")
    }

    names.na <- names(node.attribute)
    names.ea <- names(edge.attribute)

    ## Apply checks to node.attribute and edge.attribute
    node.attribute <- lapply(names.na, check, node.attribute, x$linelist)
    names(node.attribute) <- names.na

    edge.attribute <- lapply(names.ea, check, edge.attribute, x$contacts)
    names(edge.attribute) <- names.ea

    ## Apply the subs function across all attributes provided in node.attribute
    if (!(is.null(node.attribute))) {
        for (name.attribute in names.na) {
            to.keep <- to.keep(name.attribute,node.attribute,x$linelist)
            x <- x[i=to.keep]
        }
    }

    ## Apply the subs function across all attributes provided in edge.attribute
    if (!(is.null(edge.attribute))){
        for (name.attribute in names.ea) {
            to.keep <- to.keep(name.attribute,edge.attribute,x$contacts)
            x <- x[j=to.keep]
        }
    }

    ## Apply subset_clusters_by_id
    if (!is.null(cluster_id)) {
        x <- subset_clusters_by_id(x,cluster_id)
    }

    ## Apply subset_clusters_by_size
    if (!is.null(c(cs,cs_min,cs_max))) {
        x <- subset_clusters_by_size(x,cs,cs_min,cs_max)
    }

    return(x)
}
