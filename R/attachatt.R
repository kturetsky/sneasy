#' Attach attributes to network
#'
#' Attach one or more node attributes to a network object.
#'
#' @param g igraph graph with named nodes (includes an attribute for 'name')
#' @param attdf Data frame containing desired attributes
#' @param index Character string; name of column in 'attdf' that contains the node names, to be used for matching to graph names
#' @param attvars Optional vector of variable name(s) in 'attdf' for attributes to be attached. If argument is missing, all variables in 'attdf' aside from the index column will be attached (with a warning message issued). To suppress warning, use attvars=NULL.
#' @return The original network graph with attributes added
#' @export
attachatt <- function(g, attdf, index, attvars){

  if(missing(attvars)){
    warning("List of attribute variables not provided -- attaching all variables in attribute data aside from index column.")
    attvars <- setdiff(colnames(attdf), index)
  }

  for(i in 1:length(attvars)){
    if(!(attvars[i] %in% names(attdf))){
      stop(paste0("Variable '", attvars[which(!(attvars %in% names(attdf)))], "' not found in attribute dataframe. "))
    }
  }

  if(is.null(attvars)){
    attvars <- setdiff(colnames(attdf), index)
  }

  for (i in attvars){
    g <- igraph::set.vertex.attribute(g, i, value=attdf[match(igraph::V(g)$name, dplyr::pull(attdf[index])),i])
  }

  g

}
