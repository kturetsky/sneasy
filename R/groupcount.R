#' Count of groups represented among ties
#'
#' Calculate number of groups represented among each node's ties (a measure of diversity)
#'
#' @param g igraph graph
#' @param dim Character string; dimension on which to calculate diversity--the name of a graph attribute in quotes (e.g., "race")
#' @param values Optional vector of character strings; all possible values of dim (e.g., c("Black", "White", "Latinx", "Asian")). If not provided, will assume that all possible values are represented in graph attributes and will issue message listing those values. Only used if adj = TRUE. To suppress message, use values = NULL.
#' @param mode Character string; count number of groups among all ties of a node ("all"; default), or just outgoing ties ("out") or incoming ties ("in").
#' @param adj Logical; if FALSE (default), a simple count will be calculated; if TRUE, count will be divided by the total number of groups in the network as a whole, such that returned values will be a proportion of total groups in the network represented in each node's ties
#' @param include_ego Logical; if FALSE (default), group count will be calculated only on network alters (outgoing or incoming ties, or both); if TRUE, group count will include ego's group.
#' @param include_att Logical; if TRUE (default), will include a column containing node values of specified dimension (e.g., participants' races)
#' @return A dataframe of number of groups represented in ties for a particular attribute, along with optional node values for that attribute. Group count will be NaN for individuals with no ties.
#' @importFrom rlang :=
#' @importFrom rlang sym
#' @export
groupcount <- function(g, dim, values, mode = "all", adj = FALSE, include_ego = FALSE, include_att = TRUE){

  if(!dim %in% igraph::list.vertex.attributes(g)) stop("'dim' must match name of an attribute attached to graph")

  if(!igraph::is.named(g)) stop("Vertices in graph object must be named (i.e., include an attached 'name' attribute -- V(g)$name)")

  if(!(mode %in% c("out", "in", "all", "total"))) stop("'mode' must be one of the following: 'all', 'total', 'out', 'in'")

  if(any(is.na(igraph::get.vertex.attribute(g, dim)))){
    message(paste0("Some nodes are missing ", dim, " data. Dropping ", sum(is.na(igraph::get.vertex.attribute(g, dim))), " node(s) for group count calculation."))
    g <- igraph::delete.vertices(g, V(g)[is.na(igraph::get.vertex.attribute(g, dim))])
  }

  if(missing(values) & adj==TRUE) message(paste0("Calculating adjusted group count assuming the following are all possible values of ", dim, ": ", paste(unique(igraph::get.vertex.attribute(g, dim)), collapse=", ")))

  if(missing(values)) values <- as.character(unique(igraph::get.vertex.attribute(g, dim)))

  if(is.null(values)) values <- as.character(unique(igraph::get.vertex.attribute(g, dim)))

  if(!is.character(values)){
    message("Coercing 'values' vector to character string")
    values <- as.character(values)
  }

  if(any(!(values %in% as.character(unique(igraph::get.vertex.attribute(g, dim)))))) warning(paste0("One or more provided values of '", dim, "' do not appear in network but are included in calculation: ", paste(values[which(!(values %in% as.character(unique(igraph::get.vertex.attribute(g, dim)))))], collapse=", ")))

  df <- data.frame(PPID = igraph::get.vertex.attribute(g, "name")) %>%
    dplyr::mutate(PPID = as.character(PPID),
                  !!dim := igraph::get.vertex.attribute(g, dim),
                  !!paste0("ngroup_",dim) := NA_integer_,
                  !!paste0("ngroup_",dim,"_wego") := NA_integer_)

  for(i in 1:nrow(df)){
    df[i, paste0("ngroup_",dim)] <- length(unique(igraph::get.vertex.attribute(g, dim, unique(igraph::neighbors(g, df[i, "PPID"], mode = mode)))))
    df[i, paste0("ngroup_",dim,"_wego")] <- length(unique(c(igraph::get.vertex.attribute(g, dim, df[i, "PPID"]), igraph::get.vertex.attribute(g, dim, unique(igraph::neighbors(g, df[i, "PPID"], mode = mode))))))
    if(length(igraph::neighbors(g, df[i, "PPID"], mode = mode))==0){
      df[i, paste0("ngroup_",dim)] <- NaN
      df[i, paste0("ngroup_",dim,"_wego")] <- NaN
    }
  }

  if(include_ego==FALSE){
    df <- df %>%
      dplyr::select(-!!sym(paste0("ngroup_",dim,"_wego")))
    if(adj==TRUE){
      df <- df %>%
        dplyr::mutate(!!paste0("ngroup_",dim,"_adj") := !!sym(paste0("ngroup_",dim))/length(values)) %>%
        dplyr::select(-!!sym(paste0("ngroup_",dim)))
    }
  } else if(include_ego==TRUE){
    df <- df %>%
      dplyr::select(-!!sym(paste0("ngroup_",dim)))
    if(adj==TRUE){
      df <- df %>%
        dplyr::mutate(!!paste0("ngroup_",dim,"_wego_adj") := !!sym(paste0("ngroup_",dim,"_wego"))/length(values)) %>%
        dplyr::select(-!!sym(paste0("ngroup_",dim,"_wego")))
    }
  }

  if(include_att==FALSE) df %>% dplyr::select(-!!sym(dim)) %>% as.data.frame()
  else df %>% as.data.frame()

}
