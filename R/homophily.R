#' Homophily
#'
#' Calculate average homophily of ties for each node
#'
#' @param g igraph graph
#' @param dim Character string; dimension on which to calculate homophily--the name of a graph attribute in quotes (e.g., "race")
#' @param mode Character string; calculate homophily of all ties of a node ("all"; default), or just outgoing ties ("out") or incoming ties ("in").
#' @param adj Logical; if FALSE (default), homophily score will be unadjusted and will range from 0 to 1; if TRUE, will adjust raw homophily score to adjust for underlying proportion of same-group individuals in the network, such that homophily score will range from -1 to 1 (see Details).
#' @param include_att Logical; if TRUE (default), will include a column containing node values of specified dimension (e.g., participants' races)
#' @details The raw homophily score (Hraw) is a proportion ranging from 0 (no same-group ties) to 1 (all same-group ties). This score can be adjusted for the proportion of same-group individuals in the network (Hnet) and thus the baseline probability of developing same-group ties through the formula (Hraw - Hnet)/Hnet. After this adjustment, homophily is rescaled from -1 to 1, where -1 indicates fewer same-group ties than expected based on network composition, 0 indicates the same amount of same-group ties as expected based on network composition, and 1 indicates more same-group ties than expected based on network composition. This adjustment is useful when the network contains unequally sized groups.
#' @references Coleman, J. (1958). Relational analysis: The study of social organizations with survey methods. Human organization, 17(4), 28-36.
#' @return A dataframe of homophily scores for a particular attribute, along with optional node values for that attribute. Homophily will be NaN for individuals with no ties.
#' @export
homophily <- function(g, dim, mode = "all", adj = FALSE, include_att = TRUE){

  if(!dim %in% igraph::list.vertex.attributes(g)) stop("'dim' must match name of an attribute attached to graph")

  if(!igraph::is.named(g)) stop("Vertices in graph object must be named (i.e., include an attached 'name' attribute -- V(g)$name)")

  if(!(mode %in% c("out", "in", "all", "total"))) stop("'mode' must be one of the following: 'all', 'total', 'out', 'in'")

  if(any(is.na(igraph::get.vertex.attribute(g, dim)))){
    message(paste0("Some nodes are missing ", dim, " data. Dropping ", sum(is.na(igraph::get.vertex.attribute(g, dim))), " node(s) for homophily calculation."))
    g <- igraph::delete.vertices(g, V(g)[is.na(igraph::get.vertex.attribute(g, dim))])
  }

  df <- data.frame(PPID = igraph::get.vertex.attribute(g, "name")) %>%
    dplyr::mutate(PPID = as.character(PPID),
                  !!dim := igraph::get.vertex.attribute(g, dim),
                  !!paste0("homoph_",dim) := NA_integer_,
                  homophnet = NA_real_)

  for(i in 1:nrow(df)){
    df[i, paste0("homoph_",dim)] <- sum(igraph::get.vertex.attribute(g, dim, unique(igraph::neighbors(g, df[i, "PPID"], mode = mode)))==igraph::get.vertex.attribute(g, dim, df[i, "PPID"]))/length(unique(igraph::neighbors(g, df[i, "PPID"], mode = mode)))
    df[i, "homophnet"] <- sum(df[dim]==igraph::get.vertex.attribute(g, dim, df[i, "PPID"]))/nrow(df)
  }

  if(include_att==FALSE) df <- df %>% dplyr::select(-!!sym(dim))

  if(adj==FALSE) df %>% dplyr::select(-homophnet)
  else{
    df %>%
      dplyr::mutate(!!paste0("homoph_",dim,"_adj") := (!!sym(paste0("homoph_",dim))-homophnet)/homophnet) %>%
      dplyr::select(-!!sym(paste0("homoph_",dim)), -homophnet)
  }

}
