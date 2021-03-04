#' Get desired centralities in a dataframe
#'
#' Calculate common node-level centrality measures on a network.
#'
#' Centrality measures will be calculated according to the type of network provided;
#' if the graph is directed and weighted, centrality measures will be calculated using directed
#' ties and weights, as applicable (except for eigenvector centrality, which is not well-defined
#' for directed graphs and will thus convert the network to an undirected graph prior to calculation).
#' If undirected and/or unweighted centrality calculations are desired, send the function an undirected
#' and/or unweighted graph.
#'
#' Closeness and eigenvector centralities are not well-defined for disconnected graphs. Asking for
#' these centrality measures to be calculated on a graph with disconnected components will produce a warning.
#' Harmonic centrality is recommended in place of closeness centrality for disconnected graphs.
#'
#' If a particular node has no ties, their average strength (total strength of ties divided by number of
#' ties) will be returned as 0 to indicate no strength (not NaN as dividing by 0 ties would produce).
#'
#' @param g igraph graph
#' @param pps An optional vector of participant ids to include in the dataframe so that non-participants' centrality scores are not included. If argument is missing, a warning message will be issued that non-participants may be included. To suppress warning, use pps=NULL.
#' @param types Character string; which centralities to calculate (default "all"; other valid values are: "alldisconnect" (all suitable for disconnected graphs, i.e., not closeness or eigenvector centrality), "degree"/"deg", "betweenness"/"btwn", "closeness"/"close", "harmonic"/"hclose", "strength"/"str", "avgstrength"/"avgstr", "eigenvector"/"eigen").
#' @param norm Character string; which centralities to normalize if applicable (default "none"; otherwise, takes the same values as 'types'). Normalization is available for degree, betweenness, closeness, and harmonic closeness.
#' @param inout Logical; if TRUE (default), will return in, out, and total centrality of each type if applicable. If FALSE, will only return total centrality. Only total centrality will be calculated if the graph object g is undirected. Additionally, betweeness centrality does not have an in- or out-type so the function will always calculate one value for betweenness.
#' @param include_atts Logical; if TRUE (default), will include node attributes in returned dataframe
#' @return A dataframe of centralities (and, optional, node attributes) for each individual
#' @seealso [igraph::degree()], [igraph::betweenness()], [igraph::closeness], [CINNA::harmonic_centrality()], [igraph::strength()], [igraph::eigen_centrality()]
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @export
centdf <- function(g, pps, types = "all", norm = "none", inout = TRUE, include_atts = TRUE){

  if (!all(types %in% c("all", "alldisconnect", "degree", "deg", "betweenness", "btwn",
                        "closeness", "close", "harmonic", "hclose",
                        "strength", "str", "avgstrength", "avgstr", "eigenvector", "eigen"))) stop("Valid values of 'types' are:
                        'all', 'alldisconnect', 'degree', 'deg', 'betweenness', 'btwn', 'closeness', 'close', 'harmonic',
                        'hclose', 'strength', 'str', 'avgstrength', 'avgstr', 'eigenvector', 'eigen'")

  if (!igraph::is.directed(g) & inout==TRUE){
    inout <- FALSE
    warning("Cannot calculate in- and out-centrality on an undirected network;
      calculating single centrality value only")
  }

  if(!igraph::is_connected(g) & any(types %in% c("all"))) warning("Some centrality measures are not well-defined for disconnected graphs. Consider using
  types='alldisconnect' to calculate only centrality measures suitable for disconnected graphs.")

  if(!igraph::is_weighted(g) & any(types %in% c("all", "alldisconnect", "strength", "str", "avgstrength", "avgstr"))) warning("Strength cannot be calculated on an unweighted graph. Strength calculations will be skipped.")

  df <- data.frame(PPID = igraph::get.vertex.attribute(g, "name")) %>%
    dplyr::mutate(PPID = as.character(PPID))

  if (include_atts==TRUE){

    att <- igraph::list.vertex.attributes(g) %>%
      setdiff("name")

    if (length(att)>0){

      for(i in 1:length(att)){
        df <- df %>%
          dplyr::mutate(!!att[i] := igraph::get.vertex.attribute(g, att[i]))
        }
    }

  }

  if (any(types %in% c("all", "alldisconnect", "degree", "deg"))){
    if (any(norm %in% c("all", "alldisconnect", "degree", "deg"))) degnorm = TRUE else degnorm = FALSE
    if (inout==TRUE){
        df <- df %>%
          dplyr::mutate(degree = igraph::degree(g, mode="total", normalized = degnorm),
                        degin = igraph::degree(g, mode="in", normalized = degnorm),
                        degout = igraph::degree(g, mode="out", normalized = degnorm))
      }
      else df <- df %>% dplyr::mutate(degree = igraph::degree(g, mode="total", normalized = degnorm))
  }

  if (any(types %in% c("all", "alldisconnect", "betweenness", "btwn"))){
    if (any(norm %in% c("all", "alldisconnect", "betweenness", "btwn"))) btwnnorm = TRUE else btwnnorm = FALSE
    df <- df %>%
      dplyr::mutate(btwn = igraph::betweenness(g, normalized = btwnnorm))
  }

  if (any(types %in% c("all", "closeness", "close"))){
    if (any(norm %in% c("all", "closeness", "close"))) clonorm = TRUE else clonorm = FALSE
    if (inout==TRUE){
      df <- df %>%
        dplyr::mutate(close = igraph::closeness(g, mode="total", normalized = clonorm),
                      closein = igraph::closeness(g, mode="in", normalized = clonorm),
                      closeout = igraph::closeness(g, mode="out", normalized = clonorm))
    }
    else df <- df %>% dplyr::mutate(close = igraph::closeness(g, mode="total", normalized = clonorm))
  }

  if (any(types %in% c("all", "alldisconnect", "harmonic", "hclose"))){
    if (any(norm %in% c("all", "alldisconnect", "harmonic", "hclose"))) harmnorm = (igraph::vcount(g) - 1) else harmnorm = 1
    if (inout==TRUE){
      df <- df %>%
        dplyr::mutate(hclose = CINNA::harmonic_centrality(g, mode="all")/harmnorm,
                      hclosein = CINNA::harmonic_centrality(g, mode="in")/harmnorm,
                      hcloseout = CINNA::harmonic_centrality(g, mode="out")/harmnorm)
    }
    else df <- df %>% dplyr::mutate(hclose = CINNA::harmonic_centrality(g, mode="all")/harmnorm)
  }

  if (any(types %in% c("all", "alldisconnect", "strength", "str")) & igraph::is.weighted(g)){
    if (inout==TRUE){
      df <- df %>%
        dplyr::mutate(strength = igraph::strength(g, mode="total", loops = FALSE),
                      strin = igraph::strength(g, mode="in", loops = FALSE),
                      strout = igraph::strength(g, mode="out", loops = FALSE))
    }
    else df <- df %>% dplyr::mutate(strength = igraph::strength(g, mode="total", loops = FALSE))
  }

  if (any(types %in% c("all", "alldisconnect", "avgstrength", "avgstr")) & igraph::is.weighted(g)){
    if (inout==TRUE){
      df <- df %>%
        dplyr::mutate(avgstrength = dplyr::if_else(igraph::degree(g, mode="total", normalized = FALSE)==0, 0, igraph::strength(g, mode="total", loops = FALSE)/igraph::degree(g, mode="total", normalized = FALSE)),
                      avgstrin = dplyr::if_else(igraph::degree(g, mode="in", normalized = FALSE)==0, 0, igraph::strength(g, mode="in", loops = FALSE)/igraph::degree(g, mode="in", normalized = FALSE)),
                      avgstrout = dplyr::if_else(igraph::degree(g, mode="out", normalized = FALSE)==0, 0, igraph::strength(g, mode="out", loops = FALSE)/igraph::degree(g, mode="out", normalized = FALSE)))
    }
    else df <- df %>% dplyr::mutate(avgstrength = dplyr::if_else(igraph::degree(g, mode="total", normalized = FALSE)==0, 0, igraph::strength(g, mode="total", loops = FALSE)/igraph::degree(g, mode="total", normalized = FALSE)))
  }

  if (any(types %in% c("all", "eigenvector", "eigen"))){
    if (igraph::is_connected(g)==FALSE) warning("eigenvector centrality is not well-defined for disconnected graphs")
    df <- df %>%
      dplyr::mutate(eigenvector = igraph::eigen_centrality(g, directed = FALSE)$vector,
                    eigenvalue = igraph::eigen_centrality(g, directed = FALSE)$value)
  }

  if (any(types %in% c("all", "eigenvector", "eigen"))){
    if (igraph::is_connected(g)==FALSE) warning("eigenvector centrality is not well-defined for disconnected graphs")
    df <- df %>%
      dplyr::mutate(eigenvector = igraph::eigen_centrality(g, directed = FALSE)$vector,
                    eigenvalue = igraph::eigen_centrality(g, directed = FALSE)$value)
  }

  if(missing(pps)){
    warning("List of participant IDs not provided; dataframe may include non-participants who did not complete network measures")
    df
  }
  else if(is.null(pps)) df
  else df %>% dplyr::filter(PPID %in% pps)

}


