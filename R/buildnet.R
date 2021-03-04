#' Get clean network graph, matrix, or edgelist
#'
#' Transform weighted social network data collected in wide format to a network graph (igraph object), matrix, or edgelist.
#'
#' Requires set naming system: participant id column named "PPID",
#' nominated tie columns named network type + tie number + "Name" or "ID" (e.g., "Friend1Name" or "Friend1ID"),
#' tie strength columns named network type + tie number + tie strength type (e.g., "Friend1Strength").
#' Isolates are retained by including a self-nomination (loop) with tie weight 0.
#'
#' @param snawide Wide social network dataframe to be transformed
#' @param nettype Character string; name of network, matching column names, in quotes (e.g., "Friend")
#' @param weighttype Character string; tie strength type in variable names, in quotes (default is "Strength")
#' @param missingweight Numeric; if participant nominated a tie but did not provide a tie strength rating, what strength should be assumed? (default is 1)
#' @param return Character string; if "graph" or "network" (default), function will return an igraph graph object; if "matrix," function will return a network matrix; if "edgelist" or "el," function will return an edgelist
#' @return A weighted igraph graph, network matrix, or edgelist
#' @importFrom tidyselect contains
#' @importFrom stringr str_detect
#' @importFrom reshape2 acast
#' @export
buildnet <- function(snawide, nettype, weighttype = "Strength", missingweight = 1, return = "graph"){

  if(!(return %in% c("graph", "network", "matrix", "edgelist", "el"))) stop("Valid values of 'return' are: 'graph', 'network', 'matrix', 'edgelist', 'el'")

  xlong <- snawide %>%
    dplyr::select(c(PPID, tidyselect::contains(nettype))) %>%
    tidyr::pivot_longer(-PPID, names_to = "variable", values_to = "to") %>%
    dplyr::rename(from = PPID) %>%
    as.data.frame()

  el <- xlong %>%
    dplyr::filter(stringr::str_detect(variable, "Name|ID")) %>%
    dplyr::mutate(weight = dplyr::filter(xlong, stringr::str_detect(variable, weighttype))[,3]) %>%
    dplyr::select(-variable) %>%
    dplyr::mutate(weight = ifelse(!is.na(to) & is.na(weight), missingweight, weight),
                  to = ifelse(is.na(to), from, to)) %>%
    dplyr::mutate(weight = ifelse(to == from, 0, weight)) %>%
    dplyr::distinct()

  lvls <- unique(c(as.character(el$from), as.character(el$to)))

  el <- el %>%
    dplyr::mutate(from = factor(from, levels = lvls),
                  to = factor(to, levels = lvls))

  mat <- reshape2::acast(el, from~to, value.var="weight", fill=0, drop=F)

  if(return %in% c("edgelist", "el")) el
  else if(return=="matrix") mat
  else if (return %in% c("graph", "network")) igraph::graph.adjacency(mat, mode="directed", weighted=T, diag=F)
}
