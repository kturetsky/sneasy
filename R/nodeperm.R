#' Permute nodes
#'
#' Permute node labels of a network
#'
#' @param mat Network matrix
#' @param mode Character string; How igraph should interpret matrix; default is 'directed' -- other common choice is 'undirected'
#' @param weighted Logical; if TRUE (default), network is weighted; if FALSE, network is not weighted
#' @param diag Logical; if FALSE (default), self-nominations are removed; if TRUE, network will include self-nominations
#' @return An igraph object with the same underlying structure as original network, but randomized node labels
#' @export
nodeperm <- function(mat, mode = "directed", weighted = TRUE, diag = FALSE){

  randnodes <- sample(rownames(mat), replace = FALSE)
  rownames(mat) <- randnodes
  colnames(mat) <- randnodes

  igraph::graph.adjacency(mat, mode = mode, weighted = weighted, diag = diag)
}
