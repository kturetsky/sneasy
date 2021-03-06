% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/centdf.R
\name{centdf}
\alias{centdf}
\title{Get desired centralities in a dataframe}
\usage{
centdf(g, pps, types = "all", norm = "none", inout = TRUE, include_atts = TRUE)
}
\arguments{
\item{g}{igraph graph}

\item{pps}{An optional vector of participant ids to include in the dataframe so that non-participants' centrality scores are not included. If argument is missing, a warning message will be issued that non-participants may be included. To suppress warning, use pps=NULL.}

\item{types}{Character string; which centralities to calculate (default "all"; other valid values are: "alldisconnect" (all suitable for disconnected graphs, i.e., not closeness or eigenvector centrality), "degree"/"deg", "betweenness"/"btwn", "closeness"/"close", "harmonic"/"hclose", "strength"/"str", "avgstrength"/"avgstr", "eigenvector"/"eigen").}

\item{norm}{Character string; which centralities to normalize if applicable (default "none"; otherwise, takes the same values as 'types'). Normalization is available for degree, betweenness, closeness, and harmonic closeness.}

\item{inout}{Logical; if TRUE (default), will return in, out, and total centrality of each type if applicable. If FALSE, will only return total centrality. Only total centrality will be calculated if the graph object g is undirected. Additionally, betweeness centrality does not have an in- or out-type so the function will always calculate one value for betweenness.}

\item{include_atts}{Logical; if TRUE (default), will include node attributes in returned dataframe}
}
\value{
A dataframe of centralities (and, optional, node attributes) for each individual
}
\description{
Calculate common node-level centrality measures on a network.
}
\details{
Centrality measures will be calculated according to the type of network provided;
if the graph is directed and weighted, centrality measures will be calculated using directed
ties and weights, as applicable (except for eigenvector centrality, which is not well-defined
for directed graphs and will thus convert the network to an undirected graph prior to calculation).
If undirected and/or unweighted centrality calculations are desired, send the function an undirected
and/or unweighted graph.

Closeness and eigenvector centralities are not well-defined for disconnected graphs. Asking for
these centrality measures to be calculated on a graph with disconnected components will produce a warning.
Harmonic centrality is recommended in place of closeness centrality for disconnected graphs.

If a particular node has no ties, their average strength (total strength of ties divided by number of
ties) will be returned as 0 to indicate no strength (not NaN as dividing by 0 ties would produce).
}
\seealso{
\code{\link[igraph:degree]{igraph::degree()}}, \code{\link[igraph:betweenness]{igraph::betweenness()}}, \link[igraph:closeness]{igraph::closeness}, \code{\link[CINNA:harmonic_centrality]{CINNA::harmonic_centrality()}}, \code{\link[igraph:strength]{igraph::strength()}}, \code{\link[igraph:eigen_centrality]{igraph::eigen_centrality()}}
}
