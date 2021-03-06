% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/homophily.R
\name{homophily}
\alias{homophily}
\title{Homophily}
\usage{
homophily(g, dim, mode = "all", adj = FALSE, include_att = TRUE)
}
\arguments{
\item{g}{igraph graph}

\item{dim}{Character string; dimension on which to calculate homophily--the name of a graph attribute in quotes (e.g., "race")}

\item{mode}{Character string; calculate homophily of all ties of a node ("all"; default), or just outgoing ties ("out") or incoming ties ("in").}

\item{adj}{Logical; if FALSE (default), homophily score will be unadjusted and will range from 0 to 1; if TRUE, will adjust raw homophily score to adjust for underlying proportion of same-group individuals in the network, such that homophily score will range from -1 to 1 (see Details).}

\item{include_att}{Logical; if TRUE (default), will include a column containing node values of specified dimension (e.g., participants' races)}
}
\value{
A dataframe of homophily scores for a particular attribute, along with optional node values for that attribute. Homophily will be NaN for individuals with no ties.
}
\description{
Calculate average homophily of ties for each node
}
\details{
The raw homophily score (Hraw) is a proportion ranging from 0 (no same-group ties) to 1 (all same-group ties). This score can be adjusted for the proportion of same-group individuals in the network (Hnet) and thus the baseline probability of developing same-group ties through the formula (Hraw - Hnet)/Hnet. After this adjustment, homophily is rescaled from -1 to 1, where -1 indicates fewer same-group ties than expected based on network composition, 0 indicates the same amount of same-group ties as expected based on network composition, and 1 indicates more same-group ties than expected based on network composition. This adjustment is useful when the network contains unequally sized groups.
}
\references{
Coleman, J. (1958). Relational analysis: The study of social organizations with survey methods. Human organization, 17(4), 28-36.
}
