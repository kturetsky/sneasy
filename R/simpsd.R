#' Simpson's D
#'
#' Calculate node-level diversity of ties through Simpson's Diversity Index.
#'
#' @param g igraph graph
#' @param dim Character string; dimension on which to calculate diversity--the name of a graph attribute in quotes (e.g., "race")
#' @param values Optional vector of character strings; all possible values of dim (e.g., c("Black", "White", "Latinx", "Asian")). If not provided, will assume that all possible values are represented in graph attributes and will issue message listing those values. Only used if std = TRUE. To suppress message, use values = NULL.
#' @param mode Character string; calculate diversity of all ties of a node ("all"; default), or just outgoing ties ("out") or incoming ties ("in").
#' @param std Logical; if FALSE (default), Simpson's D will not be standardized (the maximum D will decrease as the number of groups increases) and can be interpreted as the probability that two individuals picked at random come from the same group; if TRUE, will use standardized version of Simpson's D (see Details) such that 1 = maximum diversity for that attribute (i.e., ties are equally distributed across all possible groups).
#' @param include_att Logical; if TRUE (default), will include a column containing node values of specified dimension (e.g., participants' races)
#' @details Simpson's D is standardized by dividing by 1 - (1/k), where k is the number of groups in the network.
#' @references Simpson, E. H. (1949). Measurement of diversity. Nature, 163, 688; Mcdonald, D. G., & Dimmick, J. (2003). The Conceptualization and Measurement of Diversity. Communication Research, 30(1), 60–79. https://doi.org/10.1177/0093650202239026
#' @return A dataframe of Simpson's D for a particular attribute, along with optional node values for that attribute. Simpson's D will be NaN for individuals with no ties.
#' @importFrom dplyr group_by
#' @export
simpsd <- function(g, dim, values, mode = "all", std = FALSE, include_att = TRUE){

  if(!dim %in% igraph::list.vertex.attributes(g)) stop("'dim' must match name of an attribute attached to graph")

  if(!igraph::is.named(g)) stop("Vertices in graph object must be named (i.e., include an attached 'name' attribute -- V(g)$name)")

  if(!(mode %in% c("out", "in", "all", "total"))) stop("'mode' must be one of the following: 'all', 'total', 'out', 'in'")

  if(any(is.na(igraph::get.vertex.attribute(g, dim)))){
    message(paste0("Some nodes are missing ", dim, " data. Dropping ", sum(is.na(igraph::get.vertex.attribute(g, dim))), " node(s) for Simpson's D calculation."))
    g <- igraph::delete.vertices(g, V(g)[is.na(igraph::get.vertex.attribute(g, dim))])
  }

  if(missing(values) & std==TRUE) message(paste0("Calculating standardized Simpson's D assuming the following are all possible values of ", dim, ": ", paste(unique(igraph::get.vertex.attribute(g, dim)), collapse=", ")))

  if(missing(values)) values <- as.character(unique(igraph::get.vertex.attribute(g, dim)))

  if(is.null(values)) values <- as.character(unique(igraph::get.vertex.attribute(g, dim)))

  if(!is.character(values)){
    message("Coercing 'values' vector to character string")
    values <- as.character(values)
  }

  if(any(!(as.character(unique(igraph::get.vertex.attribute(g, dim))) %in% values))) stop(paste0("Data includes one or more values of '", dim, "' that were not provided in 'values': ", paste(as.character(unique(igraph::get.vertex.attribute(g, dim)))[which(!(as.character(unique(igraph::get.vertex.attribute(g, dim))) %in% values))], collapse=", ")))

  if(any(!(values %in% as.character(unique(igraph::get.vertex.attribute(g, dim)))))) warning(paste0("One or more provided values of '", dim, "' do not appear in network but are included in calculation: ", paste(values[which(!(values %in% as.character(unique(igraph::get.vertex.attribute(g, dim)))))], collapse=", ")))

  df <- data.frame(PPID = igraph::get.vertex.attribute(g, "name")) %>%
    dplyr::mutate(PPID = as.character(PPID),
                  !!dim := igraph::get.vertex.attribute(g, dim)) %>%
    `is.na<-`(as.character(unique(igraph::get.vertex.attribute(g, dim))))

  for(i in 1:nrow(df)){
      natt <- igraph::get.vertex.attribute(g, dim, unique(igraph::neighbors(g, df[i, "PPID"], mode = mode)))
      for(j in 1:length(values)){
        df[i, values[j]] <- sum(natt==values[j])
      }
  }

  df <- df %>%
    tidyr::pivot_longer(-c(PPID, !!dim), names_to="variable") %>%
    group_by(PPID, !!sym(dim)) %>%
    dplyr::mutate(prop2=(value/sum(value))^2) %>%
    dplyr::summarize(!!paste0("simpsd_",dim) := 1-sum(prop2), .groups="drop_last")

  if(include_att==FALSE) df <- df %>% dplyr::select(-!!sym(dim))

  if(std==FALSE) as.data.frame(df)
  else{
    df %>%
      dplyr::mutate(!!paste0("simpsd_",dim,"_std") := !!sym(paste0("simpsd_",dim))/(1-1/length(values))) %>%
      dplyr::select(-!!sym(paste0("simpsd_",dim))) %>%
      as.data.frame()
    }

  }
