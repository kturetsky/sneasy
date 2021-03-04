#' Calculate node-level tie reciprocity
#'
#' Calculate proportion of reciprocal ties out of total ties for each node from an edgelist.
#'
#' Base calculations either on all nodes and ties in edgelist or only on those who had the opportunity to nominate ties (e.g., completed the network survey).
#' Self-loops (individuals nominating themselves) are not included.
#' Individuals who nominated 0 ties or just themselves have a reciprocity of NaN.
#'
#' @param el Edgelist with "from" and "to" columns
#' @param type Character string; either "all" (default; calculates node-level reciprocity using all nodes and ties in edgelist) or "complete" (calculates reciprocity only based on nominees who had the potential to reciprocate because they completed the network survey--this assumes that all individuals who completed the survey and no individuals who did not are listed in the 'from' column)
#' @return A dataframe containing node IDs and tie reciprocity proportions
#' @export
tierecip <- function(el, type = "all") {

  if(!type %in% c("all", "complete")) stop("Valid values of 'type' are 'all' or 'complete'")

  el <- el %>%
    dplyr::mutate(to = as.character(to),
                  from = as.character(from),
                  tierecip = NA)

  if(type=="all"){

    for(i in 1:nrow(el)){
      if(is.na(el[i,]$to)) next
      el[i,]$tierecip <- dplyr::if_else(any(el$to==el[i,]$from & el$from==el[i,]$to), 1L, 0L)
    }
  }

  else if(type=="complete"){

    for(i in 1:nrow(el)){
      if(is.na(el[i,]$to) | !(el[i,]$to %in% el$from)) next
      el[i,]$tierecip <- dplyr::if_else(any(el$to==el[i,]$from & el$from==el[i,]$to), 1L, 0L)
    }
  }

  recipprop <- el %>%
    dplyr::mutate(tierecip = dplyr::if_else(to==from, NA_integer_, tierecip)) %>%
    group_by(from) %>%
    dplyr::summarise(recip = mean(tierecip, na.rm=TRUE)) %>%
    dplyr::rename(PPID = from)

  if(type=="complete") recipprop <- recipprop %>% dplyr::rename(recip_comp = recip)

  as.data.frame(recipprop)

}
