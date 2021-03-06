#' Calculate p-perm
#'
#' Calculate two-sided exact p-perm value for a node permutation test, with optional plot of observed coefficient vs. distribution of coefficients from permuted networks
#'
#' @param obs Numeric; observed coefficient
#' @param permcoefs Numeric vector; coefficients derived from permuted networks
#' @param alpha Numeric; the criterion for significance (defaults to .05)
#' @param plot Logical; if TRUE (default), distribution plot will be included in output; if FALSE, only p-perm will be calculated
#' @param coefname Optional character string; used to specify type of coefficent for x-axis label of plot (if not provided, x-axis label will just be 'Coefficient')
#' @return Either a single numeric value representing p-perm if plot=FALSE; otherwise a list including both the plot and p-perm value
#' @export
pperm <- function(obs, permcoefs, alpha = .05, plot = TRUE, coefname = ""){

  if (plot==TRUE & !requireNamespace("ggplot2", quietly = TRUE)){
    warning("Package \"ggplot2\" is required to plot permutation test results. Returning p.perm value only.")
    plot <- FALSE
  }

  if(stats::median(permcoefs)<0){
    obs_shift <- obs + abs(stats::median(permcoefs))
    permcoefs_shift <- permcoefs + abs(stats::median(permcoefs))
  } else if(stats::median(permcoefs)>0){
    obs_shift <- obs - abs(stats::median(permcoefs))
    permcoefs_shift <- abs(permcoefs - stats::median(permcoefs))
  }

  pperm <- sum(abs(permcoefs_shift) >= abs(obs_shift))/length(permcoefs)

  if(plot==FALSE) round(pperm, digits=4)
  else{
    dist <- data.frame(permcoefs) %>%
      ggplot2::ggplot(ggplot2::aes(permcoefs)) +
      ggplot2::geom_histogram(color = "darkseagreen", fill = "darkseagreen2", bins=30) +
      ggplot2::geom_vline(aes(xintercept = obs), color = "black", lwd = 1, lty = 2) +
      ggplot2::geom_vline(aes(xintercept=as.numeric(stats::quantile(permcoefs, 1-(.5*alpha)))), color="red", lwd=0.5, lty=1) +
      ggplot2::geom_vline(aes(xintercept=as.numeric(stats::quantile(permcoefs, .5*alpha))), color="red", lwd=0.5, lty=1) +
      ggplot2::ylab("Frequency") +
      ggplot2::xlab(paste0(coefname, " Coefficient")) +
      ggplot2::theme_bw()

    return(list(p.perm=round(pperm, digits=4),
                plot=dist))
  }

}
