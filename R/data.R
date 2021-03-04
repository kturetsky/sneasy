#' Biology student network
#'
#' Sample wide social network dataset in format required by getEL, from
#' Turetsky et al. (2020), 'A psychological intervention strengthens
#' students' peer social networks and promotes persistence in STEM,'
#' \url{https://advances.sciencemag.org/content/6/45/eaba9221}
#'
#' @format A data frame with 328 rows and 37 variables:
#' \describe{
#'   \item{PPID}{Participant ID number}
#'   \item{Friend1Name}{Participant ID number of first nominated friend}
#'   \item{Friend2Name}{...}
#'   \item{Friend3Name}{...}
#'   \item{Friend4Name}{...}
#'   \item{Friend5Name}{...}
#'   \item{Friend6Name}{...}
#'   \item{Friend1Strength}{Strength of relationship (e.g., felt closeness) with first nominated friend}
#'   \item{Friend2Strength}{...}
#'   \item{Friend3Strength}{...}
#'   \item{Friend4Strength}{...}
#'   \item{Friend5Strength}{...}
#'   \item{Friend6Strength}{...}
#'   \item{Study1Name}{Participant ID number of first nominated study partner}
#'   \item{Study2Name}{...}
#'   \item{Study3Name}{...}
#'   \item{Study4Name}{...}
#'   \item{Study5Name}{...}
#'   \item{Study6Name}{...}
#'   \item{Study1Strength}{Strength of relationship (e.g., frequency of studying together) with first nominated study partner}
#'   \item{Study2Strength}{...}
#'   \item{Study3Strength}{...}
#'   \item{Study4Strength}{...}
#'   \item{Study5Strength}{...}
#'   \item{Study6Strength}{...}
#'   \item{Support1Name}{Participant ID number of first nominated support provider}
#'   \item{Support2Name}{...}
#'   \item{Support3Name}{...}
#'   \item{Support4Name}{...}
#'   \item{Support5Name}{...}
#'   \item{Support6Name}{...}
#'   \item{Support1Strength}{Strength of relationship (e.g., likelihood of seeking support) with first nominated support provider}
#'   \item{Support2Strength}{...}
#'   \item{Support3Strength}{...}
#'   \item{Support4Strength}{...}
#'   \item{Support5Strength}{...}
#'   \item{Support6Strength}{...}
#' }
#' @source \url{https://osf.io/6dqf3/}
"bionet"


#' Attribute file
#'
#' Sample wide attribute file to accompany biology student network.
#' All data in this file was randomly generated (does not reflect
#' true attribute data of the biology students).
#'
#' @format A data frame with 328 rows and 4 variables:
#' \describe{
#'   \item{PPID}{Participant ID number}
#'   \item{gender}{F or M}
#'   \item{race}{Black, EAsian, Latinx, SAsian, or White}
#'   \item{age}{Age in years from 18-22}
#'   \item{polorient}{Political orientation from 1 (most liberal) - 100 (most conservative)}
#' }
"attfile"
