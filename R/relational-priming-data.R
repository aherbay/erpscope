#' Herbay, Royle and Steinhauer data on relational priming
#'
#' Data from a nERP experiment on relational priming, with data on 11 participants and 3 conditions
#' The outcome is the EEG between -650ms and 900ms
#'
#'
#' @docType data
#'
#' @usage data(relpriming)
#'
#'
#' @keywords datasets
#'
#' @references Herbay AH, Royle P, Steinhauer K,
#'  (in preparation) Relational Priming POP2 title
#' ([PubMed](https://www.ncbi.nlm.nih.gov/pubmed/23979570))
#'
#'
#' @examples
#' data(relpriming)
#'
#' \donttest{
#' iplotCurves(phe, times, phe[,c(61,121)], phe[,c(121,181)],
#'             chartOpts=list(curves_xlab="Time (hours)", curves_ylab="Root tip angle (degrees)",
#'                            scat1_xlab="Angle at 2 hrs", scat1_ylab="Angle at 4 hrs",
#'                            scat2_xlab="Angle at 4 hrs", scat2_ylab="Angle at 6 hrs"))}
"relpriming"
