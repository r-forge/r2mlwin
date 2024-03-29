#' Hungarian component of 2nd International Science Survey, '84; see Goldstein
#' 2003
#' 
#' Hungarian component of 2nd International Science Survey, consisting of 2439
#' women across 99 districts.
#' 
#' The \code{hungary1} dataset is one of the sample datasets provided with the
#' multilevel-modelling software package MLwiN (Rasbash et al., 2009).
#' Originally analysed in Goldstein (2003), further details can also be found
#' in Browne (2012).
#' 
#' @docType data
#' @format A data frame with 2439 observations on the following 10 variables:
#' \describe{
#' \item{school}{Identifying code for each school (level 2 unit).}
#' \item{female}{Gender indicator: a factor with levels \code{Male} and
#' \code{Female}.}
#' \item{es_core}{Core Earth Sciences test result.}
#' \item{biol_core}{Core Biology test result.}
#' \item{biol_r3}{Optional Biology test result.}
#' \item{biol_r4}{Optional Biology test result.}
#' \item{phys_core}{Core Physics test result.}
#' \item{phys_r2}{Optional Physics test result.}
#' \item{cons}{Constant(=1).}
#' \item{student}{Identifying code for each student (level 1 unit).}
#' }
#' @source Browne, W. J. (2012) \emph{MCMC Estimation in MLwiN Version 2.26.}
#' University of Bristol: Centre for Multilevel Modelling.
#' 
#' Goldstein, H.. (2003) Multilevel Statistical Models. Third Edition. London,
#' Edward Arnold.
#' 
#' Rasbash, J., Charlton, C., Browne, W.J., Healy, M. and Cameron, B. (2009)
#' \emph{MLwiN Version 2.1.} Centre for Multilevel Modelling, University of
#' Bristol.
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' 
#' data(hungary1, package = "R2MLwiN")
#' 
#' (mymodel <- runMLwiN(c(es_core, biol_core, biol_r3, biol_r4, phys_core, phys_r2) ~ 
#'   1 + female + (1 | school) + (1 | student),
#'   D = "Multivariate Normal", estoptions = list(EstM = 1), data = hungary1))
#' 
#' }
#' 
"hungary1"