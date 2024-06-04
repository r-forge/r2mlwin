#' This function captures output files from MLwiN for estimation in
#' WinBUGS/OpenBUGS.
#' 
#' This function allows R to call WinBUGS using the output files from MLwiN.
#' This function uses functionalities in the \code{\link[R2WinBUGS]{R2WinBUGS-package}}
#' package.
#' 
#' @param datafile A file name where the BUGS data file will be saved in
#' .txt format.
#' @param initfiles A list of file names where the BUGS initial values will
#' be saved in .txt format.
#' @param modelfile A file name where the BUGS model will be saved in .txt
#' format.
#' @param parameters A vector of strings specifying coefficients to be
#' monitored.
#' @param n.chains The number of chains to be monitored.
#' @param n.iter The number of iterations for each chain
#' @param n.burnin The length of burn-in for each chain
#' @param n.thin Thinning rate
#' @param debug A logical value indicating whether (\code{TRUE}) or not
#' (\code{FALSE}; the default) to close the BUGS window after completion of the
#' model run
#' @param bugs.directory The full path of location where WinBUGS is installed 
#' (ignored if OpenBugs is \code{TRUE}).
#' @param bugsWorkingDir A directory where all the intermediate files are to be
#' stored; defaults to \code{tempdir()}.
#' @param OpenBugs If \code{TRUE}, OpenBUGS is used, if \code{FALSE} (the
#' default) WinBUGS is used.
#' @param cleanBugsWorkingDir If \code{TRUE}, the generated files will be
#' removed from the \code{bugsWorkingDir}; defaults to \code{FALSE}.
#' @param seed An integer specifying the random seed.
#'
#' @return Returns an \code{\link[coda]{mcmc}} object.
#'
#' @author Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne,
#' W.J. (2016) Centre for Multilevel Modelling, University of Bristol.
#'
#' @seealso \code{\link{runMLwiN}},\code{\link[R2WinBUGS]{bugs}}
#' @export
mlwin2bugs <- function(datafile, initfiles, modelfile, parameters = NULL,
                       n.chains = 1, n.iter = 5500, n.burnin = 500, n.thin = 1, 
                       debug = FALSE, bugs.directory = NULL, bugsWorkingDir = tempdir(),
                       OpenBugs = FALSE, cleanBugsWorkingDir = FALSE, seed = NULL){

  program <- "OpenBUGS"
  if (!OpenBugs) {
    if (requireNamespace("R2WinBUGS") == FALSE) {
      stop("Package R2WinBUGS is required to use this function")
    }
    chain.bugs <- R2WinBUGS::bugs(data = datafile, inits = initfiles,
                               parameters.to.save = parameters, model.file = modelfile, 
                               n.chains = n.chains, n.iter = n.iter, n.burnin = n.burnin, n.thin = n.thin,
                               debug = debug, DIC = TRUE, codaPkg = FALSE,
                               program = program, working.directory = bugsWorkingDir, clearWD = cleanBugsWorkingDir,
                               bugs.directory = bugs.directory, bugs.seed = seed)
  } else {
    if (requireNamespace("R2OpenBUGS") == FALSE) {
      stop("Package R2OpenBUGS is required to use this function")
    }
    if (is.null(seed)) {
      seed <- 1
    }
    chain.bugs <- R2OpenBUGS::bugs(data = datafile, inits = initfiles,
                               parameters.to.save = parameters, n.iter = n.iter, model.file = modelfile, 
                               n.chains = n.chains, n.burnin = n.burnin, n.thin = n.thin,
                               debug = debug, DIC = TRUE, codaPkg = FALSE,
                               OpenBUGS.pgm = bugs.directory, working.directory = bugsWorkingDir, clearWD = cleanBugsWorkingDir,
                               bugs.seed = seed)
  }

  chains.bugs.mcmc <- coda::as.mcmc.list(chain.bugs)

  chains.bugs.mcmc
}
