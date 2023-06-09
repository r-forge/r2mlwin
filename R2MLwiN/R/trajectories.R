#' Plots MCMC chain trajectories
#'
#' This function draws trajectories of MCMC chains.
#'
#' @param object An \code{\link{mlwinfitMCMC-class}}, \code{\link[coda]{mcmc}} or \code{\link[coda]{mcmc.list}} object,
#' or other object that can be converted to an \code{\link[coda]{mcmc}} object.
#' @param Range An integer vector of length two specifying the first and last
#' iterations of the chains.
#' @param selected A character vector specifying the selected chains to be
#' plotted.
#'
#' @author Zhang, Z., Charlton, C.M.J., Parker, R.M.A., Leckie, G., and Browne,
#' W.J. (2016) Centre for Multilevel Modelling, University of Bristol.
#'
#' @seealso \code{\link{sixway}}
#'
#' @examples
#'
#' \dontrun{
#' library(R2MLwiN)
#' # NOTE: if MLwiN not saved in location R2MLwiN defaults to, specify path via:
#' # options(MLwiN_path = 'path/to/MLwiN vX.XX/')
#' # If using R2MLwiN via WINE, the path may look like this:
#' # options(MLwiN_path = '/home/USERNAME/.wine/drive_c/Program Files (x86)/MLwiN vX.XX/')
#'
#' ## Example: tutorial
#' data(tutorial, package = "R2MLwiN")
#'
#' (mymodel <- runMLwiN(normexam ~ 1 + standlrt + (1 + standlrt | student),
#'                      estoptions = list(EstM = 1), data = tutorial))
#'
#' trajectories(mymodel, Range = c(4501, 5000))
#' }
#'
#' @export
trajectories <- function(object, Range = c(1, 5000), selected = NULL) {
  # This function draws trajectories of the chains for each parameter estimate
  
  if (inherits(object, "mlwinfitMCMC")) {
    chains <- object@chains
  } else {
    if (coda::is.mcmc(object) || coda::is.mcmc.list(object)) {
      chains <- object
    } else {
      chains <- coda::mcmc(object)
    }
  }
  
  if (is.null(selected)) {
    selected <- coda::varnames(chains)
  }
  
  chains <- stats::window(chains, Range[1], Range[2])
  
  if (.Platform$GUI == "RStudio") {
    o = tolower(Sys.info()["sysname"])
    a = switch(o,
               "darwin"  = "quartz",
               "linux"   = "x11",
               "windows" = "windows")
    options("device" = a)
  }  
  
  if (coda::nvar(chains) == 1)
    opar <- graphics::par(mfrow = c(1, 1))
  if (coda::nvar(chains) == 2)
    opar <- graphics::par(mfrow = c(2, 1))
  if (coda::nvar(chains) == 3)
    opar <- graphics::par(mfrow = c(3, 1))
  if (coda::nvar(chains) == 4)
    opar <- graphics::par(mfrow = c(2, 2))
  if (coda::nvar(chains) > 4)
    opar <- graphics::par(mfrow = c(3, 2))
  if (coda::nvar(chains) > 6)
    opar <- graphics::par(mfrow = c(3, 3))
  
  nwindows <- 0
 
  for (param in coda::varnames(chains, allow.null=FALSE)) {
    if (coda::is.mcmc(chains)) {
      if (is.null(rownames(chains))) {
        xvals <- 1:length(chains)
        yvals <- chains
      } else {
        xvals <- rownames(chains)
        yvals <- chains[, param]
      }
      graphics::plot(xvals, yvals, xlab = "iteration", ylab = param, type = "l")
    } else { # mcmc.list
      ymin <- min(unlist(chains[1:coda::niter(chains), param]))
      ymax <- max(unlist(chains[1:coda::niter(chains), param]))
      graphics::plot(rownames(chains[[1]]), chains[[1]][, param], xlab = "iteration", ylab = param, type = "l", ylim = c(ymin,
                                                                                                               ymax), col = 1)
      for (j in 2:coda::nchain(chains)) {
        graphics::lines(rownames(chains[[j]]), chains[[j]][, param], col = j)
      }
    }
    nwindows <- nwindows + 1
    if ((nwindows%%9) == 0) {
      grDevices::dev.new()
      opar <- graphics::par(mfrow = c(3, 3))
    }
  }
  on.exit(graphics::par(opar))
}
