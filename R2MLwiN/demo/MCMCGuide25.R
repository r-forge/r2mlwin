############################################################################
#     MLwiN MCMC Manual
#
# 25  Hierarchical Centring . . . . . . . . . . . . . . . . . . . . . . .401
#
#     Browne, W.J. (2009) MCMC Estimation in MLwiN, v2.13. Centre for
#     Multilevel Modelling, University of Bristol.
############################################################################
#     R script to replicate all analyses using R2MLwiN
#
#     Zhang, Z., Charlton, C., Parker, R, Leckie, G., and Browne, W.J.
#     Centre for Multilevel Modelling, 2012
#     http://www.bristol.ac.uk/cmm/software/R2MLwiN/
############################################################################

# 25.1 What is hierarchical centering? . . . . . . . . . . . . . . . . . 401

# 25.2 Centring Normal models using WinBUGS . . . . . . . . . . . . . . .403

library(R2MLwiN)
# MLwiN folder
mlwin <- getOption("MLwiN_path")
while (!file.access(mlwin, mode = 1) == 0) {
  cat("Please specify the root MLwiN folder or the full path to the MLwiN executable:\n")
  mlwin <- scan(what = character(0), sep = "\n")
  mlwin <- gsub("\\", "/", mlwin, fixed = TRUE)
}
options(MLwiN_path = mlwin)

# Change contrasts if wish to avoid warning indicating that, by default,
# specified contrasts for ordered predictors will be ignored by runMLwiN
# (they will be fitted as "contr.treatment" regardless of this setting). To
# enable specified contrasts, set allowcontrast to TRUE (this will be the
# default in future package releases). NB at the end of this script, the
# specification for contrasts is changed back.
my_contrasts <- options("contrasts")$contrasts
options(contrasts = c(unordered = "contr.treatment",
                      ordered = "contr.treatment"))

# As an alternative to changing contrasts, can instead use C() to specify
# contrasts for ordered predictors in formula object, e.g.:

# (mymodel <- runMLwiN(logit(use) ~ 1 + age + C(lc, "contr.treatment") + urban +
#                        (1 + urban | district),
#                      D = "Binomial",
#                      estoptions = list(EstM = 1,
#                                        mcmcOptions = list(hcen = 2)),
#                      data = bang1,
#                      allowcontrast = TRUE))

# User's input if necessary

## Read tutorial data
data(tutorial, package = "R2MLwiN")

## Hierarchical centring at level 2 (DO NOT USE VERSION 2.25; the bug has been fixed for VERSION 2.26)

mymodel <- runMLwiN(normexam ~ 1 + standlrt + (1 | school) + (1 | student), estoptions = list(EstM = 1, mcmcOptions = list(hcen = 2), 
  show.file = TRUE), BUGO = c(version = 4, n.chains = 1, debug = FALSE, seed = 1, OpenBugs = TRUE), 
  data = tutorial)

summary(mymodel)
sixway(mymodel[, "beta[1]", drop = FALSE])

# 25.3 Binomial hierarchical centering algorithm . . . . . . . . . . . . 408

# 25.4 Binomial example in practice . . . . . . . . . . . . . . . . . . .410

## Read bang1 data
data(bang1, package = "R2MLwiN")

## Define the model


## Hierarchical centring at level 2

(mymodel <- runMLwiN(logit(use) ~ 1 + age + lc + urban + (1 + urban | district), D = "Binomial", estoptions = list(EstM = 1, 
  mcmcOptions = list(hcen = 2)), data = bang1))

trajectories(mymodel)

## Hierarchical centring at level 2 + Orthogonal updates

(mymodel <- runMLwiN(logit(use) ~ 1 + age + lc + urban + (1 + urban | district), D = "Binomial", estoptions = list(EstM = 1, 
  mcmcOptions = list(hcen = 2, orth = 1)), data = bang1))

trajectories(mymodel)

# 25.5 The Melanoma example . . . . . . . . . . . . . . . . . . . . . . .414

## Read mmmec data
data(mmmec, package = "R2MLwiN")

## Define the model Hierarchical centring at level 2
(mymodel <- runMLwiN(log(obs) ~ 0 + nation + nation:uvbi + offset(log(exp)) + (1 | region), D = "Poisson", estoptions = list(EstM = 1, 
  mcmcMeth = list(iterations = 50000), mcmcOptions = list(hcen = 2)), data = mmmec))

sixway(mymodel@chains[, "FP_nationBelgium", drop = FALSE], acf.maxlag = 100, "beta_1")

## Hierarchical centring at level 2 + Orthogonal updates
(mymodel <- runMLwiN(log(obs) ~ 0 + nation + nation:uvbi + offset(log(exp)) + (1 | region), D = "Poisson", estoptions = list(EstM = 1, 
  mcmcMeth = list(iterations = 50000), mcmcOptions = list(orth = 1, hcen = 2)), data = mmmec))

sixway(mymodel@chains[, "FP_nationBelgium", drop = FALSE], acf.maxlag = 100, "beta_1")

# 25.6 Normal response models in MLwiN . . . . . . . . . . . . . . . . . 419

## Read tutorial data
data(tutorial, package = "R2MLwiN")

## Define the model Univariate MH Hierarchical centring at level 2

(mymodel <- runMLwiN(normexam ~ 1 + standlrt + (1 | school) + (1 | student), estoptions = list(EstM = 1, mcmcMeth = list(fixM = 2, 
  residM = 2), mcmcOptions = list(hcen = 2)), data = tutorial))

trajectories(mymodel, Range = c(4501, 5000))
## Gibbs Hierarchical centring at level 2

(mymodel <- runMLwiN(normexam ~ 1 + standlrt + (1 | school) + (1 | student), estoptions = list(EstM = 1, mcmcOptions = list(hcen = 2)), 
  data = tutorial))

trajectories(mymodel, Range = c(4501, 5000))

# Chapter learning outcomes . . . . . . . . . . . . . . . . . . . . . . .422

# Addendum: changing contrasts back to pre-existing . . . . . . . . . . . NA

# Following re-specification of contrast settings towards the start of this
# script, change contrasts back to pre-existing:
options(contrasts = my_contrasts)

############################################################################
