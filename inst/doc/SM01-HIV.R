## -----------------------------------------------------------------------------
#' @title Write a monetary value
#' @param x Monetary value, or vector of values
#' @param p Logical; if TRUE show value to nearest penny, cent etc. If FALSE
#' show it to the nearest pound, dollar, euro etc.
#' @noRd
gbp <- function(x, p = FALSE) {
  digits <- if (p) 2L else 0L
  s <- format(
    x = vapply(X = x, FUN.VALUE = 1.0, FUN = round, digits = digits),
    digits = NULL,
    nsmall = digits,
    scientific = FALSE,
    big.mark = ","
  )
  return(s)
}

## -----------------------------------------------------------------------------
library(rdecision)

## -----------------------------------------------------------------------------
# create Markov states
sA <- MarkovState$new("A")
sB <- MarkovState$new("B")
sC <- MarkovState$new("C")
sD <- MarkovState$new("D", utility = 0.0)
# create transitions
tAA <- Transition$new(sA, sA)
tAB <- Transition$new(sA, sB)
tAC <- Transition$new(sA, sC)
tAD <- Transition$new(sA, sD)
tBB <- Transition$new(sB, sB)
tBC <- Transition$new(sB, sC)
tBD <- Transition$new(sB, sD)
tCC <- Transition$new(sC, sC)
tCD <- Transition$new(sC, sD)
tDD <- Transition$new(sD, sD)
# set discount rates
cDR <- 6.0 # annual discount rate, costs (%)
oDR <- 0.0 # annual discount rate, benefits (%)
# construct the model
m <- SemiMarkovModel$new(
  V = list(sA, sB, sC, sD),
  E = list(tAA, tAB, tAC, tAD, tBB, tBC, tBD, tCC, tCD, tDD),
  discount.cost = cDR / 100.0,
  discount.utility = oDR / 100.0
)

## -----------------------------------------------------------------------------
# drug costs
cAZT <- 2278.0 # zidovudine drug cost
cLam <- 2087.0 # lamivudine drug cost

# direct medical and community costs
dmca <- 1701.0 # direct medical costs associated with state A
dmcb <- 1774.0 # direct medical costs associated with state B
dmcc <- 6948.0 # direct medical costs associated with state C
ccca <- 1055.0 # Community care costs associated with state A
cccb <- 1278.0 # Community care costs associated with state B
cccc <- 2059.0 # Community care costs associated with state C

# occupancy costs with monotherapy
cAm <- dmca + ccca + cAZT
cBm <- dmcb + cccb + cAZT
cCm <- dmcc + cccc + cAZT

# occupancy costs with combination therapy
cAc <- dmca + ccca + cAZT + cLam
cBc <- dmcb + cccb + cAZT + cLam
cCc <- dmcc + cccc + cAZT + cLam

## -----------------------------------------------------------------------------
RR <- 0.509

## -----------------------------------------------------------------------------
# transition counts
nAA <- 1251L
nAB <- 350L
nAC <- 116L
nAD <- 17L
nBB <- 731L
nBC <- 512L
nBD <- 15L
nCC <- 1312L
nCD <- 437L
# create transition matrix
nA <- nAA + nAB + nAC + nAD
nB <- nBB + nBC + nBD
nC <- nCC + nCD
Ptm <- matrix(
  c(nAA / nA, nAB / nA, nAC / nA, nAD / nA,
    0.0, nBB / nB, nBC / nB, nBD / nB,
    0.0,      0.0, nCC / nC, nCD / nC,
    0.0,      0.0,      0.0,      1.0),
  nrow = 4L, byrow = TRUE,
  dimnames = list(
    source = c("A", "B", "C", "D"), target = c("A", "B", "C", "D")
  )
)

## -----------------------------------------------------------------------------
#' @title Create a placeholder image in a png file
#' @description Draws a rectangle with a diagonal using the grid package.
#' @param pngfile name of png file to create.
#' @param width width of image in pixels
#' @param height height of image in pixels
#' @return Name of the png file written to.
#' @noRd
placeholder <- function(pngfile = tempfile(fileext = ".png"), width = 480L,
                        height = 320L) {
  grDevices::png(pngfile, width = width, height = height)
  grid::grid.newpage()
  grid::grid.move.to(
    x = grid::unit(0.0, "npc"),
    y = grid::unit(0.0, "npc")
  )
  grid::grid.line.to(
    x = grid::unit(1.0, "npc"),
    y = grid::unit(1.0, "npc")
  )
  grid::grid.move.to(
    x = grid::unit(0.0, "npc"),
    y = grid::unit(1.0, "npc")
  )
  grid::grid.line.to(
    x = grid::unit(1.0, "npc"),
    y = grid::unit(0.0, "npc")
  )
  invisible(grDevices::dev.off())
  return(pngfile)
}

#' @title Render a DOT format graph as a png file.
#' @description Uses the \code{dot} command line tool from the graphviz project,
#' if it is available on the host system, or creates a placeholder image if not.
#' @param dot GraphViz dot representation in character vector form.
#' @param pngfile path of png file to create.
#' @return pathname of the png file created (including extension).
#' @noRd
gv2png <- function(dot, pngfile = tempfile(fileext = ".png")) {
  cmddot <- Sys.which("dot")
  if (nchar(cmddot["dot"]) > 0L) {
    dotfile <- tempfile(fileext = ".gv")
    writeLines(dot, con = dotfile)
    system2(command = cmddot["dot"], args = c("-Tpng", "-o", pngfile, dotfile))
  } else {
    pngfile <- placeholder(pngfile = pngfile)
  }
  return(pngfile)
}

## -----------------------------------------------------------------------------
# images created with dot are more compact than DiagrammeR.
pngfile <- gv2png(
  dot = m$as_DOT(rankdir = "TB", width = 7.0, height = 7.0)
)
knitr::include_graphics(pngfile)

## -----------------------------------------------------------------------------
# function to run model for 20 years of monotherapy
run_mono <- function(Ptm, cAm, cBm, cCm, hcc = FALSE) {
  # create starting populations
  N <- 1000L
  populations <- c(A = N, B = 0L, C = 0L, D = 0L)
  m$reset(populations)
  # set costs
  sA$set_cost(cAm)
  sB$set_cost(cBm)
  sC$set_cost(cCm)
  # set transition probabilities
  m$set_probabilities(Ptm)
  # run 20 cycles
  tr <- m$cycles(
    ncycles = 20L, hcc.pop = hcc, hcc.cost = FALSE, hcc.QALY = hcc
  )
  return(tr)
}

## -----------------------------------------------------------------------------
MT.mono <- run_mono(Ptm, cAm, cBm, cCm)
el.mono <- sum(MT.mono$QALY)
cost.mono <- sum(MT.mono$Cost)

## -----------------------------------------------------------------------------
# annual probabilities modified by treatment effect
pAB <- RR * nAB / nA
pAC <- RR * nAC / nC
pAD <- RR * nAD / nA
pBC <- RR * nBC / nB
pBD <- RR * nBD / nB
pCD <- RR * nCD / nC
# annual transition probability matrix
Ptc <- matrix(
  c(1.0 - pAB - pAC - pAD,               pAB,         pAC, pAD,
    0.0, (1.0 - pBC - pBD),         pBC, pBD,
    0.0,               0.0, (1.0 - pCD), pCD,
    0.0,               0.0,         0.0, 1.0),
  nrow = 4L, byrow = TRUE,
  dimnames = list(
    source = c("A", "B", "C", "D"), target = c("A", "B", "C", "D")
  )
)

## -----------------------------------------------------------------------------
# function to run model for 2 years of combination therapy and 18 of monotherapy
run_comb <- function(Ptm, cAm, cBm, cCm, Ptc, cAc, cBc, cCc, hcc = FALSE) {
  # set populations
  N <- 1000L
  populations <- c(A = N, B = 0L, C = 0L, D = 0L)
  m$reset(populations)
  # set the transition probabilities accounting for treatment effect
  m$set_probabilities(Ptc)
  # set the costs including those for the additional drug
  sA$set_cost(cAc)
  sB$set_cost(cBc)
  sC$set_cost(cCc)
  # run first 2 yearly cycles with additional drug costs and tx effect
  tr <- m$cycles(2L, hcc.pop = hcc, hcc.cost = FALSE, hcc.QALY = hcc)
  # save the state populations after 2 years
  populations <- m$get_populations()
  # revert probabilities to those without treatment effect
  m$set_probabilities(Ptm)
  # revert costs to those without the extra drug
  sA$set_cost(cAm)
  sB$set_cost(cBm)
  sC$set_cost(cCm)
  # restart the model with populations from first 2 years with extra drug
  m$reset(
    populations,
    icycle = 2L,
    elapsed = as.difftime(365.25 * 2.0, units = "days")
  )
  # run for next 18 years, combining the traces
  tr <- rbind(
    tr,
    m$cycles(ncycles = 18L, hcc.pop = hcc, hcc.cost = FALSE, hcc.QALY = hcc)
  )
  # return the trace
  return(tr)
}

## -----------------------------------------------------------------------------
MT.comb <- run_comb(Ptm, cAm, cBm, cCm, Ptc, cAc, cBc, cCc)
el.comb <- sum(MT.comb$QALY)
cost.comb <- sum(MT.comb$Cost)
icer <- (cost.comb - cost.mono) / (el.comb - el.mono)

## -----------------------------------------------------------------------------
MT.mono.hcc <- run_mono(Ptm, cAm, cBm, cCm, hcc = TRUE)
el.mono.hcc <- sum(MT.mono.hcc$QALY)
cost.mono.hcc <- sum(MT.mono.hcc$Cost)
MT.comb.hcc <- run_comb(Ptm, cAm, cBm, cCm, Ptc, cAc, cBc, cCc, hcc = TRUE)
el.comb.hcc <- sum(MT.comb.hcc$QALY)
cost.comb.hcc <- sum(MT.comb.hcc$Cost)
icer.hcc <- (cost.comb.hcc - cost.mono.hcc) / (el.comb.hcc - el.mono.hcc)

## -----------------------------------------------------------------------------
# direct medical and community costs (modelled as gamma distributions)
dmca <- GammaModVar$new("dmca", "GBP", shape = 1.0, scale = 1701.0)
dmcb <- GammaModVar$new("dmcb", "GBP", shape = 1.0, scale = 1774.0)
dmcc <- GammaModVar$new("dmcc", "GBP", shape = 1.0, scale = 6948.0)
ccca <- GammaModVar$new("ccca", "GBP", shape = 1.0, scale = 1055.0)
cccb <- GammaModVar$new("cccb", "GBP", shape = 1.0, scale = 1278.0)
cccc <- GammaModVar$new("cccc", "GBP", shape = 1.0, scale = 2059.0)

# occupancy costs with monotherapy
cAm <- ExprModVar$new("cA", "GBP", rlang::quo(dmca + ccca + cAZT))
cBm <- ExprModVar$new("cB", "GBP", rlang::quo(dmcb + cccb + cAZT))
cCm <- ExprModVar$new("cC", "GBP", rlang::quo(dmcc + cccc + cAZT))

# occupancy costs with combination therapy
cAc <- ExprModVar$new("cAc", "GBP", rlang::quo(dmca + ccca + cAZT + cLam))
cBc <- ExprModVar$new("cBc", "GBP", rlang::quo(dmcb + cccb + cAZT + cLam))
cCc <- ExprModVar$new("cCc", "GBP", rlang::quo(dmcc + cccc + cAZT + cLam))

## -----------------------------------------------------------------------------
RR <- LogNormModVar$new(
  "Tx effect", "RR", p1 = 0.509, p2 = (0.710 - 0.365) / (2.0 * 1.96), "LN7"
)

## -----------------------------------------------------------------------------
# function to generate a probabilistic transition matrix
pt_prob <- function() {
  # create Dirichlet distributions for conditional probabilities
  DA <- DirichletDistribution$new(c(1251L, 350L, 116L, 17L)) # from A # nolint
  DB <- DirichletDistribution$new(c(731L, 512L, 15L))  # from B # nolint
  DC <- DirichletDistribution$new(c(1312L, 437L)) # from C # nolint
  # sample from the Dirichlet distributions
  DA$sample()
  DB$sample()
  DC$sample()
  # create the transition matrix
  Pt <- matrix(
    c(DA$r(), c(0.0, DB$r()), c(0.0, 0.0, DC$r()), c(0.0, 0.0, 0.0, 1.0)),
    byrow = TRUE,
    nrow = 4L,
    dimnames = list(
      source = c("A", "B", "C", "D"), target = c("A", "B", "C", "D")
    )
  )
  return(Pt)
}

## -----------------------------------------------------------------------------
# create matrix to hold the incremental costs and life years for each run
psa <- matrix(
  data = NA_real_, nrow = 1000L, ncol = 5L,
  dimnames = list(
    NULL, c("el.mono", "cost.mono", "el.comb", "cost.comb", "icer")
  )
)

# run the model repeatedly
for (irun in seq_len(nrow(psa))) {

  # sample variables from their uncertainty distributions
  cAm$set("random")
  cBm$set("random")
  cCm$set("random")
  cAc$set("random")
  cBc$set("random")
  cCc$set("random")
  RR$set("random")

  # sample the probability transition matrix from observed counts
  Ptm <- pt_prob()

  # run monotherapy model
  MT.mono <- run_mono(Ptm, cAm, cBm, cCm, hcc = TRUE)
  el.mono <- sum(MT.mono$QALY)
  cost.mono <- sum(MT.mono$Cost)
  psa[[irun, "el.mono"]] <- el.mono
  psa[[irun, "cost.mono"]] <- cost.mono

  # create Pt for combination therapy (Briggs applied the RR to the transition
  # probabilities - not recommended, but done here for reproducibility).
  Ptc <- Ptm
  for (i in 1L:4L) {
    for (j in 1L:4L) {
      Ptc[[i, j]] <- ifelse(i == j, NA, RR$get() * Ptc[[i, j]])
    }
    Ptc[i, which(is.na(Ptc[i, ]))] <- 1.0 - sum(Ptc[i, ], na.rm = TRUE)
  }

  # run combination therapy model
  MT.comb <- run_comb(Ptm, cAm, cBm, cCm, Ptc, cAc, cBc, cCc, hcc = TRUE)
  el.comb <- sum(MT.comb$QALY)
  cost.comb <- sum(MT.comb$Cost)
  psa[[irun, "el.comb"]] <- el.comb
  psa[[irun, "cost.comb"]] <- cost.comb

  # calculate the icer
  psa[[irun, "icer"]] <- (cost.comb - cost.mono) / (el.comb - el.mono)
}

