## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE,
  fig.keep = "last",
  fig.align = "center",
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library("rdecision")

## ----model-variables, echo=TRUE-----------------------------------------------
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
# Healthcare system costs
dmca <- 1701.0 # direct medical costs associated with state A
dmcb <- 1774.0 # direct medical costs associated with state B
dmcc <- 6948.0 # direct medical costs associated with state C
ccca <- 1055.0 # Community care costs associated with state A
cccb <- 1278.0 # Community care costs associated with state B
cccc <- 2059.0 # Community care costs associated with state C
# Drug costs
cAZT <- 2278.0 # zidovudine drug cost
cLam <- 2087.0 # lamivudine drug cost
# Treatment effect
RR <- 0.509 
# Discount rates
cDR <- 6.0 # annual discount rate, costs (%)
oDR <- 0.0 # annual discount rate, benefits (%)

## ----model, echo=TRUE---------------------------------------------------------
# create Markov states for monotherapy (zidovudine only)
sAm <- MarkovState$new("A", cost = dmca + ccca + cAZT)
sBm <- MarkovState$new("B", cost = dmcb + cccb + cAZT)
sCm <- MarkovState$new("C", cost = dmcc + cccc + cAZT)
sDm <- MarkovState$new("D", cost = 0.0, utility = 0.0)
# create transitions
tAAm <- Transition$new(sAm, sAm)
tABm <- Transition$new(sAm, sBm)
tACm <- Transition$new(sAm, sCm)
tADm <- Transition$new(sAm, sDm)
tBBm <- Transition$new(sBm, sBm)
tBCm <- Transition$new(sBm, sCm)
tBDm <- Transition$new(sBm, sDm)
tCCm <- Transition$new(sCm, sCm)
tCDm <- Transition$new(sCm, sDm)
tDDm <- Transition$new(sDm, sDm)
# construct the model
m.mono <- SemiMarkovModel$new(
  V = list(sAm, sBm, sCm, sDm),
  E = list(tAAm, tABm, tACm, tADm, tBBm, tBCm, tBDm, tCCm, tCDm, tDDm),
  discount.cost = cDR / 100.0,
  discount.utility = oDR / 100.0
)

## ----setprobs, echo=TRUE------------------------------------------------------
nA <- nAA + nAB + nAC + nAD
nB <- nBB + nBC + nBD
nC <- nCC + nCD
Pt <- matrix(
  c(nAA / nA, nAB / nA, nAC / nA, nAD / nA, 
         0.0, nBB / nB, nBC / nB, nBD / nB,
         0.0,      0.0, nCC / nC, nCD / nC,
         0.0,      0.0,      0.0,      1.0),
  nrow = 4L, byrow = TRUE, 
  dimnames = list(
    source = c("A", "B", "C", "D"), target = c("A", "B", "C", "D")
  )
)
m.mono$set_probabilities(Pt)

## ----caption, echo = FALSE----------------------------------------------------
f1c  <- paste("Figure 1. Markov model for comparison of HIV therapy.", 
              "A: 200 < cd4 < 500,", "B: cd4 < 200,", "C: AIDS,","D: Death.")

## ----draw, fig.cap = f1c, fig.asp = 0.21--------------------------------------
# This vignette is processed by knitr at build time (on the package developer's
# system) and so the dot tool only needs to be available there, not on any
# system used for checking or installing the vignettes, which operate on the
# versions moved to the doc directory at build time. 
pngfile <- "mono.png" 
dotfile <- "mono.gv"
# update the .gv file
DOT <- m.mono$as_DOT()
writeLines(DOT, con = dotfile)
# if graphviz is installed, create the .png file from the .gv file with the dot
# command line tool, otherwise fail gracefully and draw a placeholder image
pdot <- Sys.which("dot")
if (nchar(pdot["dot"]) > 0L) {
  system2(command = pdot["dot"], args = c("-Tpng", "-o", pngfile, dotfile))
} else {
  # create placeholder image
  grDevices::png(pngfile, width = 480L, height = 480L * 0.21)
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
}
knitr::include_graphics(path = pngfile)

## ----echo=FALSE---------------------------------------------------------------
DF <- m.mono$tabulate_states()
pander::pander(DF[, c("Name", "Cost")], justify = "lr")
rm(DF)

## ----echo=FALSE---------------------------------------------------------------
TM <- m.mono$transition_probabilities()
pander::pander(TM, emphasize.rownames = FALSE, justify = "lcccc")
rm(TM)

## ----monocycle, echo=TRUE-----------------------------------------------------
# create starting populations
N <- 1000L
populations <- c(A = N, B = 0L, C = 0L, D = 0L)
m.mono$reset(populations)
# run 20 cycles
MT.mono <- m.mono$cycles(ncycles = 20L, hcc.pop = FALSE, hcc.cost = FALSE)

## ----print_monocycle----------------------------------------------------------
keep <- c("Years", "A", "B", "C", "D", "Cost", "QALY")
pander::pander(MT.mono[, keep], row.names = FALSE, justify = "rrrrrrr", 
               round=c(2L, 0L, 0L, 0L, 0L, 0L, 3L))

## ----mon_results, echo=FALSE--------------------------------------------------
el.mono <- sum(MT.mono$QALY)
cost.mono <- sum(MT.mono$Cost)

## ----combo, echo=TRUE---------------------------------------------------------
# annual probabilities modified by treatment effect
pAB <- RR*nAB/nA
pAC <- RR*nAC/nC
pAD <- RR*nAD/nA
pBC <- RR*nBC/nB
pBD <- RR*nBD/nB
pCD <- RR*nCD/nC
# annual transition probability matrix
Ptc <- matrix(
  c(1.0 - pAB - pAC - pAD,               pAB,         pAC, pAD, 
                      0.0, (1.0 - pBC - pBD),         pBC, pBD,
                      0.0,               0.0, (1.0 - pCD), pCD,
                      0.0,               0.0,         0.0, 1.0),
  nrow = 4L, byrow = TRUE, 
  dimnames=list(
    source = c("A", "B", "C", "D"), target = c("A", "B", "C", "D")
  )
)
# create Markov states for combination therapy
sAc <- MarkovState$new("A", cost = dmca + ccca + cAZT + cLam)
sBc <- MarkovState$new("B", cost = dmcb + cccb + cAZT + cLam)
sCc <- MarkovState$new("C", cost = dmcc + cccc + cAZT + cLam)
sDc <- MarkovState$new("D", cost = 0.0, utility = 0.0)
# create transitions
tAAc <- Transition$new(sAc, sAc)
tABc <- Transition$new(sAc, sBc)
tACc <- Transition$new(sAc, sCc)
tADc <- Transition$new(sAc, sDc)
tBBc <- Transition$new(sBc, sBc)
tBCc <- Transition$new(sBc, sCc)
tBDc <- Transition$new(sBc, sDc)
tCCc <- Transition$new(sCc, sCc)
tCDc <- Transition$new(sCc, sDc)
tDDc <- Transition$new(sDc, sDc)
# construct the model
m.comb <- SemiMarkovModel$new(
  V = list(sAc, sBc, sCc, sDc),
  E = list(tAAc, tABc, tACc, tADc, tBBc, tBCc, tBDc, tCCc, tCDc, tDDc),
  discount.cost = cDR / 100.0,
  discount.utility = oDR / 100.0
)
# set the probabilities
m.comb$set_probabilities(Ptc)

## ----tmcombo, echo=FALSE------------------------------------------------------
TM <- m.comb$transition_probabilities()
pander::pander(TM, emphasize.rownames = FALSE, justify = "lcccc")
rm(TM)

## ----combo_run, echo=TRUE-----------------------------------------------------
# run combination therapy model for 2 years
populations <- c("A" = N, "B" = 0L, "C" = 0L, "D" = 0L)
m.comb$reset(populations)
# run 2 cycles
MT.comb <- m.comb$cycles(2L, hcc.pop = FALSE, hcc.cost = FALSE)
# feed populations into mono model & reset cycle counter and time
populations <- m.comb$get_populations()
m.mono$reset(
  populations, 
  icycle = 2L, 
  elapsed = as.difftime(365.25*2.0, units = "days")
)
# and run model for next 18 years
MT.comb <- rbind(
  MT.comb, m.mono$cycles(ncycles = 18L, hcc.pop = FALSE, hcc.cost = FALSE)
)

## ----echo=F-------------------------------------------------------------------
keep <- c("Years", "A", "B", "C", "D", "Cost", "QALY")
pander::pander(MT.comb[, keep], row.names = FALSE, justify = "rrrrrrr", 
               round = c(2L, 0L, 0L, 0L, 0L, 0L, 3L))

## ----ICER, echo=FALSE---------------------------------------------------------
el.comb <- sum(MT.comb$QALY)
cost.comb <- sum(MT.comb$Cost)
icer <- (cost.comb - cost.mono) / (el.comb - el.mono)

