## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE, echo=FALSE-----------------------------------------
library("rdecision")

## ----variables, echo=T--------------------------------------------------------
# clinical variables
r.CRBSI <- NormModVar$new(
  "Baseline CRBSI rate", "/1000 catheter days", mu = 1.48, sigma = 0.074
)
hr.CRBSI <- LogNormModVar$new(
  "Tegaderm CRBSI HR", "HR", p1 = -0.911, p2 = 0.393
)
hr.LSI <- LogNormModVar$new(
  "Tegaderm LSI HR", "HR", p1 = -0.911, p2 = 0.393
)
r.Dermatitis <- NormModVar$new(
  "Baseline dermatitis risk", "/catheter", mu = 0.0026, sigma = 0.00026
)
rr.Dermatitis <- LogNormModVar$new(
  "Tegaderm Dermatitis RR", "RR", p1 = 1.482, p2 = 0.490
)
# cost variables
c.CRBSI <- GammaModVar$new(
  "CRBSI cost", "GBP", shape = 198.0, scale = 50.0
)
c.Dermatitis <- GammaModVar$new(
  "Dermatitis cost", "GBP", shape = 30.0, scale = 5.0
)
c.LSI <- GammaModVar$new(
  "LSI cost", "GBP", shape = 50.0, scale = 5.0
)
n.catheters <- NormModVar$new(
  "No. catheters", "catheters", mu = 3.0, sigma = 0.3 
)
c.Tegaderm <- ExprModVar$new(
  "Tegaderm CHG cost", "GBP", rlang::quo(6.21 * n.catheters)
)
c.Standard <- ExprModVar$new(
  "Standard dressing cost", "GBP", rlang::quo(1.34*n.catheters)
)
n.cathdays <- NormModVar$new(
  "No. days with catheter", "days", mu = 10.0, sigma = 2.0
)  

## ----expressions, echo=TRUE---------------------------------------------------
# probabilities
p.Dermatitis.S <- ExprModVar$new(
  "P(dermatitis | standard dressing)", "P", 
  rlang::quo(r.Dermatitis * n.catheters)
)
q.Dermatitis.S <- ExprModVar$new(
  "Q(dermatitis | standard dressing)", "1 - P", 
  rlang::quo(1.0 - p.Dermatitis.S)
)
p.Dermatitis.T <- ExprModVar$new(
  "P(dermatitis | Tegaderm)", "P", 
  rlang::quo(r.Dermatitis * rr.Dermatitis * n.catheters)
)
q.Dermatitis.T <- ExprModVar$new(
  "Q(dermatitis | Tegaderm)", "1 - P", 
  rlang::quo(1.0 - p.Dermatitis.T)
)

p.LSI.S <- NormModVar$new(
  "P(LSI | Standard)", "/patient", mu = 0.1, sigma = 0.01 
)
q.LSI.S <- ExprModVar$new(
  "Q(LSI | Standard)", "1 - P", rlang::quo(1.0 - p.LSI.S) 
)
p.LSI.T <- ExprModVar$new(
  "P(LSI | Tegaderm)", "P", rlang::quo(1.0 - (1.0 - p.LSI.S) ^ hr.LSI)
)
q.LSI.T <- ExprModVar$new(
  "Q(LSI | Tegaderm)", "1 - P", rlang::quo(1.0 - p.LSI.T)
)

p.CRBSI.S <- ExprModVar$new(
  "P(CRBSI | standard dressing)", "P",  
  rlang::quo(r.CRBSI * n.cathdays / 1000.0)
)
q.CRBSI.S <- ExprModVar$new(
  "Q(CRBSI | standard dressing)", "1 - P",  rlang::quo(1.0 - p.CRBSI.S)
)
p.CRBSI.T <- ExprModVar$new(
  "P(CRBSI|Tegaderm)", "P", 
  rlang::quo(1.0 - (1.0 - r.CRBSI * n.cathdays / 1000.0) ^ hr.CRBSI)
)
q.CRBSI.T <- ExprModVar$new(
  "Q(CRBSI | Tegaderm)", "1 - P", rlang::quo(1.0 - p.CRBSI.T)
)

## ----tree, echo=T-------------------------------------------------------------
# create decision tree
th <- as.difftime(7L, units = "days")
# standard dressing
t01 <- LeafNode$new("t01", interval = th)
t02 <- LeafNode$new("t02", interval = th)
c01 <- ChanceNode$new()
e01 <- Reaction$new(c01, t01, p = p.Dermatitis.S, cost = c.Dermatitis,
                    label = "Dermatitis")
e02 <- Reaction$new(c01, t02, p = q.Dermatitis.S, cost = 0.0,
                    label = "No dermatitis")
#
t03 <- LeafNode$new("t03", interval = th)
t04 <- LeafNode$new("t04", interval = th)
c02 <- ChanceNode$new()
e03 <- Reaction$new(c02, t03, p = p.Dermatitis.S, cost = c.Dermatitis,
                    label = "Dermatitis")
e04 <- Reaction$new(c02, t04, p = q.Dermatitis.S, cost = 0.0,
                    label = "No dermatitis")
#
c03 <- ChanceNode$new()
e05 <- Reaction$new(c03, c01, p = p.LSI.S, cost = c.LSI, label = "LSI")
e06 <- Reaction$new(c03, c02, p = q.LSI.S, cost = 0.0, label = "No LSI")
#
t11 <- LeafNode$new("t11", interval = th)
t12 <- LeafNode$new("t12", interval = th)
c11 <- ChanceNode$new()
e11 <- Reaction$new(c11, t11, p = p.Dermatitis.S, cost = c.Dermatitis,
                    label = "Dermatitis")
e12 <- Reaction$new(c11, t12, p = q.Dermatitis.S, cost = 0.0, 
                    label = "No Dermatitis")
#
t13 <- LeafNode$new("t13", interval = th)
t14 <- LeafNode$new("t14", interval = th)
c12 <- ChanceNode$new()
e13 <- Reaction$new(c12, t13, p = p.Dermatitis.S, cost = c.Dermatitis, 
                    label = "Dermatitis")
e14 <- Reaction$new(c12, t14, p = q.Dermatitis.S, cost = 0.0, 
                    label = "No dermatitis")
#
c13 <- ChanceNode$new()
e15 <- Reaction$new(c13, c11, p = p.LSI.S, cost = c.LSI, label = "LSI")
e16 <- Reaction$new(c13, c12, p = q.LSI.S, cost = 0.0, label = "No LSI")
#
c23 <- ChanceNode$new()
e21 <- Reaction$new(c23, c03, p = p.CRBSI.S, cost = c.CRBSI, label = "CRBSI")
e22 <- Reaction$new(c23, c13, p = q.CRBSI.S, cost = 0.0, label = "No CRBSI")
#
# Tegaderm branch  
t31 <- LeafNode$new("t31", interval = th)
t32 <- LeafNode$new("t32", interval = th)
c31 <- ChanceNode$new()
e31 <- Reaction$new(c31, t31, p = p.Dermatitis.T, cost = c.Dermatitis,
                    label = "Dermatitis")
e32 <- Reaction$new(c31, t32, p = q.Dermatitis.T, cost = 0.0, 
                    label = "no dermatitis")
#
t33 <- LeafNode$new("t33", interval = th)
t34 <- LeafNode$new("t34", interval = th)
c32 <- ChanceNode$new()
e33 <- Reaction$new(c32, t33, p = p.Dermatitis.T, cost = c.Dermatitis,
                    label = "Dermatitis")
e34 <- Reaction$new(c32, t34, p = q.Dermatitis.T, cost = 0.0, 
                    label = "No dermatitis")
#
c33 <- ChanceNode$new()
e35 <- Reaction$new(c33, c31, p = p.LSI.T, cost = c.LSI, label = "LSI")
e36 <- Reaction$new(c33, c32, p = q.LSI.T, cost = 0.0, label = "No LSI")
#
t41 <- LeafNode$new("t41", interval=th)
t42 <- LeafNode$new("t42", interval=th)
c41 <- ChanceNode$new()
e41 <- Reaction$new(c41, t41, p = p.Dermatitis.T, cost = c.Dermatitis,
                    label = "Dermatitis")
e42 <- Reaction$new(c41, t42, p = q.Dermatitis.T, cost = 0.0, 
                    label = "No dermatitis")
#
t43 <- LeafNode$new("t43", interval = th)
t44 <- LeafNode$new("t44", interval = th)
c42 <- ChanceNode$new()
e43 <- Reaction$new(c42, t43, p = p.Dermatitis.T, cost = c.Dermatitis,
                    label = "Dermatitis")
e44 <- Reaction$new(c42, t44, p = q.Dermatitis.T, cost = 0.0, 
                    label = "No dermatitis")
#
c43 <- ChanceNode$new()
e45 <- Reaction$new(c43, c41, p = p.LSI.T, cost = c.LSI, label = "LSI")
e46 <- Reaction$new(c43, c42, p = q.LSI.T, cost = 0.0, label = "No LSI")
#
c53 <- ChanceNode$new()
e51 <- Reaction$new(c53, c43, p = p.CRBSI.T, cost = c.CRBSI, label = "CRBSI")
e52 <- Reaction$new(c53, c33, p = q.CRBSI.T, cost = 0.0, label = "no CRBSI")
#  
# decision node
d1 <- DecisionNode$new("d1")
e9 <- Action$new(d1, c23, label = "Standard", cost = c.Standard)
e10 <- Action$new(d1, c53, label = "Tegaderm", cost = c.Tegaderm)
#
# create decision tree
V <- list(d1, 
          c01, c02, c03, c11, c12, c13, c23, c31, c32, c33, c41, c42, c43, c53,
          t01, t02, t03, t04, t11, t12, t13, t14, t31, t32, t33, t34, 
          t41, t42, t43, t44)
E <- list(e01, e02, e03, e04, e05, e06, e11, e12, e13, e14, e15, e16, e21, e22,
          e31, e32, e33, e34, e35, e36, e41, e42, e43, e44, e45, e46, e51, e52,
          e9,e10)
DT <- DecisionTree$new(V, E)

## ----fig1.caption, echo=FALSE-------------------------------------------------
f1c <- "Decision tree for the Tegaderm model"

## ----draw, echo=FALSE, results="hide", fig.keep="all", fig.align="center", fig.cap=f1c----
DT$draw(border = TRUE)

## ----modelinputs-structure, echo=FALSE----------------------------------------
local({
  DF <- DT$modvar_table(expressions = FALSE)
  keep <- c("Description", "Distribution")
  pander::pander(DF[, keep], row.names = FALSE, digits = 3L, justify = "left")
})

## ----modelinputs-pe, echo=FALSE-----------------------------------------------
local({
  DF <- DT$modvar_table(expressions = FALSE)
  DF$Variable <- paste(DF$Description, DF$Units, sep =", ")
  keep <- c("Variable", "Mean", "Q2.5", "Q97.5")
  pander::pander(DF[, keep], row.names = FALSE, digits = 3L, justify = "lrrr")
})

## ----basecase, echo=FALSE-----------------------------------------------------
RES <- DT$evaluate()

## ----basecase_table, echo=FALSE-----------------------------------------------
local({
  keep <- c("Run", "d1", "Cost")
  pander::pander(RES[, keep], round = 2L, justify = "llr")
})

## ----fig2.caption, echo=FALSE-------------------------------------------------
f2c <- "Tornado diagram for the Tegaderm model"

## ----tornado, echo=FALSE, results="hide", fig.keep="all", fig.align="center", fig.cap=f2c----
DT$tornado(
  index = list(e10), ref = list(e9), draw = TRUE
)

## ----PSA, echo=TRUE-----------------------------------------------------------
N <- 1000L
PSA <- DT$evaluate(setvars = "random", by = "run", N = N)

## ----PSA_table, echo=FALSE----------------------------------------------------
PSA$Difference <- PSA$Cost.Tegaderm - PSA$Cost.Standard
keep<- c("Run", "Cost.Tegaderm", "Cost.Standard", "Difference")
pander::pander(head(PSA[, keep], n = 10L), round = 2L, row.names = FALSE, 
               justify = "lrrr")

## ----echo=F-------------------------------------------------------------------
rm(PSA)

