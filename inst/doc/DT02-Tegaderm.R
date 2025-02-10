## -----------------------------------------------------------------------------
library(rdecision)

## -----------------------------------------------------------------------------
# baseline risk
r.CRBSI <- GammaModVar$new(
  "Baseline CRBSI rate",  "/1000 catheter days",
  shape = (1.48 ^ 2L) / (0.12 ^ 2L),
  scale = (0.12 ^ 2L) / 1.48
)
r.LSI <- GammaModVar$new(
  "Baseline LSI rate", "/1000 catheter days",
  shape = (0.14 ^ 2L) / (0.5 ^ 2L),
  scale = (0.5 ^ 2L) / 0.14
)
r.Dermatitis <- BetaModVar$new(
  "Baseline dermatitis risk", "/catheter", alpha = 1L, beta = 475L
)
# relative effectiveness
hr.CRBSI <- LogNormModVar$new(
  "Tegaderm CRBSI HR", "HR",
  p1 = 0.402, p2 = (0.868 - 0.186) / (2L * 1.96), param = "LN7"
)
hr.LSI <- LogNormModVar$new(
  "Tegaderm LSI HR", "HR",
  p1 = 0.402, p2 = (0.868 - 0.186) / (2L * 1.96), param = "LN7"
)
rr.Dermatitis <- LogNormModVar$new(
  "Tegaderm Dermatitis RR", "RR", p1 = 1.0, p2 = 0.5, param = "LN7"
)
# cost variables
c.CRBSI <- GammaModVar$new(
  "CRBSI cost", "GBP",
  shape = (9900.0 ^ 2L) / (3000.0 ^ 2L),
  scale = (3000.0 ^ 2L) / 9900.0
)
c.LSI <- GammaModVar$new(
  "LSI cost", "GBP",
  shape = (100.0 ^ 2L) / (30.0 ^ 2L),
  scale = (30.0 ^ 2L) / 100.0
)
c.Dermatitis <- GammaModVar$new(
  "Dermatitis cost", "GBP",
  shape = (6.0 ^ 2L) / (3.0 ^ 2L),
  scale = (3.0 ^ 2L) / 6.0
)
# number of dressings and days with catheter
n.dressings <- GammaModVar$new(
  "No. dressings", "dressings",
  shape = (3.0 ^ 2L) / (2.0 ^ 2L),
  scale = (2.0 ^ 2L) / 3.0
)
n.cathdays <- GammaModVar$new(
  "No. days with catheter", "days",
  shape = (10.0 ^ 2L) / (5.0 ^ 2L),
  scale = (5.0 ^ 2L) / 10.0
)

## -----------------------------------------------------------------------------
p.CRBSI.S <- ExprModVar$new(
  "P(CRBSI | standard dressing)", "P",
  rlang::quo(r.CRBSI * n.cathdays / 1000.0)
)
p.CRBSI.T <- ExprModVar$new(
  "P(CRBSI|Tegaderm)", "P",
  rlang::quo(p.CRBSI.S * hr.CRBSI)
)
p.LSI.S <- ExprModVar$new(
  "P(LSI | Standard)", "/patient",
  rlang::quo(r.LSI * n.cathdays / 1000.0)
)
p.LSI.T <- ExprModVar$new(
  "P(LSI | Tegaderm)", "P", rlang::quo(p.LSI.S * hr.LSI)
)
p.Dermatitis.S <- ExprModVar$new(
  "P(dermatitis | standard dressing)", "P",
  rlang::quo(r.Dermatitis)
)
p.Dermatitis.T <- ExprModVar$new(
  "P(dermatitis | Tegaderm)", "P",
  rlang::quo(p.Dermatitis.S * rr.Dermatitis)
)
c.Tegaderm <- ExprModVar$new(
  "Tegaderm CHG cost", "GBP", rlang::quo(6.26 * n.dressings)
)
c.Standard <- ExprModVar$new(
  "Standard dressing cost", "GBP", rlang::quo(1.54 * n.dressings)
)

## -----------------------------------------------------------------------------
# create decision tree
th <- as.difftime(7L, units = "days")
# standard dressing
t01 <- LeafNode$new("t01", interval = th)
t02 <- LeafNode$new("t02", interval = th)
c01 <- ChanceNode$new()
e01 <- Reaction$new(
  c01, t01, p = p.Dermatitis.S, cost = c.Dermatitis, label = "Dermatitis"
)
e02 <- Reaction$new(
  c01, t02, p = NA_real_, cost = 0.0, label = "No dermatitis"
)
t03 <- LeafNode$new("t03", interval = th)
t04 <- LeafNode$new("t04", interval = th)
c02 <- ChanceNode$new()
e03 <- Reaction$new(
  c02, t03, p = p.Dermatitis.S, cost = c.Dermatitis, label = "Dermatitis"
)
e04 <- Reaction$new(
  c02, t04, p = NA_real_, cost = 0.0, label = "No dermatitis"
)
c03 <- ChanceNode$new()
e05 <- Reaction$new(c03, c01, p = p.LSI.S, cost = c.LSI, label = "LSI")
e06 <- Reaction$new(c03, c02, p = NA_real_, cost = 0.0, label = "No LSI")
t11 <- LeafNode$new("t11", interval = th)
t12 <- LeafNode$new("t12", interval = th)
c11 <- ChanceNode$new()
e11 <- Reaction$new(
  c11, t11, p = p.Dermatitis.S, cost = c.Dermatitis, label = "Dermatitis"
)
e12 <- Reaction$new(
  c11, t12, p = NA_real_, cost = 0.0, label = "No Dermatitis"
)
t13 <- LeafNode$new("t13", interval = th)
t14 <- LeafNode$new("t14", interval = th)
c12 <- ChanceNode$new()
e13 <- Reaction$new(
  c12, t13, p = p.Dermatitis.S, cost = c.Dermatitis, label = "Dermatitis"
)
e14 <- Reaction$new(
  c12, t14, p = NA_real_, cost = 0.0, label = "No dermatitis"
)
c13 <- ChanceNode$new()
e15 <- Reaction$new(c13, c11, p = p.LSI.S, cost = c.LSI, label = "LSI")
e16 <- Reaction$new(c13, c12, p = NA_real_, cost = 0.0, label = "No LSI")
c23 <- ChanceNode$new()
e21 <- Reaction$new(c23, c03, p = p.CRBSI.S, cost = c.CRBSI, label = "CRBSI")
e22 <- Reaction$new(c23, c13, p = NA_real_, cost = 0.0, label = "No CRBSI")

# Tegaderm branch
t31 <- LeafNode$new("t31", interval = th)
t32 <- LeafNode$new("t32", interval = th)
c31 <- ChanceNode$new()
e31 <- Reaction$new(
  c31, t31, p = p.Dermatitis.T, cost = c.Dermatitis, label = "Dermatitis"
)
e32 <- Reaction$new(
  c31, t32, p = NA_real_, cost = 0.0, label = "no dermatitis"
)
t33 <- LeafNode$new("t33", interval = th)
t34 <- LeafNode$new("t34", interval = th)
c32 <- ChanceNode$new()
e33 <- Reaction$new(
  c32, t33, p = p.Dermatitis.T, cost = c.Dermatitis, label = "Dermatitis"
)
e34 <- Reaction$new(
  c32, t34, p = NA_real_, cost = 0.0, label = "No dermatitis"
)
c33 <- ChanceNode$new()
e35 <- Reaction$new(c33, c31, p = p.LSI.T, cost = c.LSI, label = "LSI")
e36 <- Reaction$new(c33, c32, p = NA_real_, cost = 0.0, label = "No LSI")
t41 <- LeafNode$new("t41", interval = th)
t42 <- LeafNode$new("t42", interval = th)
c41 <- ChanceNode$new()
e41 <- Reaction$new(
  c41, t41, p = p.Dermatitis.T, cost = c.Dermatitis, label = "Dermatitis"
)
e42 <- Reaction$new(
  c41, t42, p = NA_real_, cost = 0.0, label = "No dermatitis"
)
t43 <- LeafNode$new("t43", interval = th)
t44 <- LeafNode$new("t44", interval = th)
c42 <- ChanceNode$new()
e43 <- Reaction$new(
  c42, t43, p = p.Dermatitis.T, cost = c.Dermatitis, label = "Dermatitis"
)
e44 <- Reaction$new(
  c42, t44, p = NA_real_, cost = 0.0, label = "No dermatitis"
)
c43 <- ChanceNode$new()
e45 <- Reaction$new(c43, c41, p = p.LSI.T, cost = c.LSI, label = "LSI")
e46 <- Reaction$new(c43, c42, p = NA_real_, cost = 0.0, label = "No LSI")
c53 <- ChanceNode$new()
e51 <- Reaction$new(c53, c43, p = p.CRBSI.T, cost = c.CRBSI, label = "CRBSI")
e52 <- Reaction$new(c53, c33, p = NA_real_, cost = 0.0, label = "no CRBSI")

# decision node
d1 <- DecisionNode$new("d1")
e9 <- Action$new(d1, c23, label = "Standard", cost = c.Standard)
e10 <- Action$new(d1, c53, label = "Tegaderm", cost = c.Tegaderm)

# create decision tree
V <- list(
  d1,
  c01, c02, c03, c11, c12, c13, c23, c31, c32, c33, c41, c42, c43, c53,
  t01, t02, t03, t04, t11, t12, t13, t14, t31, t32, t33, t34,
  t41, t42, t43, t44
)
E <- list(
  e01, e02, e03, e04, e05, e06, e11, e12, e13, e14, e15, e16, e21, e22,
  e31, e32, e33, e34, e35, e36, e41, e42, e43, e44, e45, e46, e51, e52,
  e9, e10
)
DT <- DecisionTree$new(V, E)

## -----------------------------------------------------------------------------
DT$draw(border = TRUE)

## -----------------------------------------------------------------------------
with(data = DT$modvar_table(), expr = {
  data.frame(
    Description = Description,
    Distribution = Distribution,
    stringsAsFactors = FALSE
  )
})

## -----------------------------------------------------------------------------
with(data = DT$modvar_table(), expr = {
  data.frame(
    Variable = paste(Description, Units, sep = ", "),
    Mean = round(E, digits = 3L),
    LowerCI = round(Q2.5, digits = 3L),
    UpperCI = round(Q97.5, digits = 3L),
    stringsAsFactors = FALSE
  )
})

## -----------------------------------------------------------------------------
RES <- DT$evaluate()

## -----------------------------------------------------------------------------
with(data = RES, expr = {
  data.frame(
    Run = Run,
    d1 = d1,
    Cost = gbp(Cost, p = TRUE, char = FALSE),
    stringsAsFactors = FALSE
  )
})

## -----------------------------------------------------------------------------
to <- DT$tornado(index = e10, ref = e9, draw = TRUE)

## -----------------------------------------------------------------------------
with(data = to, expr = {
  data.frame(
    Variable = paste(Description, Units, sep = ", "),
    LL = round(x = LL, digits = 2L),
    UL = round(x = UL, digits = 2L),
    Min.CostDiff = round(x = outcome.min, digits = 2L),
    Max.CostDiff = round(x = outcome.max, digits = 2L),
    stringsAsFactors = FALSE
  )
})

## -----------------------------------------------------------------------------
N <- 1000L
psa <- DT$evaluate(setvars = "random", by = "run", N = N)
psa[, "Difference"] <- psa[, "Cost.Standard"] - psa[, "Cost.Tegaderm"]

## -----------------------------------------------------------------------------
with(data = head(psa, n = 10L), expr = {
  data.frame(
    Run = Run,
    Cost.Tegaderm = gbp(Cost.Tegaderm, p = TRUE, char = FALSE),
    Cost.Standard = gbp(Cost.Standard, p = TRUE, char = FALSE),
    Cost.Difference = gbp(Difference, p = TRUE, char = FALSE),
    stringsAsFactors = FALSE
  )
})

## -----------------------------------------------------------------------------
rm(psa)

## -----------------------------------------------------------------------------
r.CRBSI <- GammaModVar$new(
  "Baseline CRBSI rate",  "/1000 catheter days",
  shape = (0.30 ^ 2L) / (0.102 ^ 2L),
  scale = (0.102 ^ 2L) / 0.30
)
p.CRBSI.S <- ExprModVar$new(
  "P(CRBSI | standard dressing)", "P",
  rlang::quo(r.CRBSI * n.cathdays / 1000.0)
)
p.CRBSI.T <- ExprModVar$new(
  "P(CRBSI|Tegaderm)", "P",
  rlang::quo(p.CRBSI.S * hr.CRBSI)
)
e21 <- Reaction$new(c23, c03, p = p.CRBSI.S, cost = c.CRBSI, label = "CRBSI")
e22 <- Reaction$new(c23, c13, p = NA_real_, cost = 0.0, label = "No CRBSI")
e51 <- Reaction$new(c53, c43, p = p.CRBSI.T, cost = c.CRBSI, label = "CRBSI")
e52 <- Reaction$new(c53, c33, p = NA_real_, cost = 0.0, label = "no CRBSI")
E <- list(
  e01, e02, e03, e04, e05, e06, e11, e12, e13, e14, e15, e16, e21, e22,
  e31, e32, e33, e34, e35, e36, e41, e42, e43, e44, e45, e46, e51, e52,
  e9, e10
)
DT <- DecisionTree$new(V, E)

## -----------------------------------------------------------------------------
N <- 1000L
psa <- DT$evaluate(setvars = "random", by = "run", N = N)
psa[, "Difference"] <- psa[, "Cost.Standard"] - psa[, "Cost.Tegaderm"]

## -----------------------------------------------------------------------------
hr_threshold <- DT$threshold(
  index = list(e10),
  ref = list(e9),
  outcome = "saving",
  mvd = "Tegaderm CRBSI HR",
  a = 0.1,
  b = 0.9,
  tol = 0.01
)

## -----------------------------------------------------------------------------
c_crbsi_threshold <- DT$threshold(
  index = list(e10),
  ref = list(e9),
  outcome = "saving",
  mvd = "CRBSI cost",
  a = 0.0,
  b = 9900.0,
  tol = 10.0
)

