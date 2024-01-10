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
# Time horizon
th <- as.difftime(24L, units = "hours")

# model variables for cost
c_sumatriptan <- 16.10
c_caffeine <- 1.32
c_ed <- 63.16
c_admission <- 1093.0

# model variables for utility
u_relief_norecurrence <- 1.0
u_relief_recurrence <- 0.9
u_norelief_endures <- -0.30
u_norelief_er <- 0.1

# model variables for effect
p_sumatriptan_recurrence <- 0.594
p_caffeine_recurrence <- 0.703
p_sumatriptan_relief <- 0.558
p_caffeine_relief <- 0.379
p_er <- 0.08
p_admitted <- 0.002

## -----------------------------------------------------------------------------
# Sumatriptan branch
ta <- LeafNode$new("A", utility = u_relief_norecurrence, interval = th)
tb <- LeafNode$new("B", utility = u_relief_recurrence, interval = th)
c3 <- ChanceNode$new()
e1 <- Reaction$new(
  c3, ta, p = p_sumatriptan_recurrence, label = "No recurrence"
)
e2 <- Reaction$new(
  c3, tb, p = 1.0 - p_sumatriptan_recurrence, cost = c_sumatriptan,
  label = "Relieved 2nd dose"
)
td <- LeafNode$new("D", utility = u_norelief_er, interval = th)
te <- LeafNode$new("E", utility = u_norelief_endures, interval = th)
c7 <- ChanceNode$new()
e3 <- Reaction$new(c7, td, p = 1.0 - p_admitted, label = "Relief")
e4 <- Reaction$new(
  c7, te, p = p_admitted, cost = c_admission, label = "Hospitalization"
)

tc <- LeafNode$new("C", utility = u_norelief_endures, interval = th)
c4 <- ChanceNode$new()
e5 <- Reaction$new(c4, tc, p = 1.0 - p_er, label = "Endures attack")
e6 <- Reaction$new(c4, c7, p = p_er, cost = c_ed, label = "ER")

c1 <- ChanceNode$new()
e7 <- Reaction$new(c1, c3, p = p_sumatriptan_relief, label = "Relief")
e8 <- Reaction$new(c1, c4, p = 1.0 - p_sumatriptan_relief, label = "No relief")

# Caffeine/Ergotamine branch
tf <- LeafNode$new("F", utility = u_relief_norecurrence, interval = th)
tg <- LeafNode$new("G", utility = u_relief_recurrence, interval = th)
c5 <- ChanceNode$new()
e9 <- Reaction$new(c5, tf, p = p_caffeine_recurrence, label = "No recurrence")
e10 <- Reaction$new(
  c5, tg, p = 1.0 - p_caffeine_recurrence, cost = c_caffeine,
  label = "Relieved 2nd dose"
)
ti <- LeafNode$new("I", utility = u_norelief_er, interval = th)
tj <- LeafNode$new("J", utility = u_norelief_endures, interval = th)
c8 <- ChanceNode$new()
e11 <- Reaction$new(c8, ti, p = 1.0 - p_admitted, label = "Relief")
e12 <- Reaction$new(
  c8, tj, p = p_admitted, cost = c_admission, label = "Hospitalization"
)

th <- LeafNode$new("H", utility = u_norelief_endures, interval = th)
c6 <- ChanceNode$new()
e13 <- Reaction$new(c6, th, p = 1.0 - p_er, label = "Endures attack")
e14 <- Reaction$new(c6, c8, p = p_er, cost = c_ed, label = "ER")

c2 <- ChanceNode$new()
e15 <- Reaction$new(c2, c5, p = p_caffeine_relief, label = "Relief")
e16 <- Reaction$new(c2, c6, p = 1.0 - p_caffeine_relief, label = "No relief")

# decision node
d1 <- DecisionNode$new("d1")
e17 <- Action$new(d1, c1, cost = c_sumatriptan, label = "Sumatriptan")
e18 <- Action$new(d1, c2, cost = c_caffeine, label = "Caffeine-Ergotamine")

# create lists of nodes and edges
V <- list(
  d1, c1, c2, c3, c4, c5, c6, c7, c8,
  ta, tb, tc, td, te, tf, tg, th, ti, tj
)
E <- list(
  e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16,
  e17, e18
)

# tree
DT <- DecisionTree$new(V, E)

## -----------------------------------------------------------------------------
DT$draw(border = TRUE)

## -----------------------------------------------------------------------------
ep <- DT$evaluate(by = "path")

## -----------------------------------------------------------------------------
knitr::kable(
  ep[, c("Leaf", "Probability", "Cost", "Utility")],
  align = "lrrr",
  digits = c(2L, 4L, 2L, 5L),
  format.args = list(scientific = FALSE)
)

## -----------------------------------------------------------------------------
es <- DT$evaluate()

## -----------------------------------------------------------------------------
knitr::kable(
  es[, c("d1", "Cost", "Utility", "QALY")],
  align = "lrrr",
  digits = c(2L, 2L, 4L, 4L),
  format.args = list(scientific = FALSE)
)

## -----------------------------------------------------------------------------
is <- which(es[, "d1"] == "Sumatriptan")
cost_s <- es[[is, "Cost"]]
utility_s <- es[[is, "Utility"]]
qaly_s <- es[[is, "QALY"]]

ic <- which(es[, "d1"] == "Caffeine-Ergotamine")
cost_c <- es[[ic, "Cost"]]
utility_c <- es[[ic, "Utility"]]
qaly_c <- es[[ic, "QALY"]]

delta_c <- cost_s - cost_c
delta_u <- utility_s - utility_c
delta_q <- qaly_s - qaly_c
icer <- delta_c / delta_q

## -----------------------------------------------------------------------------
p_sumatriptan_relief <- p_caffeine_relief + 0.268
e7$set_probability(p_sumatriptan_relief)
e8$set_probability(1.0 - p_sumatriptan_relief)
es <- DT$evaluate()

## -----------------------------------------------------------------------------
is <- which(es[, "d1"] == "Sumatriptan")
cost_s_upper <- es[[is, "Cost"]]
utility_s_upper <- es[[is, "Utility"]]
qaly_s_upper <- es[[is, "QALY"]]

ic <- which(es[, "d1"] == "Caffeine-Ergotamine")
cost_c_upper <- es[[ic, "Cost"]]
utility_c_upper <- es[[ic, "Utility"]]
qaly_c_upper <- es[[ic, "QALY"]]

delta_c_upper <- cost_s_upper - cost_c_upper
delta_u_upper <- utility_s_upper - utility_c_upper
delta_q_upper <- qaly_s_upper - qaly_c_upper
icer_upper <- delta_c_upper / delta_q_upper

## -----------------------------------------------------------------------------
knitr::kable(
  es[, c("d1", "Cost", "Utility", "QALY")],
  align = "lrrr",
  digits = c(2L, 2L, 4L, 4L),
  format.args = list(scientific = FALSE)
)

## -----------------------------------------------------------------------------
p_sumatriptan_relief <- p_caffeine_relief + 0.091
e7$set_probability(p_sumatriptan_relief)
e8$set_probability(1.0 - p_sumatriptan_relief)
es <- DT$evaluate()

## -----------------------------------------------------------------------------
is <- which(es[, "d1"] == "Sumatriptan")
cost_s_lower <- es[[is, "Cost"]]
utility_s_lower <- es[[is, "Utility"]]
qaly_s_lower <- es[[is, "QALY"]]

ic <- which(es[, "d1"] == "Caffeine-Ergotamine")
cost_c_lower <- es[[ic, "Cost"]]
utility_c_lower <- es[[ic, "Utility"]]
qaly_c_lower <- es[[ic, "QALY"]]

delta_c_lower <- cost_s_lower - cost_c_lower
delta_u_lower <- utility_s_lower - utility_c_lower
delta_q_lower <- qaly_s_lower - qaly_c_lower
icer_lower <- delta_c_lower / delta_q_lower

## -----------------------------------------------------------------------------
knitr::kable(
  es[, c("d1", "Cost", "Utility", "QALY")],
  align = "lrrr",
  digits = c(2L, 2L, 4L, 4L),
  format.args = list(scientific = FALSE)
)

