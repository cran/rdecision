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
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = FALSE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library("rdecision")

## -----------------------------------------------------------------------------
cost_diet <- 50.0
cost_exercise <- 750.0
cost_stent <- 5000.0

## -----------------------------------------------------------------------------
decision_node <- DecisionNode$new("Programme")

## -----------------------------------------------------------------------------
chance_node_diet <- ChanceNode$new("Outcome")
chance_node_exercise <- ChanceNode$new("Outcome")

## -----------------------------------------------------------------------------
leaf_node_diet_no_stent <- LeafNode$new("No intervention")
leaf_node_diet_stent <- LeafNode$new("Intervention")
leaf_node_exercise_no_stent <- LeafNode$new("No intervention")
leaf_node_exercise_stent <- LeafNode$new("Intervention")

## -----------------------------------------------------------------------------
action_diet <- Action$new(
  decision_node, chance_node_diet, cost = cost_diet, label = "Diet"
)
action_exercise <- Action$new(
  decision_node, chance_node_exercise, cost = cost_exercise, label = "Exercise"
)

## -----------------------------------------------------------------------------
s_diet <- 12L
f_diet <- 56L
s_exercise <- 18L
f_exercise <- 40L

## -----------------------------------------------------------------------------
ip_diet <- f_diet / (s_diet + f_diet)
ip_exercise <- f_exercise / (s_exercise + f_exercise)
nnt <- 1.0 / (ip_diet - ip_exercise)

## -----------------------------------------------------------------------------
p_diet <- 1.0 - ip_diet
p_exercise <- 1.0 - ip_exercise
q_diet <- 1.0 - p_diet
q_exercise <- 1.0 - p_exercise

## -----------------------------------------------------------------------------
reaction_diet_success <- Reaction$new(
  chance_node_diet, leaf_node_diet_no_stent,
  p = p_diet, cost = 0.0, label = "Success"
)

reaction_diet_failure <- Reaction$new(
  chance_node_diet, leaf_node_diet_stent,
  p = q_diet, cost = cost_stent, label = "Failure"
)

reaction_exercise_success <- Reaction$new(
  chance_node_exercise, leaf_node_exercise_no_stent,
  p = p_exercise, cost = 0.0, label = "Success"
)

reaction_exercise_failure <- Reaction$new(
  chance_node_exercise, leaf_node_exercise_stent,
  p = q_exercise, cost = cost_stent, label = "Failure"
)

## -----------------------------------------------------------------------------
dt <- DecisionTree$new(
  V = list(
    decision_node,
    chance_node_diet,
    chance_node_exercise,
    leaf_node_diet_no_stent,
    leaf_node_diet_stent,
    leaf_node_exercise_no_stent,
    leaf_node_exercise_stent
  ),
  E = list(
    action_diet,
    action_exercise,
    reaction_diet_success,
    reaction_diet_failure,
    reaction_exercise_success,
    reaction_exercise_failure
  )
)

## -----------------------------------------------------------------------------
dt$draw()

## -----------------------------------------------------------------------------
rs <- dt$evaluate()

## -----------------------------------------------------------------------------
knitr::kable(rs, digits = 2L)

## -----------------------------------------------------------------------------
o_netc_diet <- rs[[which(rs[, "Programme"] == "Diet"), "Cost"]]
e_netc_diet <- cost_diet + q_diet * cost_stent
o_netc_exercise <- rs[[which(rs[, "Programme"] == "Exercise"), "Cost"]]
e_netc_exercise <- cost_exercise + q_exercise * cost_stent
incc <- nnt * (cost_exercise - cost_diet)
o_deltac <- o_netc_exercise - o_netc_diet
e_deltac <- (incc - cost_stent) / nnt
e_cost_threshold <- (cost_stent / nnt) + cost_diet
nnt_threshold <- cost_stent / (cost_exercise - cost_diet)
e_success_threshold <- 1.0 - (ip_diet - (1.0 / nnt_threshold))

## -----------------------------------------------------------------------------
rp <- dt$evaluate(by = "path")

## -----------------------------------------------------------------------------
knitr::kable(rp, digits = c(1L, NA, NA, 3L, 2L, 2L, 3L, 3L))

## -----------------------------------------------------------------------------
du_stent <- 0.05
leaf_node_diet_stent$set_utility(1.0 - du_stent)
leaf_node_exercise_stent$set_utility(1.0 - du_stent)
rs <- dt$evaluate()

## -----------------------------------------------------------------------------
knitr::kable(rs, digits = c(1L, NA, 1L, 2L, 2L, 3L, 3L))

## -----------------------------------------------------------------------------
delta_c <- rs[[which(rs[, "Programme"] == "Exercise"), "Cost"]] -
  rs[[which(rs[, "Programme"] == "Diet"), "Cost"]]
delta_u <- rs[[which(rs[, "Programme"] == "Exercise"), "Utility"]] -
  rs[[which(rs[, "Programme"] == "Diet"), "Utility"]]
icer <- delta_c / delta_u

## -----------------------------------------------------------------------------
e_du <- du_stent * (p_exercise - p_diet)
e_icer <- (e_netc_exercise - e_netc_diet) / e_du

## -----------------------------------------------------------------------------
cost_diet <- ConstModVar$new("Cost of diet programme", "GBP", 50.0)
cost_exercise <- ConstModVar$new("Cost of exercise programme", "GBP", 750.0)
cost_stent <- ConstModVar$new("Cost of stent intervention", "GBP", 5000.0)

## -----------------------------------------------------------------------------
p_diet <- BetaModVar$new(
  alpha = s_diet, beta = f_diet, description = "P(diet)", units = ""
)
p_exercise <- BetaModVar$new(
  alpha = s_exercise, beta = f_exercise, description = "P(exercise)", units = ""
)

q_diet <- ExprModVar$new(
  rlang::quo(1.0 - p_diet), description = "1 - P(diet)", units = ""
)
q_exercise <- ExprModVar$new(
  rlang::quo(1.0 - p_exercise), description = "1 - P(exercise)", units = ""
)

## -----------------------------------------------------------------------------
action_diet$set_cost(cost_diet)
action_exercise$set_cost(cost_exercise)

## -----------------------------------------------------------------------------
reaction_diet_success$set_probability(p_diet)

reaction_diet_failure$set_probability(q_diet)
reaction_diet_failure$set_cost(cost_stent)

reaction_exercise_success$set_probability(p_exercise)

reaction_exercise_failure$set_probability(q_exercise)
reaction_exercise_failure$set_cost(cost_stent)

## -----------------------------------------------------------------------------
knitr::kable(dt$modvar_table(), digits = 3L)

## -----------------------------------------------------------------------------
rs <- dt$evaluate()

## -----------------------------------------------------------------------------
knitr::kable(rs, digits = 2L)

## -----------------------------------------------------------------------------
rs_025 <- dt$evaluate(setvars = "q2.5")
rs_975 <- dt$evaluate(setvars = "q97.5")

## -----------------------------------------------------------------------------
N <- 1000L
rs <- dt$evaluate(setvars = "random", by = "run", N = N)

## -----------------------------------------------------------------------------
knitr::kable(summary(rs[, c(3L, 8L)]))

## -----------------------------------------------------------------------------
rs[, "Difference"] <- rs[, "Cost.Diet"] - rs[, "Cost.Exercise"]
CI <- quantile(rs[, "Difference"], c(0.025, 0.975))

## -----------------------------------------------------------------------------
cost_threshold <- dt$threshold(
  index = list(action_exercise),
  ref = list(action_diet),
  outcome = "saving",
  mvd = cost_exercise$description(),
  a = 0.0, b = 5000.0, tol = 0.1
)

success_threshold <- dt$threshold(
  index = list(action_exercise),
  ref = list(action_diet),
  outcome = "saving",
  mvd = p_exercise$description(),
  a = 0.0, b = 1.0, tol = 0.001
)

