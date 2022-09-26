## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library("rdecision")

## ----decision-node------------------------------------------------------------
decision_node <- DecisionNode$new("Programme")

## ----chance-nodes-------------------------------------------------------------
chance_node_diet <- ChanceNode$new("Outcome")
chance_node_exercise <- ChanceNode$new("Outcome")

## ----leaf-nodes---------------------------------------------------------------
leaf_node_diet_no_stent <- LeafNode$new("No intervention")
leaf_node_diet_stent <- LeafNode$new("Intervention")
leaf_node_exercise_no_stent <- LeafNode$new("No intervention")
leaf_node_exercise_stent <- LeafNode$new("Intervention")

## ----actions------------------------------------------------------------------
action_diet <- Action$new(
  decision_node, chance_node_diet, cost = 50.0, label = "Diet"
)
action_exercise <- Action$new(
  decision_node, chance_node_exercise, cost = 750.0, label = "Exercise"
)

## ----reactions----------------------------------------------------------------
p.diet <- 12L/68L
p.exercise <- 18L/58L

reaction_diet_success <- Reaction$new(
  chance_node_diet, leaf_node_diet_no_stent, 
  p = p.diet, cost = 0.0, label = "Success")

reaction_diet_failure <- Reaction$new(
  chance_node_diet, leaf_node_diet_stent, 
  p = 1.0 - p.diet, cost = 5000.0, label = "Failure")

reaction_exercise_success <- Reaction$new(
  chance_node_exercise, leaf_node_exercise_no_stent, 
  p = p.exercise, cost = 0.0, label = "Success")

reaction_exercise_failure <- Reaction$new(
  chance_node_exercise, leaf_node_exercise_stent, 
  p = 1.0 - p.exercise, cost = 5000.0, label = "Failure")

## ----decision-tree------------------------------------------------------------
DT <- DecisionTree$new(
  V = list(decision_node, 
           chance_node_diet, 
           chance_node_exercise, 
           leaf_node_diet_no_stent, 
           leaf_node_diet_stent, 
           leaf_node_exercise_no_stent, 
           leaf_node_exercise_stent),
  E = list(action_diet,
           action_exercise,
           reaction_diet_success,
           reaction_diet_failure,
           reaction_exercise_success,
           reaction_exercise_failure)
)

## ----decision-tree-draw-------------------------------------------------------
DT$draw()

## ----evaluate-----------------------------------------------------------------
DT_evaluation <- DT$evaluate()
knitr::kable(DT_evaluation, digits = 2L)

## ----evaluate-by-path---------------------------------------------------------
knitr::kable(DT$evaluate(by = "path"), digits=c(NA, NA, 3L, 2L, 3L, 3L, 3L, 1L))

## ----lower-utility------------------------------------------------------------
leaf_node_diet_stent_u <- LeafNode$new("Intervention", utility = 0.75)
leaf_node_exercise_stent_u <- LeafNode$new("Intervention", utility = 0.75)

reaction_diet_failure_u <- Reaction$new(
  chance_node_diet, leaf_node_diet_stent_u, 
  p = 1.0 - p.diet, cost = 5000.0, label = "Failure")

reaction_exercise_failure_u <- Reaction$new(
  chance_node_exercise, leaf_node_exercise_stent_u, 
  p = 1.0 - p.exercise, cost = 5000.0, label = "Failure")

DT_u <- DecisionTree$new(
  V = list(decision_node, 
           chance_node_diet, 
           chance_node_exercise, 
           leaf_node_diet_no_stent, 
           leaf_node_diet_stent_u, 
           leaf_node_exercise_no_stent, 
           leaf_node_exercise_stent_u),
  E = list(action_diet,
           action_exercise,
           reaction_diet_success,
           reaction_diet_failure_u,
           reaction_exercise_success,
           reaction_exercise_failure_u)
)

DT_u_evaluation <- DT_u$evaluate()
knitr::kable(DT_u_evaluation, digits = 2L)

## ----icer---------------------------------------------------------------------
ICER <- diff(DT_u_evaluation$Cost)/diff(DT_u_evaluation$Utility)

## ----beta-mod-vars------------------------------------------------------------
# Diet: 12 successes / 68 total
p.diet_beta <- BetaModVar$new(
  alpha = 12L, beta = 68L - 12L, description = "P(diet)", units = ""
)
# Exercise: 18 successes / 58 total
p.exercise_beta <- BetaModVar$new(
  alpha = 18L, beta = 58L - 18L, description = "P(exercise)", units = ""
)

## ----expr-mod-vars------------------------------------------------------------
q.diet_beta <- ExprModVar$new(
  rlang::quo(1.0 - p.diet_beta), description = "1 - P(diet)", units = ""
)
q.exercise_beta <- ExprModVar$new(
  rlang::quo(1.0 - p.exercise_beta), description = "1 - P(exercise)", units = ""
)

## ----const-vars---------------------------------------------------------------
cost_diet <- ConstModVar$new("Cost of diet programme", "GBP", 50.0)
cost_exercise <- ConstModVar$new("Cost of exercise programme", "GBP", 750.0)
cost_stent <- ConstModVar$new("Cost of stent intervention", "GBP", 5000.0)

## ----DT-probabilistic---------------------------------------------------------

action_diet_prob <- Action$new(
  decision_node, chance_node_diet,
  cost = cost_diet, label = "Diet")

action_exercise_prob <- Action$new(
  decision_node, chance_node_exercise, 
  cost = cost_exercise, label = "Exercise")

reaction_diet_success_prob <- Reaction$new(
  chance_node_diet, leaf_node_diet_no_stent, 
  p = p.diet_beta, cost = 0.0, label = "Success")

reaction_diet_failure_prob <- Reaction$new(
  chance_node_diet, leaf_node_diet_stent, 
  p = q.diet_beta, cost = cost_stent, label = "Failure")

reaction_exercise_success_prob <- Reaction$new(
  chance_node_exercise, leaf_node_exercise_no_stent, 
  p = p.exercise_beta, cost = 0.0, label = "Success")

reaction_exercise_failure_prob <- Reaction$new(
  chance_node_exercise, leaf_node_exercise_stent, 
  p = q.exercise_beta, cost = cost_stent, label = "Failure")

## ----decision-tree-prob-------------------------------------------------------
DT_prob <- DecisionTree$new(
  V = list(decision_node, 
           chance_node_diet, 
           chance_node_exercise, 
           leaf_node_diet_no_stent, 
           leaf_node_diet_stent, 
           leaf_node_exercise_no_stent, 
           leaf_node_exercise_stent),
  E = list(action_diet_prob,
           action_exercise_prob,
           reaction_diet_success_prob,
           reaction_diet_failure_prob,
           reaction_exercise_success_prob,
           reaction_exercise_failure_prob)
)

## ----modvar-table-------------------------------------------------------------
knitr::kable(DT_prob$modvar_table(), digits = 3L)

## ----dt-evaluate-expected-----------------------------------------------------
knitr::kable(DT_prob$evaluate(), digits = 2L)

## ----dt-evaluate-quantiles----------------------------------------------------
knitr::kable(data.frame(
  "Q2.5" = DT_prob$evaluate(setvars = "q2.5")$Cost,
  "Q97.5" = DT_prob$evaluate(setvars = "q97.5")$Cost,
  row.names = c("Diet", "Exercise")
), digits = 2L)

## ----dt-evaluate-random-------------------------------------------------------
N <- 1000L
DT_evaluation_random <- DT_prob$evaluate(setvars = "random", by = "run", N = N)
plot(DT_evaluation_random$Cost.Diet, DT_evaluation_random$Cost.Exercise,
     pch = 20L, 
     xlab = "Cost Diet (GBP)", ylab = "Cost Exercise (GBP)", 
     main = paste(N, "simulations of vascular disease prevention model"))
abline(a = 0.0, b = 1.0, col = "red")
knitr::kable(summary(DT_evaluation_random[, c(3L, 8L)]))

## ----dt-difference------------------------------------------------------------
DT_evaluation_random$Difference <- 
  DT_evaluation_random$Cost.Diet - DT_evaluation_random$Cost.Exercise
hist(DT_evaluation_random$Difference, 100L,  main = "Distribution of saving",
     xlab = "Saving (GBP)")
knitr::kable(DT_evaluation_random[1L : 10L, c(1L, 3L, 8L, 12L)], digits = 2L,
             row.names = FALSE)
CI <- quantile(DT_evaluation_random$Difference, c(0.025, 0.975))

## ----threshold----------------------------------------------------------------
cost_threshold <- DT_prob$threshold(
  index = list(action_exercise_prob),
  ref = list(action_diet_prob),
  outcome = "saving",
  mvd = cost_exercise$description(),
  a = 0.0, b = 5000.0, tol = 0.1
)

success_threshold <- DT_prob$threshold(
  index = list(action_exercise_prob),
  ref = list(action_diet_prob),
  outcome = "saving",
  mvd = p.exercise_beta$description(),
  a = 0.0, b = 1.0, tol = 0.001
)

