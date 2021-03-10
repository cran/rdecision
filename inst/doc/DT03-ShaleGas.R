## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=F------------------------------------------------------------
library(rdecision)

## ----create-model, echo=TRUE--------------------------------------------------
  # nodes
  d1 <- DecisionNode$new("d1")
  d2 <- DecisionNode$new("d2")
  d3 <- DecisionNode$new("d3")
  c1 <- ChanceNode$new("c1")
  c2 <- ChanceNode$new("c2")
  c3 <- ChanceNode$new("c3")
  c4 <- ChanceNode$new("c4")
  t1 <- LeafNode$new("t1")
  t2 <- LeafNode$new("t2")
  t3 <- LeafNode$new("t3")
  t4 <- LeafNode$new("t4")
  t5 <- LeafNode$new("t5")
  t6 <- LeafNode$new("t6")
  t7 <- LeafNode$new("t7")
  t8 <- LeafNode$new("t8")
  t9 <- LeafNode$new("t9")
  # probabilities
  p.sens <- 0.9
  p.spec <- 0.7
  p.gas <- 0.7
  p.nogas <- 1-p.gas
  p.ptest <- p.sens*p.gas + (1-p.spec)*p.nogas
  p.ntest <- (1-p.sens)*p.gas + p.spec*p.nogas
  p.gas.ptest <- p.sens*p.gas / p.ptest
  p.gas.ntest <- (1-p.sens)*p.gas / p.ntest
  # edges
  E <- list(
    Action$new(d1,t1,"sell",benefit=800),
    Action$new(d1,c1,"dig",cost=300),
    Reaction$new(c1,t2,p=p.gas,benefit=2500,label="gas"),
    Reaction$new(c1,t3,p=p.nogas,label="no gas"),
    Action$new(d1,c2,"test",cost=50),
    Reaction$new(c2,d2,p=p.ntest,label="negative"),
    Action$new(d2,t4,"sell",benefit=600),
    Action$new(d2,c3,"dig",cost=300),
    Reaction$new(c3,t5,p=p.gas.ntest,benefit=2500,label="gas"),
    Reaction$new(c3,t6,p=(1-p.gas.ntest),label="no gas"),
    Reaction$new(c2,d3,p=p.ptest,label="positive"),
    Action$new(d3,t7,"sell",benefit=1000),
    Action$new(d3,c4,"dig",cost=300),
    Reaction$new(c4,t8,p=p.gas.ptest,benefit=2500,label="gas"),
    Reaction$new(c4,t9,p=(1-p.gas.ptest),label="no gas")
  )
  # tree
  V <- list(d1,d2,d3, c1,c2,c3,c4, t1,t2,t3,t4,t5,t6,t7,t8,t9)
  DT<-DecisionTree$new(V,E)

## ----fig1c, echo=FALSE--------------------------------------------------------
f1c <- "Decision tree used in the shale gas problem"

## ----draw,echo=FALSE,results="hide",fig.keep="last",fig.align="center",fig.cap=f1c----
DT$draw(border=TRUE)

## ----evaluate, echo=TRUE------------------------------------------------------
# find optimal strategies
RES <- DT$evaluate()
RES$Payoff <- RES$Benefit-RES$Cost

## ----results, echo=FALSE, results='asis'--------------------------------------
RES$Run <- NULL
RES$Probability <- NULL
RES$Utility <- NULL
RES$QALY <- NULL
knitr::kable(RES, row.names=FALSE)

## ----poptimal, echo=FALSE-----------------------------------------------------
imax <- which.max(RES$Payoff)
popt <- paste(RES$d1[imax], RES$d2[imax], RES$d3[imax], sep="/")
