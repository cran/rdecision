## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library("rdecision")

## ----diagram, echo=FALSE, results='hide', fig.keep='last', fig.align='center'----
# new page
grid::grid.newpage()
# functions to transform coordinates and distances in graph space (0:300) 
# to grid space (cm)
fig.size <- dev.size("cm")
scale <- max(300.0 / fig.size[1L], 300.0 / fig.size[2L])
gx <- function(x) {
  xcm <- fig.size[1L] / 2.0 + (x - 150.0) / scale
  return(xcm)
}
gy <- function(y) {
  ycm <- fig.size[2L] / 2.0 + (y - 150.0) / scale
  return(ycm)
}
gd <- function(d) {
  dcm <- d / scale
  return(dcm)
}
# grid
for (x in seq(50L, 250L, 50L)) {
  grid::grid.move.to(
    x = grid::unit(gx(x), "cm"), y = grid::unit(gy(50.0), "cm")
  )
  grid::grid.line.to(
    x = grid::unit(gx(x), "cm"), y = grid::unit(gy(250.0), "cm"), 
    gp = grid::gpar(lwd = 2.0)
  )
}
for (y in seq(50L, 250L, 50L)) {
  grid::grid.move.to(
    x = grid::unit(gx(50.0), "cm"), y = grid::unit(gy(y), "cm")
  )
  grid::grid.line.to(
    x = grid::unit(gx(250.0), "cm"), y = grid::unit(gy(y), "cm"), 
    gp = grid::gpar(lwd = 2.0)
  )
}
grid::grid.text(
  label = "A", x = grid::unit(gx(45.0), "cm"), y = grid::unit(gy(255.0), "cm"), 
  gp = grid::gpar(fontsize = 14.0)
)
grid::grid.text(
  label = "B", x = grid::unit(gx(255.0), "cm"), y = grid::unit(gy(45.0), "cm"),
  gp = grid::gpar(fontsize = 14.0)
)
# restaurants
BB <- data.frame(
  x0 = c(150.0, 100.0, 210.0, 160.0, 250.0, 110.0, 50.0),
  y0 = c(60.0, 110.0, 100.0, 150.0, 160.0, 200.0, 210.0),
  x1 = c(150.0, 100.0, 240.0, 190.0, 250.0, 140.0, 50.0),
  y1 = c(90.0, 140.0, 100.0, 150.0, 190.0, 200.0, 240.0)
)
apply(BB, MARGIN = 1L, function(r) {
  grid::grid.move.to(
    x = grid::unit(gx(r["x0"]), "cm"), y = grid::unit(gy(r["y0"]), "cm")
  )
  grid::grid.line.to(
    x = grid::unit(gx(r["x1"]), "cm"), 
    y = grid::unit(gy(r["y1"]), "cm"), 
    gp = grid::gpar(col = "red", lwd = 6.0, lend = "square")
  )
})

## ----construct-graph, echo=TRUE-----------------------------------------------
# create vertices
V <- list()
for (i in 1L : 5L) {
  for (j in 1L : 5L) {
    V <- c(V, Node$new(paste0("N", i, j)))
  }
}
# create edges
E <- list()
for (i in 1L : 5L) {
  for (j in 1L : 4L) {
    a <- Arrow$new(
      V[[5L * (i - 1L) + j]], V[[5L * (i - 1L) + j + 1L]], paste0("H", i, j)
    )
    E <- c(E, a)
  }
} 
for (i in 1L : 4L) {
  for (j in 1L : 5L) {
    a <- Arrow$new(
      V[[5L * (i - 1L) + j]], V[[5L * i + j]], paste0("V", i, j)
    )
    E <- c(E, a)
  }
} 
# create graph
G <- Digraph$new(V, E)

## ----findpaths, echo=TRUE, results = "markdown"-------------------------------
# get all paths from A to B
A <- V[[1L]]
B <- V[[25L]]
P <- G$paths(A, B)
# convert paths to walks
W <- lapply(P, function(p) G$walk(p))
# count and tabulate how many special edges each walk traverses
BB <- c("V11", "H22", "V25", "H33", "V32", "H44", "V43")
nw <- vapply(W, FUN.VALUE = 1L, FUN = function(w) {
  lv <- vapply(w, FUN.VALUE = TRUE, FUN = function(e) e$label() %in% BB) 
  return(sum(lv))
})
# tabulate 
ct <- as.data.frame(table(nw))

## ----echo=FALSE, results='markdown'-------------------------------------------
names(ct) <- c("n", "frequency")
knitr::kable(ct)

