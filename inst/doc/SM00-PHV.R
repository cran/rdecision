## -----------------------------------------------------------------------------
library("rdecision")

## -----------------------------------------------------------------------------
s_well <- MarkovState$new(name = "Well")
s_disabled <- MarkovState$new(name = "Disabled")
s_dead <- MarkovState$new(name = "Dead")

## -----------------------------------------------------------------------------
E <- list(
  Transition$new(s_well, s_well),
  Transition$new(s_dead, s_dead),
  Transition$new(s_disabled, s_disabled),
  Transition$new(s_well, s_disabled),
  Transition$new(s_well, s_dead),
  Transition$new(s_disabled, s_dead)
)

## -----------------------------------------------------------------------------
m <- SemiMarkovModel$new(V = list(s_well, s_disabled, s_dead), E)

## -----------------------------------------------------------------------------
local({
  # create an igraph object (requires square plot region)
  gml <- m$as_gml()
  gmlfile <- tempfile(fileext = ".gml")
  writeLines(gml, con = gmlfile)
  ig <- igraph::read_graph(gmlfile, format = "gml")
  # match layout to Sonnenberg and Beck, fig 3
  vxy <- matrix(
    data = c(
      -0.75, +0.75, +0.00,
      +0.75, +0.75, -0.75
    ),
    ncol = 2L,
    dimnames = list(c("Well", "Disabled", "Dead"), c("x", "y"))
  )
  layout <- matrix(
    data = c(
      vapply(X = igraph::V(ig), FUN.VALUE = 1.0, FUN = function(v) {
        lbl <- igraph::vertex_attr(ig, "label", v)
        return(vxy[[lbl, "x"]])
      }),
      vapply(X = igraph::V(ig), FUN.VALUE = 1.0, FUN = function(v) {
        lbl <- igraph::vertex_attr(ig, "label", v)
        return(vxy[[lbl, "y"]])
      })
    ),
    byrow = FALSE,
    ncol = 2L
  )
  # loop angles
  loopa <- vapply(X = igraph::E(ig), FUN.VALUE = 1.0, FUN = function(e) {
    # find source and target labels
    trg <- igraph::head_of(ig, e)
    trgl <- igraph::vertex_attr(ig, name = "label", index = trg)
    src <- igraph::tail_of(ig, e)
    srcl <- igraph::vertex_attr(ig, name = "label", index = src)
    la <- 0.0
    if (trgl == srcl) {
      if (trgl == "Well") {
        la <- pi
      } else if (trgl == "Dead") {
        la <- pi / 2.0
      }
    }
    return(la)
  })
  # plot into png file
  withr::with_par(
    new = list(
      oma = c(0L, 0L, 0L, 0L),
      mar = c(3L, 3L, 3L, 3L),
      xpd = NA
    ),
    code = {
      plot(
        ig,
        rescale = FALSE, asp = 0L,
        vertex.shape = "circle", vertex.size = 60.0,
        vertex.color = "white", vertex.label.color = "black",
        edge.color = "black",
        edge.arrow.size = 0.75,
        frame = FALSE,
        layout = layout,
        loop.size = 0.8,
        edge.loop.angle = loopa
      )
    }
  )
})

## -----------------------------------------------------------------------------
s_disabled$set_utility(0.7)
s_dead$set_utility(0.0)

## -----------------------------------------------------------------------------
snames <- c("Well", "Disabled", "Dead")
pt <- matrix(
  data = c(NA, 0.2, 0.2, 0.0, NA, 0.4, 0.0, 0.0, NA),
  nrow = 3L, byrow = TRUE,
  dimnames = list(source = snames, target = snames)
)
m$set_probabilities(pt)

## -----------------------------------------------------------------------------
with(data = as.data.frame(pt), expr = {
  data.frame(
    Well = round(Well, digits = 3L),
    Disabled = round(Disabled, digits = 3L),
    Dead = round(Dead, digits = 3L),
    row.names = row.names(pt),
    stringsAsFactors = FALSE
  )
})

## -----------------------------------------------------------------------------
local({
  ptc <- m$transition_probabilities()
  with(data = as.data.frame(ptc), expr = {
    data.frame(
      Well = round(Well, digits = 3L),
      Disabled = round(Disabled, digits = 3L),
      Dead = round(Dead, digits = 3L),
      row.names = row.names(ptc),
      stringsAsFactors = FALSE
    )
  })
})

## -----------------------------------------------------------------------------
m$reset(populations = c(Well = 10000L, Disabled = 0L, Dead = 0L))

## -----------------------------------------------------------------------------
mt <- m$cycles(25L, hcc.pop = FALSE, hcc.cost = FALSE, hcc.QALY = FALSE)

## -----------------------------------------------------------------------------
t2 <- with(data = mt, expr = {
  data.frame(
    Cycle = Cycle,
    Well = round(Well, digits = 2L),
    Disabled = round(Disabled, digits = 2L),
    Dead = round(Dead, digits = 2L),
    QALY = round(QALY, digits = 4L),
    cQALY = round(cumsum(QALY), digits = 4L),
    stringsAsFactors = FALSE
  )
})
t2

