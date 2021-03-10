
# setequal function for Nodes
nodesetequal <- function(A,B) {
  AinB <- all(sapply(A, function(a) {
    return(any(sapply(B, function(b){identical(a,b)})))
  }))  
  BinA <- all(sapply(B, function(b) {
    return(any(sapply(A, function(a){identical(a,b)})))
  }))
  return(AinB & BinA)
}

# tests of graph creation
test_that("incorrect node and edge types are rejected", {
  n1 <- Node$new()
  n2 <- Node$new()
  e1 <- Edge$new(n1, n2)
  #
  expect_error(Graph$new(n1, list(e1)), class="non-list_vertices")
  expect_error(Graph$new(list(n1,n2), e1), class="non-list_edges")
  expect_error(Graph$new(list(n1,42), list(e1)), class="non-Node_vertex")
  expect_error(Graph$new(list(n1,n2), list(e1,42)), class="non-Edge_edge")
  expect_error(Graph$new(V=list(n1,n1), E=list(e1)), class="repeated_nodes")
  expect_error(Graph$new(V=list(n1,n2), E=list(e1,e1)), class="repeated_edges")
  #
  n3 <- Node$new()
  e2 <- Edge$new(n1,n3)
  expect_error(Graph$new(V=list(n1,n2),E=list(e1,e2)), class="not_in_graph")
  #
  n1 <- Node$new("n1")
  n2 <- Node$new("n1")
  n3 <- Node$new()
  e1 <- Edge$new(n1,n2,"e1")
  expect_silent(Graph$new(V=list(n1,n2),E=list(e1)))
  expect_silent(Graph$new(V=list(n1,n2,n3),E=list(e1)))
  #
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  e1 <- Edge$new(n1,n2,"e1")
  e2 <- Edge$new(n2,n1,"e1")
  e3 <- Edge$new(n1,n2)
  expect_silent(Graph$new(V=list(n1,n2),E=list(e1,e2)))
  expect_silent(Graph$new(V=list(n1,n2),E=list(e1,e2,e3)))
})

# tests of simple graph properties
test_that("basic graph properties are set and got", {
  #
  n1 <- Node$new()
  n2 <- Node$new()
  e1 <- Edge$new(n1, n2)
  #
  V <- list(n1,n2)
  E <- list(e1)
  G <- Graph$new(V, E)
  expect_equal(G$order(), length(V))
  expect_equal(G$size(), length(E))
  #
  V <- list(n1)
  E <- list()
  G <- Graph$new(V, E)
  expect_equal(G$order(), 1)
  expect_equal(G$size(), 0)
})

# tests of vertex and edge properties
test_that("vertex and edge properties are set and got", {
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Edge$new(n1, n2)
  G <- Graph$new(V=list(n1,n2), E=list(e1))
  #
  expect_error(G$has_vertex(42), class="invalid_vertex")
  expect_true(G$has_vertex(n1))
  expect_true(G$has_vertex(n2))
  expect_true(G$has_edge(e1))
  expect_false(G$has_vertex(n3))
  #
  expect_equal(G$vertex_index(n1),1)
  expect_equal(G$vertex_index(n2),2)
  expect_equal(G$edge_index(e1),1)
  #
  expect_error(G$edge_index(42), class="invalid_edge")
  #
  expect_error(G$degree(42), class="invalid_vertex")
  expect_error(G$degree(n3), class="not_in_graph")
  expect_error(G$degree(e1), class="invalid_vertex")
  expect_equal(G$degree(n1), 1)
})

# tests of adjacency matrix
test_that("adjacency matrix has correct properties", {
  # empty graph
  G <- Graph$new(V=list(),E=list())
  expect_error(G$graph_adjacency_matrix(42), class="non-logical_boolean")
  A <- G$graph_adjacency_matrix()
  expect_true(is.matrix(A))
  expect_equal(nrow(A),0)
  expect_equal(ncol(A),0)
  # trivial graph
  n1 <- Node$new()
  G <- Graph$new(V=list(n1),E=list())
  A <- G$graph_adjacency_matrix()
  expect_true(is.matrix(A))
  expect_equal(nrow(A),1)
  expect_equal(ncol(A),1)
  expect_equal(A[1,1],0)
  # named nodes
  n1 <- Node$new("n1")
  n2 <- Node$new()
  e1 <- Edge$new(n1,n2)
  G <- Graph$new(V=list(n1,n2),E=list(e1))
  A <- G$graph_adjacency_matrix()
  expect_true(is.null(dimnames(A))) 
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  e1 <- Edge$new(n1,n2)
  G <- Graph$new(V=list(n1,n2),E=list(e1))
  A <- G$graph_adjacency_matrix()
  dn <- dimnames(A)
  expect_equal(names(dn), c("out.node", "in.node"))  
  expect_equal(dn$out.node, c("n1", "n2"))
  expect_equal(dn$in.node, c("n1", "n2"))
  expect_equal(sum(A-matrix(c(0,1,0,1),nrow=2)),0)
  # binary
  n1 <- Node$new("n1")
  n2 <- Node$new("n2")
  e1 <- Edge$new(n1,n2)
  e2 <- Edge$new(n1,n1)
  G <- Graph$new(V=list(n1,n2),E=list(e1,e2))
  A <- G$graph_adjacency_matrix(boolean=FALSE)
  expect_equal(A["n1","n1"],2)
  A <- G$graph_adjacency_matrix(boolean=TRUE)
  expect_true(A["n1","n1"])
})

# tests of graph algorithms
test_that("simple and non-simple graphs are detected", {
  n1 <- Node$new()
  n2 <- Node$new()
  e1 <- Edge$new(n1,n2)
  #
  G <- Graph$new(V=list(n1,n2), E=list(e1))
  expect_true(G$is_simple())
  #
  e2 <- Edge$new(n1,n1)
  G <- Graph$new(V=list(n1,n2), E=list(e1,e2))
  expect_false(G$is_simple())
  #
  e2 <- Edge$new(n2,n1)
  G <- Graph$new(V=list(n1,n2), E=list(e1,e2))
  expect_false(G$is_simple())
})

test_that("connected and non-connected graphs are identified", {
  #
  G <- Graph$new(V=list(), E=list())
  expect_false(G$is_connected())
  # 
  n1 <- Node$new()
  G <- Graph$new(V=list(n1), E=list())
  expect_true(G$is_connected())
  e1 <- Edge$new(n1,n1)
  G <- Graph$new(V=list(n1), E=list(e1))
  expect_true(G$is_connected())
  #
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Edge$new(n1,n2)
  e2 <- Edge$new(n3,n3)
  G <- Graph$new(V=list(n1,n2,n3), E=list(e1,e2))
  expect_false(G$is_connected())
  #
  n1 <- Node$new()
  n2 <- Node$new()
  n3 <- Node$new()
  e1 <- Edge$new(n1,n2)
  e2 <- Edge$new(n1,n3)
  G <- Graph$new(V=list(n2,n3,n1), E=list(e1,e2))
  expect_true(G$is_connected())
})

test_that("cyclic and acyclic graphs are identified", {
  # 
  G <- Graph$new(V=list(), E=list())
  expect_true(G$is_simple())
  expect_true(G$is_acyclic())
  #
  n1 <- Node$new()
  G <- Graph$new(V=list(n1), E=list())
  expect_true(G$is_acyclic())
  #
  n1 <- Node$new()
  e1 <- Edge$new(n1,n1)
  G <- Graph$new(V=list(n1), E=list(e1))
  expect_false(G$is_acyclic())
  #
  n0 <- Node$new("0")
  n1 <- Node$new("1")
  n2 <- Node$new("2")
  n3 <- Node$new("3")
  e1 <- Edge$new(n0,n1)
  e2 <- Edge$new(n1,n2)
  e3 <- Edge$new(n2,n3)
  #
  G <- Graph$new(V=list(n0,n1,n2,n3), E=list(e1,e2,e3))
  expect_true(G$is_acyclic())
  #
  e4 <- Edge$new(n0,n2)
  G <- Graph$new(V=list(n0,n1,n2,n3), E=list(e1,e2,e3,e4))
  expect_false(G$is_acyclic())
})

# Published examples
test_that("Fig 1.1.1 from Gross & Yellen (2013)", {
  # the graph
  u <- Node$new("u")
  v <- Node$new("v")
  w <- Node$new("w")
  x <- Node$new("x")
  a <- Edge$new(u,v,"a")
  b <- Edge$new(v,u,"b")
  c <- Edge$new(x,x,"c")
  d <- Edge$new(x,w,"d")
  e <- Edge$new(x,v,"e")
  f <- Edge$new(w,v,"f")
  G <- Graph$new(V=list(u,v,w,x), E=list(a,b,c,d,e,f))
  # counts
  expect_equal(G$order(), 4)
  expect_equal(G$size(), 6)
  expect_equal(G$degree(u), 2)
  expect_equal(G$degree(v), 4)
  expect_equal(G$degree(w), 2)
  expect_equal(G$degree(x), 4)
  # adjacency
  A <- G$graph_adjacency_matrix()
  EA <- matrix(c(0,2,0,0, 2,0,1,1, 0,1,0,1, 0,1,1,2), nrow=4, byrow=TRUE,
               dimnames=list(out.node=c("u","v","w","x"),
                             in.node=c("u","v","w","x")))
  expect_identical(A, EA)
  # neighbours
  XX <- Node$new("XX")
  expect_error(G$neighbours(XX), class="not_in_graph")
  expect_true(nodesetequal(G$neighbours(u),list(v)))
  expect_true(nodesetequal(G$neighbours(v),list(u,w,x)))
  expect_true(nodesetequal(G$neighbours(w),list(v,x)))
  expect_true(nodesetequal(G$neighbours(x),list(v,w)))
  # connected
  expect_true(G$is_connected())
  # cycle
  expect_false(G$is_acyclic())
  # tree
  expect_false(G$is_tree())
})
