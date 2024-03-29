
test_that("properties are set correctly", {
  x <- 2L
  y <- 3.0
  expect_error(ExprModVar$new("z", "GBP", x + y), class = "quo_not_quosure")
  z <- ExprModVar$new("z", "GBP", quo = rlang::quo(x + y))
  expect_true(z$is_expression())
  expect_identical(z$distribution(), "x + y")
  expect_false(z$is_probabilistic())
  #
  y <- ConstModVar$new("y", "GBP", 42.0)
  z <- ExprModVar$new("z", "GBP", quo = rlang::quo(x + y))
  expect_true(z$is_expression())
  expect_identical(z$distribution(), "x + y")
  expect_false(z$is_probabilistic())
  #
  y <- NormModVar$new("y", "GBP", mu = 0.0, sigma = 1.0)
  z <- ExprModVar$new("z", "GBP", quo = rlang::quo(x + y))
  expect_true(z$is_expression())
  expect_identical(z$distribution(), "x + y")
  expect_true(z$is_probabilistic())
})

test_that("ExprModVar obeys scoping rules", {
  # operands in a function environment (test_that)
  x <- 2L
  y <- 3L
  z <- ExprModVar$new("z", "Z", quo = rlang::quo(x + y))
  expect_identical(z$distribution(), "x + y")
  expect_intol(z$mean(), 5.0, 0.1)
  # operands in different function environments
  f <- function() {
    y <- 4.0
    z <- ExprModVar$new("z", "Z", quo = rlang::quo(x + y))
    expect_intol(z$mean(), 6.0, 0.1)
  }
  f()
  # ExprModVar can be passed as an object
  x <- 20.0
  y <- 30.0
  z <- ExprModVar$new("z", "Z", quo = rlang::quo(x + y))
  g <- function(mv) {
    x <- 200.0
    y <- 300.0
    expect_intol(mv$mean(), 50.0, 1.0)
  }
  g(z)
})

test_that("stub quantile function checks inputs and has correct output", {
  x <- 2.0
  y <- 3.0
  z <- ExprModVar$new("z", "GBP", quo = rlang::quo(x + y))
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(z$quantile(probs))
  probs <- c(0.1, NA_real_, 0.5)
  expect_error(z$quantile(probs), class = "probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(z$quantile(probs), class = "probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(z$quantile(probs), class = "probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5)
  expect_length(z$quantile(probs), 3L)
})

test_that("operands are identified correctly", {
  # simple case
  x <- 2.0
  y <- NormModVar$new("y", "GBP", mu = 0.0, sigma = 1.0)
  z <- ConstModVar$new("z", "GPB", 42.0)
  e <- ExprModVar$new("e", "GBP", quo = rlang::quo(x * y + z))
  mv <- e$operands()
  expect_length(mv, 2L)  # y and z
  d <- vapply(mv, FUN.VALUE = "x", FUN = function(v) {
    return(v$description())
  })
  expect_setequal(d, c("y", "z"))
  # nested case, with repeats
  e1 <- ExprModVar$new("e1", "GBP", quo = rlang::quo(x * y + z))
  e2 <- ExprModVar$new("e2", "GBP", quo = rlang::quo(z + 3.0))
  e3 <- ExprModVar$new("e3", "GBP", quo = rlang::quo(e1 + e2))
  mv <- e3$operands()
  expect_length(mv, 4L)  # y, z, e1, e2
  d <- vapply(mv, FUN.VALUE = "x", FUN = function(v) {
    return(v$description())
  })
  expect_setequal(d, c("e1", "e2", "y", "z"))
  # deeper nesting
  e4 <- ExprModVar$new("e4", "", quo = rlang::quo(2.0 * e3))
  mv <- e4$operands()
  d <- vapply(mv, FUN.VALUE = "x", FUN = function(v) {
    return(v$description())
  })
  expect_setequal(d, c("e1", "e2", "e3", "y", "z"))
  # nesting without recursion
  mv <- e3$operands(recursive = FALSE)
  d <- vapply(mv, FUN.VALUE = "x", FUN = function(v) {
    return(v$description())
  })
  expect_setequal(d, c("e1", "e2"))
})

test_that("set and get function as expected", {
  # check initialization
  x <- 2L
  y <- NormModVar$new("y", "GBP", mu = 0.0, sigma = 1.0)
  z <- ExprModVar$new("z", "GBP", quo = rlang::quo(x * y))
  expect_false(is.na(z$get()))
  # check illegal input
  expect_error(z$set(TRUE), class = "what_not_character")
  expect_error(z$set("red"), class = "what_not_supported")
  expect_error(z$set("value"), class = "invalid_val")
  expect_error(z$set("value", val = TRUE), class = "invalid_val")
  # check value setting
  z$set("value", 10.0)
  expect_identical(z$get(), 10.0)
  # skip remaining tests on CRAN because they rely on sampling
  skip_on_cran()
  # check quantile, estimated from empirical distribution
  z$set("q97.5")
  v <- z$get()
  expect_gt(v, 3.5)
  # check mean is within 3.29 std errors (roughly 0.1%)
  z$set("expected")
  tol <- 3.29 * 2.0 / sqrt(1000.0)
  expect_intol(z$get(), 0.0, tol)
  # check that set() for operands affects get() for the expression
  y$set("q97.5")
  z$set("current")
  expect_gt(z$get(), 3.5)
  y$set("expected")
  expect_intol(z$get(), 0.0, tol)
  # check that a random sample from z is from a SN*2, despite setting y
  n <- 1000L
  S <- vector(mode = "numeric", length = n)
  for (i in seq_len(n)) {
    y$set("q97.5")
    z$set("random")
    S[[i]] <- z$get()
  }
  # 99.9% confidence limits; expected 0.1% test failure rate; skip for CRAN
  ht <- ks.test(S, rnorm(n, mean = 0.0, sd = 2.0))
  expect_gt(ht$p.value, 0.001)
})

test_that("modified expressions are created correctly", {
  alpha <- 1.0
  beta <- 9.0
  p <- BetaModVar$new("P(success)", "P", alpha = alpha, beta = beta)
  q <- ExprModVar$new("P(failure)", "P", rlang::quo(1.0 - p))
  # check externally added method
  expect_error(q$add_method(42.0), class = "method_not_character")
  q.mean <- q$add_method("mean()")
  expect_intol(
    eval(rlang::quo_get_expr(q.mean), envir = rlang::quo_get_env(q.mean)),
    0.9,
    0.05
  )
  expect_intol(q$mean(), 0.9, 0.05)
  # check that random sampling is supported
  q$set("random")
  rbeta <- q$get()
  expect_gte(rbeta, 0.0)
  expect_lte(rbeta, 1.0)
  # check internally added methods; 99.9% confidence limits assuming CLT; expect
  # 0.1% test failure rate; skip for CRAN
  n <- 1000L
  samp <- vapply(seq_len(n), FUN.VALUE = 1.0, FUN = function(i) {
    q$set("random")
    rv <- q$get()
    return(rv)
  })
  expect_length(samp, n)
  skip_on_cran()
  ht <- ks.test(samp, rbeta(n, shape1 = beta, shape2 = alpha))
  expect_gt(ht$p.value, 0.001)
})

test_that("illegal sample sizes for estimating parameters are rejected", {
  x <- 3.0
  y <- NormModVar$new("y", "GBP", mu = 0.0, sigma = 1.0)
  z <- ExprModVar$new("z", "GBP", quo = rlang::quo(x * y))
  expect_error(
    ExprModVar$new("z", "GBP", quo = rlang::quo(x * y), "100"),
    class = "nemp_not_integer"
  )
  expect_error(
    ExprModVar$new("z", "GBP", quo = rlang::quo(x * y), 100L),
    class = "nemp_too_small"
  )
  expect_error(
    ExprModVar$new("z", "GBP", quo = rlang::quo(x * y), 999.5),
    class = "nemp_not_integer"
  )
  expect_silent(
    ExprModVar$new("z", "GBP", quo = rlang::quo(x * y))
  )
  expect_silent(
    ExprModVar$new("z", "GBP", quo = rlang::quo(x * y), 10000L)
  )
})

test_that("quantile estimation checks inputs and has correct output", {
  p <- BetaModVar$new("P(success)", "P", alpha = 1.0, beta = 9.0)
  q <- ExprModVar$new("P(failure)", "P", rlang::quo(1.0 - p))
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(q$q_hat(probs))
  probs <- c(0.1, NA_real_, 0.5)
  expect_error(q$q_hat(probs), class = "probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(q$q_hat(probs), class = "probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(q$q_hat(probs), class = "probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5)
  expect_length(q$q_hat(probs), 3L)
})

test_that("nested expressions are evaluated correctly", {
  # skip test on cran because it involves sampling
  skip_on_cran()
  # standard normal and an expression with an identity operator
  sn1 <- NormModVar$new("sn1", "", mu = 0.0, sigma = 1.0)
  s1 <- ExprModVar$new("s1", "", rlang::quo(1.0 * sn1))
  # tolerance is 3.29 * standard error (roughly 0.1%)
  tol <- 3.29 * sqrt(2.0) / sqrt(1000L)
  # check that all 3 products have a mean of 1 (chisq with 1 dof)
  p1 <- ExprModVar$new("p1", "", rlang::quo(sn1 * sn1))
  expect_intol(p1$mu_hat(), 1.0, tol)
  p2 <- ExprModVar$new("p2", "", rlang::quo(s1 * s1))
  expect_intol(p2$mu_hat(), 1.0, tol)
  p3 <- ExprModVar$new("p3", "", rlang::quo(s1 * sn1))
  expect_intol(p3$mu_hat(), 1.0, tol)
})

test_that("autocorrelation in nested expressions is preserved", {
  # skip test on cran because it involves sampling
  skip_on_cran()
  # create expressions to wrap standard normals
  sn1 <- NormModVar$new("sn1", "", mu = 0.0, sigma = 1.0)
  s1 <- ExprModVar$new("s1", "", rlang::quo(1.0 * sn1))
  sn2 <- NormModVar$new("sn2", "", mu = 0.0, sigma = 1.0)
  s2 <- ExprModVar$new("s2", "", rlang::quo(1.0 * sn2))
  x <- ExprModVar$new("x", "", rlang::quo(1.0 * s1))
  y <- ExprModVar$new("y", "", rlang::quo(1.0 * s2))
  # create nested correlated and nested uncorrelated expressions
  zc <- ExprModVar$new("zc", "", rlang::quo(x * s1))
  zu <- ExprModVar$new("zu", "", rlang::quo(y * s1))
  # tolerance is 3.29 * standard error (roughly 0.1%)
  tol <- 3.29 * sqrt(2.0) / sqrt(1000L)
  # zc is a chi-squared with 1 dof
  expect_intol(zc$mu_hat(), 1.0, tol)
  # zu is a modified Bessel function with mean 0 and sd 1
  expect_intol(zu$mu_hat(), 0.0, tol)
})

test_that("scalar expression is evaluated correctly", {
  x <- ExprModVar$new("x", "", rlang::quo(1.0))
  expect_false(x$is_probabilistic())
  expect_length(x$operands(), 0L)
  expect_identical(x$distribution(), "1")
  expect_intol(x$mean(), 1.0, 0.001)
  expect_true(is.na(x$mode()))
  expect_true(is.na(x$SD()))
  expect_identical(x$quantile(probs = c(0.025, 0.975)), c(NA_real_, NA_real_))
  expect_intol(x$mu_hat(), 1.0, 0.01)
  expect_intol(x$sigma_hat(), 0.0, 0.01)
  expect_intol(x$q_hat(probs = 0.025), 1.0, 0.01)
  expect_intol(x$q_hat(probs = 0.975), 1.0, 0.01)
})

test_that("expression chi square from SN is correct", {
  # x ~ N(0,1), y = x^2 ~ Chisq(k=1)
  k <- 1L
  x <- NormModVar$new("x", "", mu = 0.0, sigma = 1.0)
  y <- ExprModVar$new("y", "", rlang::quo(x ^ 2L))
  # check that mode and SD are undefined
  expect_true(is.na(y$mode()))  # mode is undefined for ExprModVar
  expect_true(is.na(y$SD()))  # SD is undefined for ExprModVar
  # skip sampling tests on cran
  skip_on_cran()
  # tolerance is 3.29 * standard error (roughly 0.1%)
  tol <- 3.29 * sqrt(2L * k) / sqrt(1000L)
  expect_equal(y$mean(), 0.0)          # product of operand means is 0
  expect_intol(y$mu_hat(), k, tol)  # true mean is k=1
  median <- k * (1L - 2L / (9L * k)) ^ 3L
  expect_intol(y$q_hat(p = 0.5), median, tol)
  # generate a distribution and check it
  n <- 1000L
  samp <- vapply(seq_len(n), FUN.VALUE = 1.0, FUN = function(i) {
    y$set("random")
    rv <- y$get()
    return(rv)
  })
  ht <- ks.test(samp, rchisq(n, df = 1L))
  expect_gt(ht$p.value, 0.001)
})

test_that("one Dirichlet matches a Beta and an expression", {
  # skip on cran because tests involve sampling
  skip_on_cran()
  # p follows Beta(1,9) and q is 1-p
  alpha <- 1.0
  beta <- 9.0
  p <- BetaModVar$new("P(success)", "P", alpha = alpha, beta = beta)
  q <- ExprModVar$new("P(failure)", "P", rlang::quo(1.0 - p))
  # p and q are both derived from Dir(1,9) distribution
  D <- DirichletDistribution$new(alpha = c(1.0, 9.0))
  p.d <- ModVar$new("P(success)", "P", D = D, k = 1L)
  q.d <- ModVar$new("P(failure)", "P", D = D, k = 2L)
  # tolerance is 3.29 standard errors (approx 0.1%)
  sd <- sqrt((alpha * beta) / ((alpha + beta) ^ 2L * (alpha + beta + 1L)))
  tol <- 3.29 * sd / sqrt(1000L)
  # compare means
  expect_identical(p$mean(), p.d$mean())
  expect_intol(q$mean(), q.d$mean(), tol)
  # compare quantiles for p
  probs <- c(0.025, 0.975)
  expect_setequal(unname(p$quantile(probs)), unname(p.d$quantile(probs)))
  # quantiles defined for q.d but not q
  expect_true(all(is.na(q$quantile(probs))))
  expect_false(anyNA(q.d$quantile(probs)))
})
