
test_that("illegal initializations are rejected", {
  expect_silent(NormModVar$new("norm", "GBP", 0.0, 1.0))
  expect_error(
    NormModVar$new(42L, 42L, 0.0, 1.0), class = "description_not_string"
  )
  expect_error(
    NormModVar$new("norm", 42L, 0.0, 1.0), class = "units_not_string"
  )
  expect_error(
    NormModVar$new("norm", "GBP", "0", 1.0), class = "mu_not_numeric"
  )
  expect_error(
    NormModVar$new("norm", "GBP", 0.0, "1"), class = "sigma_not_numeric"
  )
})

test_that("properties are correct", {
  sn <- NormModVar$new("sn", "GBP", 0.0, 1.0)
  expect_false(sn$is_expression())
  expect_true(sn$is_probabilistic())
})

test_that("modvar has correct distribution name", {
  sn <- NormModVar$new("sn", "GBP", 0.0, 1.0)
  expect_identical(sn$distribution(), "N(0,1)")
  n <- NormModVar$new("n", "GBP", 42.0, 1.0)
  expect_identical(n$distribution(), "N(42,1)")
})

test_that("quantile function checks inputs", {
  x <- NormModVar$new("x", "GBP", 0.0, 1.0)
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(x$quantile(probs))
  probs <- c(0.1, NA, 0.5)
  expect_error(x$quantile(probs), class = "probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(x$quantile(probs), class = "probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(x$quantile(probs), class = "probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5)
  expect_length(x$quantile(probs), 3L)
})

test_that("pe, mean, sd and quantiles are returned correctly", {
  sn <- NormModVar$new("sn", "GBP", 0.0, 1.0)
  expect_identical(sn$mean(), 0.0)
  expect_identical(sn$SD(), 1.0)
  probs <- c(0.025, 0.975)
  q <- sn$quantile(probs)
  expect_identical(round(q[[1L]], 2L), -1.96, 0.05)
  expect_identical(round(q[[2L]], 2L),  1.96, 0.05)
})

test_that("random sampling is from a Normal disribution", {
  mu <- 0.0
  sigma <- 1.0
  sn <- NormModVar$new("sn", "GBP", mu, sigma)
  n <- 1000L
  samp <- vapply(seq_len(n), FUN.VALUE = 1.0, FUN = function(i) {
    sn$set("random")
    rv <- sn$get()
    return(rv)
  })
  expect_length(samp, n)
  # check sample mean and sd are within 99.9% CI based on CLT; this is exact
  # for a normal, and is expected to fail for 0.1% of tests; skip for CRAN
  skip_on_cran()
  ht <- ks.test(samp, rnorm(n, mean = mu, sd = sigma))
  expect_gt(ht$p.value, 0.001)
})

test_that("First call to get() returns mean", {
  sn <- NormModVar$new("sn", "GBP", 0.0, 1.0)
  expect_identical(sn$get(), 0.0)
})

test_that("variable passing and persistency of get and set are correct", {
  f <- function(mv) {
    expect_equal(mv$get(), 0.0)
    mv$set("q2.5")
  }
  g <- function(mv) {
    expect_identical(mv$get(), 0.0)
  }
  sn <- NormModVar$new("sn", "GBP", 0.0, 1.0)
  f(sn)
  expect_false(sn$get() == 0.0)
  sn$set("expected")
  g(sn)
})
