
test_that("illegal initializations are rejected", {
  expect_silent(NormModVar$new("norm", "GBP", 0, 1))
  expect_error(NormModVar$new(42,42,0,1), class="description_not_string")
  expect_error(NormModVar$new("norm",42,0,1), class="units_not_string")
  expect_error(NormModVar$new("norm","GBP","0",1), class="mu_not_numeric")
  expect_error(NormModVar$new("norm","GBP",0,"1"), class="sigma_not_numeric")
})

test_that("properties are correct", {
  sn <- NormModVar$new("sn", "GBP", 0, 1)
  expect_false(sn$is_expression())
  expect_true(sn$is_probabilistic())
})

test_that("modvar has correct distribution name", {
  sn <- NormModVar$new("sn", "GBP", 0, 1)
  expect_equal(sn$distribution(), "N(0,1)")  
  n <- NormModVar$new("n", "GBP", 42, 1)
  expect_equal(n$distribution(), "N(42,1)")  
})

test_that("quantile function checks inputs", {
  x <- NormModVar$new("x", "GBP", 0, 1)
  probs <- c(0.1, 0.2, 0.5)
  expect_silent(x$quantile(probs))
  probs <- c(0.1, NA, 0.5)
  expect_error(x$quantile(probs), class="probs_not_defined")
  probs <- c(0.1, "boo", 0.5)
  expect_error(x$quantile(probs), class="probs_not_numeric")
  probs <- c(0.1, 0.4, 1.5)
  expect_error(x$quantile(probs), class="probs_out_of_range")
  probs <- c(0.1, 0.2, 0.5)
  expect_equal(length(x$quantile(probs)),3)
})

test_that("pe, mean, sd and quantiles are returned correctly", {
  sn <- NormModVar$new("sn", "GBP", 0, 1)
  expect_equal(sn$mean(), 0)
  expect_equal(sn$SD(), 1)
  probs <- c(0.025, 0.975)
  q <- sn$quantile(probs)
  expect_true(abs(q[1]-(-1.96))<0.05)
  expect_true(abs(q[2]-1.96)<0.05)
})

test_that("random sampling is from a Normal distribution", {
  sn <- NormModVar$new("sn", "GBP", 0, 1)
  samp <- sn$r(1000)
  expect_equal(length(samp), 1000)
  expect_true(abs(mean(samp)-0)<0.1)
  expect_true(abs(sd(samp)-1)<0.1)
})

test_that("First call to get() returns mean", {
  sn <- NormModVar$new("sn", "GBP", 0, 1)
  expect_equal(sn$get(), 0)  
})

test_that("variable passing and persistency of get/set are correct", {
  f <- function(mv) {
    expect_equal(mv$get(), 0)
    mv$set("q2.5")
  }
  g <- function(mv) {
    expect_equal(mv$get(), 0)
  }
  sn <- NormModVar$new("sn", "GBP", 0, 1)
  f(sn)
  expect_false(sn$get()==0)
  sn$set("expected")
  g(sn)
})