library(testthat)
library(dgme)

test_that("dgme_parametrize creates valid params objects", {
  par1 <- dgme_parametrize("paper_example_1")
  expect_s3_class(par1, "dgme_params")
  expect_equal(par1$q, 0.8)
  expect_equal(dim(par1$e), c(2, 2))
  expect_equal(sum(par1$m), 20)

  par2 <- dgme_parametrize("paper_example_2")
  expect_s3_class(par2, "dgme_params")
  expect_equal(par2$q, 0.75)
})

test_that("dgme_parametrize random generation works", {
  par <- dgme_parametrize("random", style = "symmetric", seed = 42)
  expect_s3_class(par, "dgme_params")
  # Symmetric: mirror endowments
  expect_equal(par$e[1, 1], par$e[2, 2])
  expect_equal(par$e[1, 2], par$e[2, 1])
  # Symmetric: equal money
  expect_equal(par$m[1], par$m[2])
})

test_that("dgme_parametrize manual specification works", {
  par <- dgme_parametrize(NULL,
    e = matrix(c(5, 3, 3, 5), nrow = 2, byrow = TRUE),
    alpha = c(0.3, 0.7), m = c(8, 8), q = 0.85)
  expect_s3_class(par, "dgme_params")
  expect_equal(par$q, 0.85)
})

test_that("baseline solver converges for paper_example_1", {
  par <- dgme_parametrize("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")

  expect_s3_class(res, "dgme_results")
  expect_true("baseline" %in% res$variants_solved)

  bl <- res$results$baseline
  expect_true(bl$converged)

  # Prices must be positive
  expect_true(all(bl$p > 0))

  # Market clearing: sum of net trades must be zero
  expect_equal(colSums(bl$tau), c(0, 0), tolerance = 1e-8)

  # Flow-of-funds: M = (1-q) * D
  M <- sum(par$m)
  expect_equal(M, (1 - par$q) * bl$D, tolerance = 1e-6)

  # Consumption must be positive
  expect_true(all(bl$x > 0))

  # Each household is better off than autarky
  expect_true(all(bl$utilities > bl$autarky_utilities))
})

test_that("baseline solver converges for paper_example_2", {
  par <- dgme_parametrize("paper_example_2")
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline

  expect_true(bl$converged)
  expect_equal(colSums(bl$tau), c(0, 0), tolerance = 1e-8)

  M <- sum(par$m)
  expect_equal(M, (1 - par$q) * bl$D, tolerance = 1e-6)
  expect_true(all(bl$utilities > bl$autarky_utilities))
})

test_that("net nominal income transfers are non-negative at equilibrium", {
  par <- dgme_parametrize("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline

  # Both nu0 and nu1 must be >= 0 at feasibility
  expect_true(all(bl$nu >= -1e-8))
})

test_that("substitution variant equals baseline", {
  par <- dgme_parametrize("paper_example_1")
  res <- dgme_solve(par, variants = c("baseline", "substitution"))

  bl  <- res$results$baseline
  sub <- res$results$substitution

  expect_equal(bl$p, sub$p, tolerance = 1e-8)
  expect_equal(bl$x, sub$x, tolerance = 1e-8)
  expect_equal(bl$D, sub$D, tolerance = 1e-8)
})

test_that("solver handles asymmetric economies", {
  par <- dgme_parametrize(NULL,
    e = matrix(c(9, 1, 3, 7), nrow = 2, byrow = TRUE),
    alpha = c(0.3, 0.55), m = c(5, 15), q = 0.7)
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline

  expect_true(bl$converged)
  expect_equal(colSums(bl$tau), c(0, 0), tolerance = 1e-8)
  expect_true(all(bl$x > 0))
})

test_that("payment capacity equals M + qD", {
  par <- dgme_parametrize("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline

  expect_equal(bl$payment_capacity, sum(par$m) + par$q * bl$D,
               tolerance = 1e-8)
})

test_that("borrowing positions are non-positive", {
  par <- dgme_parametrize("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline

  expect_true(all(bl$b <= 1e-10))
})
