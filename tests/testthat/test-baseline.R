# ===========================================================================
# Tests for dgme package
# ===========================================================================

# Suppress expected gains-from-trade warnings in test presets
quiet_par <- function(...) suppressWarnings(dgme_parametrize(...))


# ---------------------------------------------------------------------------
# dgme_parametrize
# ---------------------------------------------------------------------------

test_that("dgme_parametrize creates valid params for paper_example_1", {
  par1 <- quiet_par("paper_example_1")
  expect_s3_class(par1, "dgme_params")
  expect_equal(par1$q, 0.6)
  expect_equal(dim(par1$e), c(2, 2))
  expect_equal(sum(par1$m), 2.2)

  # Canonical preset now includes Treasury and competing defaults

  expect_equal(par1$rho, 0.10)
  expect_equal(par1$eta, 0.80)
  expect_equal(par1$S, 1.0)
  expect_equal(par1$m_S, c(0.2, 0.9))
  expect_true(!is.null(par1$e_grid))
})

test_that("dgme_parametrize creates valid params for paper_example_2", {
  par2 <- quiet_par("paper_example_2")
  expect_s3_class(par2, "dgme_params")
  expect_equal(par2$q, 0.75)
})

test_that("dgme_parametrize random generation works", {
  par <- quiet_par("random", style = "symmetric", seed = 42)
  expect_s3_class(par, "dgme_params")
  expect_equal(par$e[1, 1], par$e[2, 2])
  expect_equal(par$e[1, 2], par$e[2, 1])
  expect_equal(par$m[1], par$m[2])
})

test_that("dgme_parametrize manual specification works", {
  par <- quiet_par(NULL,
    e = matrix(c(5, 3, 3, 5), nrow = 2, byrow = TRUE),
    alpha = c(0.3, 0.7), m = c(8, 8), q = 0.85)
  expect_s3_class(par, "dgme_params")
  expect_equal(par$q, 0.85)
})


# ---------------------------------------------------------------------------
# dgme_solve — baseline variant
# ---------------------------------------------------------------------------

test_that("baseline solver converges for paper_example_1", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")

  expect_s3_class(res, "dgme_results")
  expect_true("baseline" %in% res$variants_solved)

  bl <- res$results$baseline
  expect_true(bl$converged)
  expect_true(all(bl$p > 0))
  expect_equal(unname(colSums(bl$tau)), c(0, 0), tolerance = 1e-8)

  M <- sum(par$m)
  expect_equal(M, (1 - par$q) * bl$D, tolerance = 1e-6)
  expect_true(all(bl$x > 0))
  expect_true(all(bl$utilities > bl$autarky_utilities))
})

test_that("baseline solver converges for paper_example_2", {
  par <- quiet_par("paper_example_2")
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline

  expect_true(bl$converged)
  expect_equal(unname(colSums(bl$tau)), c(0, 0), tolerance = 1e-8)
  expect_equal(sum(par$m), (1 - par$q) * bl$D, tolerance = 1e-6)
  expect_true(all(bl$utilities > bl$autarky_utilities))
})

test_that("net nominal income transfers are non-negative at equilibrium", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline
  expect_true(all(bl$nu >= -1e-8))
})

test_that("payment capacity equals M + qD", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline
  expect_equal(bl$payment_capacity, sum(par$m) + par$q * bl$D,
               tolerance = 1e-8)
})

test_that("borrowing positions are non-positive", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline
  expect_true(all(bl$b <= 1e-10))
})


# ---------------------------------------------------------------------------
# dgme_solve — substitution variant
# ---------------------------------------------------------------------------

test_that("substitution variant equals baseline", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = c("baseline", "substitution"))

  bl  <- res$results$baseline
  sub <- res$results$substitution

  expect_equal(bl$p, sub$p, tolerance = 1e-8)
  expect_equal(bl$x, sub$x, tolerance = 1e-8)
  expect_equal(bl$D, sub$D, tolerance = 1e-8)
})


# ---------------------------------------------------------------------------
# dgme_solve — asymmetric economies
# ---------------------------------------------------------------------------

test_that("solver handles asymmetric economies", {
  par <- quiet_par(NULL,
    e = matrix(c(9, 1, 3, 7), nrow = 2, byrow = TRUE),
    alpha = c(0.3, 0.55), m = c(5, 15), q = 0.7)
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline

  expect_true(bl$converged)
  expect_equal(unname(colSums(bl$tau)), c(0, 0), tolerance = 1e-8)
  expect_true(all(bl$x > 0))
})


# ---------------------------------------------------------------------------
# dgme_solve — treasury variant (Variant 2)
# ---------------------------------------------------------------------------

test_that("treasury solver converges and passes consistency checks", {
  par <- quiet_par("paper_example_1", rho = 0.1, eta = 0.8, S = 2)
  res <- dgme_solve(par, variants = "treasury")
  tr  <- res$results$treasury

  expect_true(tr$converged)
  expect_true(tr$check$passed)
  expect_equal(unname(colSums(tr$tau)), c(0, 0), tolerance = 1e-8)
  expect_true(all(tr$x > 0))

  G_expected <- 0.8 * (1 - 0.1) * 2
  expect_equal(tr$G, G_expected)
})

test_that("treasury is inflationary: prices higher than baseline", {
  par <- quiet_par("paper_example_1", rho = 0.1, eta = 0.8, S = 2)
  res <- dgme_solve(par, variants = c("baseline", "treasury"))

  bl <- res$results$baseline
  tr <- res$results$treasury
  expect_true(all(tr$p > bl$p))
})

test_that("treasury payment capacity expands by G/(1-q)", {
  par <- quiet_par("paper_example_1", rho = 0.1, eta = 0.8, S = 2)
  res <- dgme_solve(par, variants = c("baseline", "treasury"))

  bl <- res$results$baseline
  tr <- res$results$treasury

  expected_expansion <- tr$G / (1 - par$q)
  actual_expansion <- tr$payment_capacity - bl$payment_capacity
  expect_equal(actual_expansion, expected_expansion, tolerance = 1e-6)
})

test_that("treasury is neutral when rho = 1 (full custody)", {
  par <- quiet_par("paper_example_1", rho = 1, eta = 0.8, S = 2)
  res <- dgme_solve(par, variants = c("baseline", "treasury"))

  expect_equal(res$results$baseline$p, res$results$treasury$p, tolerance = 1e-8)
  expect_equal(res$results$baseline$x, res$results$treasury$x, tolerance = 1e-8)
})

test_that("treasury is neutral when eta = 0 (pure substitution)", {
  par <- quiet_par("paper_example_1", rho = 0.1, eta = 0, S = 2)
  res <- dgme_solve(par, variants = c("baseline", "treasury"))

  expect_equal(res$results$baseline$p, res$results$treasury$p, tolerance = 1e-8)
  expect_equal(res$results$baseline$x, res$results$treasury$x, tolerance = 1e-8)
})

test_that("treasury skips when parameters missing", {
  # Use a manual economy without treasury defaults
  par <- quiet_par(NULL,
    e = matrix(c(5, 3, 3, 5), nrow = 2, byrow = TRUE),
    alpha = c(0.3, 0.7), m = c(8, 8), q = 0.85)
  res <- suppressWarnings(dgme_solve(par, variants = "treasury"))
  expect_false("treasury" %in% res$variants_solved)
})


# ---------------------------------------------------------------------------
# dgme_solve — competing variant (Variant 4)
# ---------------------------------------------------------------------------

test_that("competing solver converges and passes consistency checks", {
  par <- quiet_par("paper_example_1",
                   m_S = c(0.5, 0.5), e_grid = seq(0.5, 2, by = 0.5))
  res <- dgme_solve(par, variants = "competing")
  comp <- res$results$competing

  expect_true(comp$converged)
  expect_true(comp$check$passed)
  expect_equal(unname(colSums(comp$tau)), c(0, 0), tolerance = 1e-8)
  expect_true(all(comp$x > 0))
})

test_that("competing: D and D_S constant across exchange-rate grid", {
  par <- quiet_par("paper_example_1",
                   m_S = c(0.5, 0.5), e_grid = seq(0.5, 2, by = 0.5))
  res <- dgme_solve(par, variants = "competing")
  comp <- res$results$competing

  expect_true(all(abs(comp$D_grid - 5.5) < 1e-6))
  expect_true(all(abs(comp$D_S_grid - 2.5) < 1e-6))
})

test_that("competing: sovereign prices decrease as e_bar increases", {
  par <- quiet_par("paper_example_1",
                   m_S = c(0.5, 0.5), e_grid = seq(0.5, 2, by = 0.5))
  res <- dgme_solve(par, variants = "competing")
  comp <- res$results$competing

  expect_true(all(diff(comp$p_grid[, 1]) < 0))
  expect_true(all(diff(comp$p_grid[, 2]) < 0))
})

test_that("competing skips when m_S not set", {
  # Use a manual economy without competing defaults
  par <- quiet_par(NULL,
    e = matrix(c(5, 3, 3, 5), nrow = 2, byrow = TRUE),
    alpha = c(0.3, 0.7), m = c(8, 8), q = 0.85)
  res <- suppressWarnings(dgme_solve(par, variants = "competing"))
  expect_false("competing" %in% res$variants_solved)
})


# ---------------------------------------------------------------------------
# Canonical preset: all variants solve out of the box
# ---------------------------------------------------------------------------

test_that("paper_example_1 solves all four variants", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "all")

  expect_true("baseline" %in% res$variants_solved)
  expect_true("substitution" %in% res$variants_solved)
  expect_true("treasury" %in% res$variants_solved)
  expect_true("competing" %in% res$variants_solved)
})

test_that("canonical treasury is inflationary at default parameters", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = c("baseline", "treasury"))

  bl <- res$results$baseline
  tr <- res$results$treasury
  expect_true(all(tr$p > bl$p))
  expect_equal(tr$G, 0.8 * (1 - 0.1) * 1.0)  # eta*(1-rho)*S
})


# ---------------------------------------------------------------------------
# Proposition 4(iii): real-allocation invariance with proportional m_S
# ---------------------------------------------------------------------------

test_that("competing: proportional m_S yields invariant relative prices", {
  # Canonical preset has m_S = 0.5 * m = (0.2, 0.9), which is proportional.
  # Relative prices must be constant across e_grid (Proposition 4(iii)).
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "competing")
  comp <- res$results$competing

  # Relative prices constant across exchange rates
  rel_p <- comp$p_grid[, 1] / comp$p_grid[, 2]
  expect_true(max(abs(rel_p - rel_p[1])) < 1e-6)

  # Nominal prices decrease as e increases (more stablecoin money = less
  # sovereign-equivalent money M + M_S/e, so lower prices)
  expect_true(all(diff(comp$p_grid[, 1]) < 0))
  expect_true(all(diff(comp$p_grid[, 2]) < 0))
})


# ---------------------------------------------------------------------------
# dgme_vary
# ---------------------------------------------------------------------------

test_that("dgme_vary creates correct number of economies", {
  base <- quiet_par("paper_example_1")

  fam1 <- dgme_vary(base, q = 0.5)
  expect_s3_class(fam1, "dgme_vary")
  expect_equal(fam1$n, 1)
  expect_equal(fam1[[1]]$q, 0.5)

  fam7 <- dgme_vary(base, q = seq(0.3, 0.9, by = 0.1))
  expect_equal(fam7$n, 7)
  expect_equal(fam7[[1]]$q, 0.3)
  expect_equal(fam7[[7]]$q, 0.9)
})

test_that("dgme_vary cross product works", {
  base_v2 <- quiet_par("paper_example_1", rho = 0.2, eta = 0.5, S = 5)
  fam <- dgme_vary(base_v2, q = c(0.5, 0.7), S = c(0, 5, 10))
  expect_equal(fam$n, 6)
  expect_equal(fam$grid_type, "cross")
})

test_that("dgme_vary parallel mode works", {
  base <- quiet_par("paper_example_1")
  fam <- dgme_vary(base,
    q = c(0.4, 0.6, 0.8),
    m = list(c(0.2, 1), c(0.4, 1.8), c(1, 3)),
    grid_type = "parallel")
  expect_equal(fam$n, 3)
  expect_equal(fam$grid_type, "parallel")
  expect_equal(fam[[2]]$q, 0.6)
})

test_that("dgme_vary preserves base economy", {
  base <- quiet_par("paper_example_1")
  fam <- dgme_vary(base, q = 0.5)
  expect_equal(fam$base$q, base$q)
  expect_s3_class(fam$base, "dgme_params")
})

test_that("dgme_vary rejects unknown parameters", {
  base <- quiet_par("paper_example_1")
  expect_error(dgme_vary(base, z = 1), "Cannot vary")
})

test_that("dgme_vary rejects parallel with mismatched lengths", {
  base <- quiet_par("paper_example_1")
  expect_error(
    dgme_vary(base, q = c(0.4, 0.6), m = list(c(1, 1)), grid_type = "parallel"),
    "same length"
  )
})

test_that("dgme_vary [[ extraction returns dgme_params", {
  base <- quiet_par("paper_example_1")
  fam <- dgme_vary(base, q = c(0.4, 0.5, 0.6))
  expect_s3_class(fam[[2]], "dgme_params")
  expect_equal(length(fam), 3)
})


# ---------------------------------------------------------------------------
# dgme_geometry
# ---------------------------------------------------------------------------

test_that("dgme_geometry returns correct structure", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline

  geom <- dgme_geometry(bl)
  expect_s3_class(geom, "dgme_geometry")
  expect_true(all(c("params", "result", "na_real", "me_nominal") %in% names(geom)))

  na <- geom$na_real
  expect_true(all(c("kinks_x", "kinks_tau", "tau_opt", "window", "scale",
                     "ic", "regime", "price_normal") %in% names(na)))
  expect_equal(length(na$kinks_x), 2)
  expect_equal(sort(names(na$kinks_x[[1]])), c("A", "B", "C", "D"))
  expect_true(na$window$Xmin < na$window$Xmax)
  expect_true(na$scale$Unit > 0)
})

test_that("dgme_geometry ME-nominal has correct bond points", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline

  geom <- dgme_geometry(bl)
  me <- geom$me_nominal

  q <- par$q
  for (h in 1:2) {
    bp <- me$bond_pts[[h]]
    if (abs(bp[1]) > 1e-10) {
      expect_equal(bp[2] / bp[1], -1 / q, tolerance = 1e-8)
    }
  }
})

test_that("dgme_geometry NA-real tau_opt sums to zero", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline

  geom <- dgme_geometry(bl)
  tau_sum <- geom$na_real$tau_opt$h1 + geom$na_real$tau_opt$h2
  expect_equal(tau_sum, c(0, 0), tolerance = 1e-8)
})


# ---------------------------------------------------------------------------
# dgme_tikz
# ---------------------------------------------------------------------------

test_that("dgme_tikz_na_real produces valid TikZ string", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline

  out <- dgme_tikz_na_real(bl, write = FALSE)
  expect_type(out, "list")
  expect_null(out$path)

  tex <- out$tex
  expect_true(grepl("\\\\documentclass", tex))
  expect_true(grepl("\\\\begin\\{tikzpicture\\}", tex))
  expect_true(grepl("\\\\end\\{document\\}", tex))
})

test_that("dgme_tikz_me_nominal produces valid TikZ string", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline

  out <- dgme_tikz_me_nominal(bl, write = FALSE)
  tex <- out$tex
  expect_true(grepl("\\\\documentclass", tex))
  expect_true(grepl("\\\\begin\\{tikzpicture\\}", tex))
})

test_that("dgme_tikz_me_nominal respects show_dual_cone and show_bank", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline

  out_dc <- dgme_tikz_me_nominal(bl, show_dual_cone = TRUE, write = FALSE)
  expect_true(grepl("dual.*cone", out_dc$tex, ignore.case = TRUE))

  out_nb <- dgme_tikz_me_nominal(bl, show_bank = FALSE, write = FALSE)
  expect_false(grepl("bank counterposition", out_nb$tex))
})

test_that("dgme_tikz_na_real handles single-household panels", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")
  bl  <- res$results$baseline

  out_h1 <- dgme_tikz_na_real(bl, households = 1, write = FALSE)
  out_h2 <- dgme_tikz_na_real(bl, households = 2, write = FALSE)
  expect_true(grepl("\\\\begin\\{tikzpicture\\}", out_h1$tex))
  expect_true(grepl("\\\\begin\\{tikzpicture\\}", out_h2$tex))
})


# ---------------------------------------------------------------------------
# dgme_table
# ---------------------------------------------------------------------------

test_that("dgme_table produces parameter table", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")

  tex <- dgme_table(res, "parameters")
  expect_true(grepl("\\\\begin\\{table\\}", tex))
  expect_true(grepl("\\\\toprule", tex))
  expect_true(grepl("bond price", tex))

  df <- dgme_table(res, "parameters", format = "data.frame")
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) >= 9)
  expect_equal(ncol(df), 2)
})

test_that("dgme_table produces equilibrium table", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")

  df <- dgme_table(res, "equilibrium", format = "data.frame")
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) >= 16)
})

test_that("dgme_table produces comparison table", {
  par <- quiet_par("paper_example_1", rho = 0.1, eta = 0.8, S = 2)
  res <- dgme_solve(par, variants = c("baseline", "treasury"))

  df <- dgme_table(res, "comparison", format = "data.frame")
  expect_s3_class(df, "data.frame")
  expect_true("baseline" %in% names(df))
  expect_true("treasury" %in% names(df))
  expect_equal(nrow(df), 10)
})

test_that("dgme_table produces markdown output", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")

  md <- dgme_table(res, "parameters", format = "markdown")
  expect_true(grepl("^\\|", md))
  expect_true(grepl("---", md))
})

test_that("dgme_table produces indeterminacy table", {
  par <- quiet_par("paper_example_1",
                   m_S = c(0.5, 0.5), e_grid = seq(0.5, 2, by = 0.5))
  res <- dgme_solve(par, variants = "competing")

  df <- dgme_table(res, "indeterminacy", format = "data.frame")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 4)
  expect_equal(ncol(df), 7)
})

test_that("dgme_table parameter table includes variant-specific params", {
  par_t <- quiet_par("paper_example_1", rho = 0.1, eta = 0.8, S = 2)
  res_t <- dgme_solve(par_t, variants = "baseline")
  df_t <- dgme_table(res_t, "parameters", format = "data.frame")
  expect_true(any(grepl("rho", df_t$Parameter)))
  expect_true(nrow(df_t) >= 13)

  par_c <- quiet_par("paper_example_1", m_S = c(0.5, 0.5))
  res_c <- dgme_solve(par_c, variants = "baseline")
  df_c <- dgme_table(res_c, "parameters", format = "data.frame")
  expect_true(any(grepl("stablecoin", df_c$Parameter)))
})

test_that("dgme_table errors on indeterminacy without competing variant", {
  par <- quiet_par("paper_example_1")
  res <- dgme_solve(par, variants = "baseline")
  expect_error(dgme_table(res, "indeterminacy"), "competing")
})
