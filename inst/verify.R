#!/usr/bin/env Rscript
# ============================================================================
# dgme package: verification script
# Run after:  R CMD INSTALL dgme
# ============================================================================

cat("=== dgme package verification ===\n\n")

library(dgme)

# --------------------------------------------------------------------------
# 1. Paper Example 1: symmetric economy
# --------------------------------------------------------------------------
cat("--- Paper Example 1 ---\n")
par1 <- dgme_parametrize("paper_example_1")
print(par1)

res1 <- dgme_solve(par1, variants = "baseline")
bl1  <- res1$results$baseline
print(bl1)

# Checks
stopifnot(bl1$converged)
stopifnot(max(abs(colSums(bl1$tau))) < 1e-8)  # market clearing
M1 <- sum(par1$m)
stopifnot(abs(M1 - (1 - par1$q) * bl1$D) < 1e-6)  # flow of funds
stopifnot(all(bl1$x > 0))
stopifnot(all(bl1$b <= 1e-10))
stopifnot(all(bl1$nu >= -1e-8))
stopifnot(all(bl1$utilities > bl1$autarky_utilities))
cat("  All checks passed.\n\n")

# --------------------------------------------------------------------------
# 2. Paper Example 2: asymmetric economy
# --------------------------------------------------------------------------
cat("--- Paper Example 2 ---\n")
par2 <- dgme_parametrize("paper_example_2")
res2 <- dgme_solve(par2, variants = "baseline")
bl2  <- res2$results$baseline
print(bl2)

stopifnot(bl2$converged)
stopifnot(max(abs(colSums(bl2$tau))) < 1e-8)
M2 <- sum(par2$m)
stopifnot(abs(M2 - (1 - par2$q) * bl2$D) < 1e-6)
cat("  All checks passed.\n\n")

# --------------------------------------------------------------------------
# 3. Substitution variant = baseline (Proposition 1)
# --------------------------------------------------------------------------
cat("--- Substitution = Baseline ---\n")
res_both <- dgme_solve(par1, variants = c("baseline", "substitution"))
bl  <- res_both$results$baseline
sub <- res_both$results$substitution
stopifnot(max(abs(bl$p - sub$p)) < 1e-8)
stopifnot(max(abs(bl$x - sub$x)) < 1e-8)
cat("  Substitution variant identical to baseline. Check passed.\n\n")

# --------------------------------------------------------------------------
# 4. Comparative statics: vary q
# --------------------------------------------------------------------------
cat("--- Comparative statics: varying q ---\n")
q_grid <- seq(0.6, 0.95, by = 0.05)
results_q <- data.frame(
  q = q_grid,
  r = (1 - q_grid) / q_grid,
  p1 = NA_real_, p2 = NA_real_,
  D  = NA_real_, PC = NA_real_,
  u1 = NA_real_, u2 = NA_real_
)

for (i in seq_along(q_grid)) {
  par_i <- dgme_parametrize(NULL,
    e = par1$e, alpha = par1$alpha, m = par1$m, q = q_grid[i])
  res_i <- dgme_solve(par_i, variants = "baseline")
  bl_i  <- res_i$results$baseline
  if (bl_i$converged) {
    results_q$p1[i] <- bl_i$p[1]
    results_q$p2[i] <- bl_i$p[2]
    results_q$D[i]  <- bl_i$D
    results_q$PC[i] <- bl_i$payment_capacity
    results_q$u1[i] <- bl_i$utilities[1]
    results_q$u2[i] <- bl_i$utilities[2]
  }
}

cat("  q       r      p1      p2       D       PC      u1      u2\n")
for (i in seq_len(nrow(results_q))) {
  cat(sprintf("  %.2f  %6.3f  %6.3f  %6.3f  %7.3f  %7.3f  %6.3f  %6.3f\n",
    results_q$q[i], results_q$r[i],
    results_q$p1[i], results_q$p2[i],
    results_q$D[i], results_q$PC[i],
    results_q$u1[i], results_q$u2[i]))
}

cat("\n--- Comparative statics: as q -> 1 (r -> 0), economy approaches Walrasian ---\n")
cat("  Note: D increases, prices rise, but utilities also increase\n")
cat("  (friction from interest rate diminishes).\n\n")

# --------------------------------------------------------------------------
# 5. Random economies
# --------------------------------------------------------------------------
cat("--- Testing random economies ---\n")
n_pass <- 0
n_fail <- 0
for (s in 1:20) {
  par_r <- dgme_parametrize("random", style = "symmetric", seed = s)
  res_r <- dgme_solve(par_r, variants = "baseline")
  bl_r  <- res_r$results$baseline
  if (bl_r$converged && max(abs(colSums(bl_r$tau))) < 1e-6) {
    n_pass <- n_pass + 1
  } else {
    n_fail <- n_fail + 1
    cat("  FAIL: seed =", s, "\n")
  }
}
cat(sprintf("  %d / %d random symmetric economies solved successfully.\n\n",
            n_pass, n_pass + n_fail))

# --------------------------------------------------------------------------
# 6. Summary
# --------------------------------------------------------------------------
cat("=== All verification checks completed. ===\n")
