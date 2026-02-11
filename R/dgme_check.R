#' Post-Solution Consistency Check for a DG Monetary Equilibrium
#'
#' Given a \code{dgme_result} object, verify that the computed equilibrium
#' satisfies all theoretical conditions.  This is a diagnostic tool, not
#' part of the solver itself.  It catches both numerical issues (solver
#' didn't quite converge) and implementation errors (regime selection,
#' budget construction).
#'
#' The following conditions are checked:
#' \enumerate{
#'   \item \strong{Goods market clearing.}
#'     \eqn{\sum_h x^h_\ell = \sum_h e^h_\ell} for each good \eqn{\ell}.
#'   \item \strong{Flow of funds.}
#'     \eqn{M = (1-q) D} where \eqn{M = \sum_h m^h} and
#'     \eqn{D = \sum_h (-b^h)}.
#'   \item \strong{Budget constraint satisfaction.}
#'     Each household's consumption satisfies the collapsed NA constraint:
#'     \eqn{p \cdot \tau^{h,+} - q \, p \cdot \tau^{h,-} \le m^h}.
#'   \item \strong{Non-negativity of consumption.}
#'     \eqn{x^h_\ell \ge 0} for all households and goods.
#'   \item \strong{Non-negativity of nominal transfers.}
#'     \eqn{\nu^h_0 \ge 0} and \eqn{\nu^h_1 \ge 0} (no unspent-cash
#'     violations with monotone preferences).
#'   \item \strong{Household optimality.}
#'     No household can improve utility by switching to a different trading
#'     regime at the equilibrium prices.
#'   \item \strong{Gains from trade.}
#'     Each household weakly prefers its equilibrium allocation to autarky.
#' }
#'
#' @param result A \code{dgme_result} object.
#' @param tol Numeric.  Tolerance for numerical checks.  Default \code{1e-6}.
#' @param verbose Logical.  If \code{TRUE}, print diagnostic messages for
#'   each check.  Default \code{FALSE}.
#'
#' @return An S3 object of class \code{dgme_check} (invisibly), a list with:
#'   \describe{
#'     \item{\code{passed}}{Logical: \code{TRUE} if all checks pass.}
#'     \item{\code{checks}}{A named list of individual check results, each
#'       with \code{ok} (logical), \code{value} (the diagnostic quantity),
#'       and \code{message} (human-readable description).}
#'     \item{\code{n_passed}}{Integer: number of checks passed.}
#'     \item{\code{n_failed}}{Integer: number of checks failed.}
#'   }
#'
#' @export
#'
#' @examples
#' par <- dgme_parametrize("paper_example_1")
#' res <- dgme_solve(par)
#' chk <- dgme_check(res$results$baseline)
#' print(chk)
dgme_check <- function(result, tol = 1e-6, verbose = FALSE) {

  stopifnot(inherits(result, "dgme_result"))

  params <- result$params
  p   <- result$p
  x   <- result$x
  e   <- params$e
  m   <- params$m
  q   <- params$q
  tau <- result$tau
  b   <- result$b
  nu  <- result$nu
  M   <- sum(m)
  D   <- result$D

  alpha <- params$alpha

  checks <- list()

  # ---- 1. Goods market clearing ----------------------------------------------

  excess <- colSums(x) - colSums(e)
  max_excess <- max(abs(excess))
  checks$market_clearing <- list(
    ok      = max_excess < tol,
    value   = excess,
    message = sprintf("Max |excess demand| = %.2e (tol = %.2e)",
                      max_excess, tol)
  )

  # ---- 2. Flow of funds: M = (1-q) * D --------------------------------------
  #
  # For baseline/substitution: M = (1-q)*D
  # For treasury:              M_eff = M + G = (1-q)*D  (augmented money)
  # For competing:             M_tilde = (1-q)*D_tilde  (aggregate circuit)

  if (result$variant == "treasury" && !is.null(result$m_eff)) {
    M_check <- sum(result$m_eff)
  } else if (result$variant == "competing" && !is.null(params$m_S)) {
    # At the reference exchange rate, effective money is m + m_S / e_bar
    e_bar <- if (!is.null(result$e_grid)) {
      result$e_grid[which.min(abs(result$e_grid - 1))]
    } else 1
    M_check <- M + sum(params$m_S) / e_bar
  } else {
    M_check <- M
  }

  # For competing, D in the result is the sovereign-circuit credit only;
  # total credit is D + D_S / e_bar
  D_check <- if (result$variant == "competing" && !is.null(result$D_S)) {
    e_bar <- if (!is.null(result$e_grid)) {
      result$e_grid[which.min(abs(result$e_grid - 1))]
    } else 1
    D + result$D_S / e_bar
  } else {
    D
  }

  fof_gap <- abs(M_check - (1 - q) * D_check)
  checks$flow_of_funds <- list(
    ok      = fof_gap < tol,
    value   = fof_gap,
    message = sprintf("|M - (1-q)D| = %.2e (tol = %.2e)", fof_gap, tol)
  )

  # ---- 3. Budget constraint satisfaction -------------------------------------
  #
  # For each household: p . tau^+ - q * p . tau^- <= m^h
  # (should hold with equality for monotone preferences)
  # For the treasury variant, m^h is replaced by m^h_eff = m^h + g^h.
  # For the competing variant, m^h is replaced by m^h + m^h_S / e_bar.

  if (result$variant == "treasury" && !is.null(result$m_eff)) {
    m_budget <- result$m_eff
  } else if (result$variant == "competing" && !is.null(params$m_S)) {
    e_bar <- if (!is.null(result$e_grid)) {
      result$e_grid[which.min(abs(result$e_grid - 1))]
    } else 1
    m_budget <- m + params$m_S / e_bar
  } else {
    m_budget <- m
  }

  budget_resid <- numeric(2)
  for (h in 1:2) {
    tau_plus  <- pmax(tau[h, ], 0)
    tau_minus <- pmax(-tau[h, ], 0)
    lhs <- sum(p * tau_plus) - q * sum(p * tau_minus)
    budget_resid[h] <- lhs - m_budget[h]
  }
  max_budget_violation <- max(budget_resid)
  # With monotone preferences, lhs should equal m^h (binding).
  # A positive residual means spending exceeds budget (violation).
  # A negative residual means slack (possible but unlikely with monotone u).
  checks$budget_constraint <- list(
    ok      = max_budget_violation < tol,
    value   = budget_resid,
    message = sprintf("Max budget violation = %.2e (tol = %.2e)",
                      max_budget_violation, tol)
  )

  # ---- 4. Non-negativity of consumption --------------------------------------

  min_x <- min(x)
  checks$consumption_nonneg <- list(
    ok      = min_x > -tol,
    value   = min_x,
    message = sprintf("Min consumption = %.6f", min_x)
  )

  # ---- 5. Non-negativity of nominal transfers --------------------------------
  #
  # nu0 = m^h - q*b^h - p . tau^+  (cash left at t=0 after purchases)
  # nu1 = p . tau^- + b^h          (cash at t=1 after sales and repayment)
  # Both should be >= 0 with monotone preferences (actually = 0).

  min_nu <- min(nu)
  checks$nominal_transfers_nonneg <- list(
    ok      = min_nu > -tol,
    value   = nu,
    message = sprintf("Min nominal transfer = %.2e (tol = %.2e)",
                      min_nu, tol)
  )

  # ---- 6. Household optimality -----------------------------------------------
  #
  # Check that no household can improve by switching to a different trading
  # regime at the equilibrium prices.  For each household, compute the
  # Cobb-Douglas optimum on every face of the kinked budget and verify
  # that the equilibrium allocation is (weakly) best.

  u_log <- function(xv, ah) {
    ah * log(max(xv[1], 1e-15)) + (1 - ah) * log(max(xv[2], 1e-15))
  }

  optimality_gaps <- numeric(2)
  for (h in 1:2) {
    u_eq <- u_log(x[h, ], alpha[h])

    # Compute demand on each face and its utility
    best_alt <- u_eq
    eh <- e[h, ]; mh <- m[h]; ah <- alpha[h]

    # Face S1B2: sell good 1, buy good 2
    p_eff <- c(q * p[1], p[2])
    I_eff <- sum(p_eff * eh) + mh
    x_alt <- c(ah * I_eff / p_eff[1], (1 - ah) * I_eff / p_eff[2])
    if (x_alt[1] <= eh[1] + tol && x_alt[2] >= eh[2] - tol &&
        all(x_alt > 0)) {
      best_alt <- max(best_alt, u_log(x_alt, ah))
    }

    # Face B1S2: buy good 1, sell good 2
    p_eff <- c(p[1], q * p[2])
    I_eff <- sum(p_eff * eh) + mh
    x_alt <- c(ah * I_eff / p_eff[1], (1 - ah) * I_eff / p_eff[2])
    if (x_alt[1] >= eh[1] - tol && x_alt[2] <= eh[2] + tol &&
        all(x_alt > 0)) {
      best_alt <- max(best_alt, u_log(x_alt, ah))
    }

    # Face BB: buy both
    I_eff <- sum(p * eh) + mh
    x_alt <- c(ah * I_eff / p[1], (1 - ah) * I_eff / p[2])
    if (x_alt[1] >= eh[1] - tol && x_alt[2] >= eh[2] - tol &&
        all(x_alt > 0)) {
      best_alt <- max(best_alt, u_log(x_alt, ah))
    }

    # Corner: spend all money on good 1
    x_corner1 <- c(eh[1] + mh / p[1], eh[2])
    if (all(x_corner1 > 0)) {
      best_alt <- max(best_alt, u_log(x_corner1, ah))
    }

    # Corner: spend all money on good 2
    x_corner2 <- c(eh[1], eh[2] + mh / p[2])
    if (all(x_corner2 > 0)) {
      best_alt <- max(best_alt, u_log(x_corner2, ah))
    }

    # Autarky (no trade)
    best_alt <- max(best_alt, u_log(eh, ah))

    optimality_gaps[h] <- best_alt - u_eq
  }

  max_opt_gap <- max(optimality_gaps)
  checks$optimality <- list(
    ok      = max_opt_gap < tol,
    value   = optimality_gaps,
    message = sprintf(
      "Max utility gap vs. alternatives = %.2e (tol = %.2e)",
      max_opt_gap, tol)
  )

  # ---- 7. Gains from trade ---------------------------------------------------
  #
  # Each household should (weakly) prefer equilibrium to autarky.

  u_eq_vec <- numeric(2)
  u_aut_vec <- numeric(2)
  for (h in 1:2) {
    u_eq_vec[h]  <- u_log(x[h, ], alpha[h])
    u_aut_vec[h] <- u_log(e[h, ], alpha[h])
  }
  gft_gaps <- u_eq_vec - u_aut_vec  # should be >= 0

  checks$gains_from_trade <- list(
    ok      = all(gft_gaps > -tol),
    value   = gft_gaps,
    message = sprintf("Utility gains: h1 = %.6f, h2 = %.6f",
                      gft_gaps[1], gft_gaps[2])
  )

  # ---- Assemble result -------------------------------------------------------

  n_passed <- sum(vapply(checks, function(c) c$ok, logical(1)))
  n_failed <- length(checks) - n_passed

  if (verbose || n_failed > 0) {
    for (nm in names(checks)) {
      ch <- checks[[nm]]
      status <- if (ch$ok) "PASS" else "FAIL"
      if (verbose || !ch$ok) {
        message(sprintf("  [%s] %s: %s", status, nm, ch$message))
      }
    }
  }

  if (n_failed > 0) {
    warning(n_failed, " of ", length(checks),
            " consistency check(s) failed. Use print() for details.")
  }

  structure(
    list(
      passed   = (n_failed == 0),
      checks   = checks,
      n_passed = n_passed,
      n_failed = n_failed,
      variant  = result$variant
    ),
    class = "dgme_check"
  )
}


# ---- Print method ------------------------------------------------------------

#' @export
print.dgme_check <- function(x, ...) {
  cat("DG Equilibrium Consistency Check (", x$variant, ")\n", sep = "")
  cat(strrep("-", 55), "\n")

  for (nm in names(x$checks)) {
    ch <- x$checks[[nm]]
    status <- if (ch$ok) "\u2713" else "\u2717"
    cat(sprintf("  %s  %-28s %s\n", status, nm, ch$message))
  }

  cat(strrep("-", 55), "\n")
  cat(sprintf("Result: %d/%d passed",
              x$n_passed, x$n_passed + x$n_failed))
  if (x$passed) {
    cat(" \u2014 all checks OK\n")
  } else {
    cat(sprintf(" \u2014 %d FAILED\n", x$n_failed))
  }
  invisible(x)
}
