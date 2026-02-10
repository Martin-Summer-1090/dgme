#' Compute Monetary Equilibria
#'
#' Compute monetary equilibria for one or more model variants given a parameter
#' set.  This is the computational core of the package.
#'
#' @param params A \code{dgme_params} object from \code{\link{dgme_parametrize}}.
#' @param variants Character.  Which variants to solve: one or more of
#'   \code{"baseline"}, \code{"substitution"}, \code{"treasury"},
#'   \code{"competing"}, or \code{"all"} (default).  Variants whose required
#'   parameters are \code{NULL} in \code{params} are silently skipped.
#' @param tol Numeric.  Convergence tolerance for the nonlinear solver.
#' @param maxit Integer.  Maximum iterations.
#' @param method Character.  Solver back-end.  Default \code{"nleqslv"}.
#' @param ... Additional arguments passed to the solver.
#'
#' @return An S3 object of class \code{dgme_results} containing the shared
#'   \code{params} and a named list of \code{dgme_result} objects.
#' @export
#'
#' @examples
#' par <- dgme_parametrize("paper_example_1")
#' res <- dgme_solve(par)
#' summary(res)
dgme_solve <- function(params,
                       variants = "all",
                       tol = 1e-10,
                       maxit = 1000,
                       method = "nleqslv",
                       ...) {

  stopifnot(inherits(params, "dgme_params"))

  # Resolve variant list
  all_variants <- c("baseline", "substitution", "treasury", "competing")
  if (identical(variants, "all")) {
    variants <- all_variants
  } else {
    variants <- match.arg(variants, all_variants, several.ok = TRUE)
  }

  results <- list()

  for (v in variants) {
    res <- switch(v,
      baseline     = .solve_baseline(params, tol, maxit, method, ...),
      substitution = .solve_substitution(params, tol, maxit, method, ...),
      treasury     = .solve_treasury(params, tol, maxit, method, ...),
      competing    = .solve_competing(params, tol, maxit, method, ...)
    )
    if (!is.null(res)) {
      results[[v]] <- res
    }
  }

  if (length(results) == 0L) {
    warning("No variants could be solved (check that the required ",
            "parameters are set).")
  }

  structure(
    list(
      params         = params,
      results        = results,
      variants_solved = names(results)
    ),
    class = "dgme_results"
  )
}


# =============================================================================
# BASELINE SOLVER
# =============================================================================

#' Solve the baseline DG monetary equilibrium (2 households, 2 goods)
#' @keywords internal
.solve_baseline <- function(params, tol, maxit, method, ...) {

  e     <- params$e
  alpha <- params$alpha
  m     <- params$m
  q     <- params$q
  M     <- sum(m)

  # --- Household demand function ----------------------------------------------
  # For given prices (p1, p2), compute optimal consumption for household h
  # under the collapsed constraint: p . tau^+ - q * p . tau^- <= m^h

  hh_demand <- function(p, h) {
    .cobb_douglas_demand(p, e[h, ], alpha[h], m[h], q)
  }

  # --- Excess demand ----------------------------------------------------------
  excess_demand <- function(p) {
    if (any(p <= 0)) return(c(1e6, 1e6))  # guard
    x1 <- hh_demand(p, 1)
    x2 <- hh_demand(p, 2)
    z <- (x1 + x2) - colSums(e)
    z
  }

  # --- Starting values --------------------------------------------------------
  # Use a heuristic: at the monetary equilibrium, the price level scales
  # roughly with M.  Start with prices proportional to relative scarcity.
  total <- colSums(e)
  # Rough Walrasian relative price from Cobb-Douglas aggregation
  agg_alpha <- mean(alpha)
  p2_init <- (agg_alpha / (1 - agg_alpha)) * (total[1] / total[2])
  # Scale so that nominal value of endowments is commensurate with M + qD
  # Rough guess: p . total ≈ 2 * M / (1-q) (from M = (1-q)*D and p.T ≈ D)
  scale_factor <- 2 * M / ((1 - q) * (total[1] + p2_init * total[2]))
  p_init <- c(1, p2_init) * scale_factor
  p_init <- pmax(p_init, 0.01)  # ensure positive

  # --- Solve ------------------------------------------------------------------
  sol <- nleqslv::nleqslv(
    x      = p_init,
    fn     = excess_demand,
    method = "Newton",
    control = list(ftol = tol, xtol = tol, maxit = maxit)
  )

  # If Newton fails, try Broyden
 if (sol$termcd != 1) {
    sol2 <- nleqslv::nleqslv(
      x      = p_init,
      fn     = excess_demand,
      method = "Broyden",
      control = list(ftol = tol, xtol = tol, maxit = maxit)
    )
    if (sol2$termcd == 1 ||
        max(abs(sol2$fvec)) < max(abs(sol$fvec))) {
      sol <- sol2
    }
  }

  # Try a grid of starting values if still not converged
  if (sol$termcd != 1) {
    best_sol <- sol
    for (s in c(0.5, 2, 5, 0.1, 10)) {
      trial <- nleqslv::nleqslv(
        x = p_init * s,
        fn = excess_demand,
        method = "Newton",
        control = list(ftol = tol, xtol = tol, maxit = maxit)
      )
      if (trial$termcd == 1) {
        best_sol <- trial
        break
      }
      if (max(abs(trial$fvec)) < max(abs(best_sol$fvec))) {
        best_sol <- trial
      }
    }
    sol <- best_sol
  }

  converged <- (sol$termcd == 1)
  if (!converged) {
    warning("Baseline solver did not converge (termination code ",
            sol$termcd, ", max |f| = ",
            format(max(abs(sol$fvec)), digits = 4), ").")
  }

  p_eq <- sol$x

  # --- Construct solution -----------------------------------------------------
  x1 <- hh_demand(p_eq, 1)
  x2 <- hh_demand(p_eq, 2)
  x_eq <- rbind(x1, x2)
  rownames(x_eq) <- c("h1", "h2")
  colnames(x_eq) <- c("good1", "good2")

  tau_eq <- x_eq - e

  # Borrowing: from binding repayment constraint, -b^h = p . tau^{h,-}
  b_eq <- numeric(2)
  for (h in 1:2) {
    tau_minus <- pmax(-tau_eq[h, ], 0)  # sales
    b_eq[h] <- -sum(p_eq * tau_minus)
  }
  D_eq <- -sum(b_eq)

  # Net nominal income transfers
  nu_eq <- matrix(0, nrow = 2, ncol = 2)
  colnames(nu_eq) <- c("nu0", "nu1")
  rownames(nu_eq) <- c("h1", "h2")
  for (h in 1:2) {
    tau_plus  <- pmax(tau_eq[h, ], 0)
    tau_minus <- pmax(-tau_eq[h, ], 0)
    nu_eq[h, 1] <- m[h] - q * b_eq[h] - sum(p_eq * tau_plus)
    nu_eq[h, 2] <- sum(p_eq * tau_minus) + b_eq[h]
  }

  payment_capacity <- M + q * D_eq

  # Verify flow of funds
  fof_check <- abs(M - (1 - q) * D_eq)

  # Utilities
  utilities <- numeric(2)
  for (h in 1:2) {
    utilities[h] <- x_eq[h, 1]^alpha[h] * x_eq[h, 2]^(1 - alpha[h])
  }
  autarky_utilities <- numeric(2)
  for (h in 1:2) {
    autarky_utilities[h] <- e[h, 1]^alpha[h] * e[h, 2]^(1 - alpha[h])
  }

  structure(
    list(
      variant          = "baseline",
      params           = params,
      p                = p_eq,
      x                = x_eq,
      b                = b_eq,
      tau              = tau_eq,
      nu               = nu_eq,
      D                = D_eq,
      payment_capacity = payment_capacity,
      utilities        = utilities,
      autarky_utilities = autarky_utilities,
      fof_residual     = fof_check,
      s                = NULL,
      b_S              = NULL,
      D_S              = NULL,
      sigma            = NULL,
      converged        = converged,
      solver_info      = list(
        iterations = sol$iter,
        tolerance  = max(abs(sol$fvec)),
        method     = method,
        termcd     = sol$termcd
      )
    ),
    class = "dgme_result"
  )
}


# =============================================================================
# COBB-DOUGLAS DEMAND UNDER DG CONSTRAINTS
# =============================================================================

#' Compute optimal consumption for a Cobb-Douglas household under the DG
#' collapsed budget constraint.
#'
#' Determines the trading regime (which goods are bought/sold), then computes
#' the analytical Cobb-Douglas demand at the effective prices.
#'
#' @param p Price vector (length 2).
#' @param eh Endowment vector (length 2).
#' @param alpha_h Cobb-Douglas exponent for good 1.
#' @param mh Outside-money endowment.
#' @param q Bond price.
#' @return Optimal consumption vector (length 2).
#' @keywords internal
.cobb_douglas_demand <- function(p, eh, alpha_h, mh, q) {

  # Compute demand under each candidate regime and check consistency.
  # The correct regime is the one where the demands are consistent with
  # the assumed trading pattern.

  # --- Regime S1B2: sell good 1, buy good 2 ---
  p_eff_s1b2 <- c(q * p[1], p[2])
  I_s1b2     <- sum(p_eff_s1b2 * eh) + mh
  x_s1b2     <- c(alpha_h * I_s1b2 / p_eff_s1b2[1],
                   (1 - alpha_h) * I_s1b2 / p_eff_s1b2[2])

  if (x_s1b2[1] < eh[1] && x_s1b2[2] > eh[2]) {
    return(x_s1b2)
  }

  # --- Regime B1S2: buy good 1, sell good 2 ---
  p_eff_b1s2 <- c(p[1], q * p[2])
  I_b1s2     <- sum(p_eff_b1s2 * eh) + mh
  x_b1s2     <- c(alpha_h * I_b1s2 / p_eff_b1s2[1],
                   (1 - alpha_h) * I_b1s2 / p_eff_b1s2[2])

  if (x_b1s2[1] > eh[1] && x_b1s2[2] < eh[2]) {
    return(x_b1s2)
  }

  # --- Regime BB: buy both (no borrowing) ---
  # Effective prices at full rate (no discounting), income = p . e + m
  I_bb <- sum(p * eh) + mh
  x_bb <- c(alpha_h * I_bb / p[1],
             (1 - alpha_h) * I_bb / p[2])

  if (x_bb[1] >= eh[1] - 1e-12 && x_bb[2] >= eh[2] - 1e-12) {
    return(x_bb)
  }

  # --- Regime SS: sell both (corner case, household only receives money) ---
  # This happens when the household wants less of both goods than it has.
  # With strictly increasing preferences this shouldn't happen, but handle it:
  # The household would sell both goods: p.tau^{h,+} = 0, tau^{h,-} = e - x
  # Collapsed: -q * p . (e - x) = m, i.e., q * p . x = q * p . e + m
  # But this is the same as BB with effective prices (q*p1, q*p2)
  # which simplifies to the same relative allocation as BB.

  # --- Fallback: pick the regime with the smallest inconsistency ---
  # This handles numerical edge cases near kink points.
  candidates <- list(
    list(x = x_s1b2, name = "S1B2"),
    list(x = x_b1s2, name = "B1S2"),
    list(x = x_bb,   name = "BB")
  )

  # Score by consistency: how close are the demands to satisfying the regime
  scores <- c(
    max(x_s1b2[1] - eh[1], eh[2] - x_s1b2[2], 0),  # want x1<e1, x2>e2
    max(eh[1] - x_b1s2[1], x_b1s2[2] - eh[2], 0),  # want x1>e1, x2<e2
    max(eh[1] - x_bb[1], eh[2] - x_bb[2], 0)        # want x1>=e1, x2>=e2
  )

  best <- which.min(scores)
  return(candidates[[best]]$x)
}


# =============================================================================
# SUBSTITUTION VARIANT (identical to baseline by Proposition 1)
# =============================================================================

#' @keywords internal
.solve_substitution <- function(params, tol, maxit, method, ...) {
  # By Proposition: substitution variant yields identical equilibrium
  # to the baseline. Solve baseline and relabel.
  res <- .solve_baseline(params, tol, maxit, method, ...)
  if (is.null(res)) return(NULL)
  res$variant <- "substitution"
  res
}


# =============================================================================
# TREASURY CHANNEL (Variant 2) — stub for future implementation
# =============================================================================

#' @keywords internal
.solve_treasury <- function(params, tol, maxit, method, ...) {
  if (is.null(params$rho) || is.null(params$eta) || is.null(params$S)) {
    message("Treasury variant: skipped (rho, eta, or S not set).")
    return(NULL)
  }
  message("Treasury variant: not yet implemented (coming in v0.2.0).")
  NULL
}


# =============================================================================
# COMPETING OUTSIDE MONEY (Variant 4) — stub for future implementation
# =============================================================================

#' @keywords internal
.solve_competing <- function(params, tol, maxit, method, ...) {
  if (is.null(params$m_S)) {
    message("Competing variant: skipped (m_S not set).")
    return(NULL)
  }
  message("Competing variant: not yet implemented (coming in v0.2.0).")
  NULL
}


# =============================================================================
# PRINT / SUMMARY METHODS
# =============================================================================

#' @export
print.dgme_result <- function(x, digits = 4, ...) {
  cat("DG Monetary Equilibrium (", x$variant, ")\n", sep = "")
  cat("Label:", x$params$label, "\n")
  cat("Converged:", x$converged, "\n\n")

  cat("Prices: p = (", paste(round(x$p, digits), collapse = ", "), ")\n")
  cat("Interest rate: r =", round((1 - x$params$q) / x$params$q, digits), "\n\n")

  cat("Consumption allocations:\n")
  print(round(x$x, digits))

  cat("\nNet trades:\n")
  print(round(x$tau, digits))

  cat("\nBorrowing: b = (", paste(round(x$b, digits), collapse = ", "), ")\n")
  cat("Aggregate credit: D =", round(x$D, digits), "\n")
  cat("Payment capacity: M + qD =", round(x$payment_capacity, digits), "\n")

  cat("\nNet nominal income transfers:\n")
  print(round(x$nu, digits))

  cat("\nUtilities (equilibrium):", paste(round(x$utilities, digits),
                                         collapse = ", "), "\n")
  cat("Utilities (autarky):   ", paste(round(x$autarky_utilities, digits),
                                         collapse = ", "), "\n")

  fof <- x$fof_residual
  cat("\nFlow-of-funds check: |M - (1-q)D| =",
      format(fof, digits = 2, scientific = TRUE), "\n")

  invisible(x)
}

#' @export
summary.dgme_result <- function(object, ...) {
  print(object, ...)
}

#' @export
print.dgme_results <- function(x, ...) {
  cat("DG Monetary Economy: ", x$params$label, "\n")
  cat("Variants solved:", paste(x$variants_solved, collapse = ", "), "\n\n")
  for (v in x$variants_solved) {
    cat(strrep("-", 60), "\n")
    print(x$results[[v]], ...)
    cat("\n")
  }
  invisible(x)
}

#' @export
summary.dgme_results <- function(object, ...) {
  cat("DG Monetary Economy: ", object$params$label, "\n")
  cat("Variants solved:", paste(object$variants_solved, collapse = ", "), "\n\n")
  cat("Summary across variants:\n\n")
  for (v in object$variants_solved) {
    r <- object$results[[v]]
    cat(sprintf("  %-15s  converged=%-5s  p=(%.3f, %.3f)  D=%.3f  PC=%.3f\n",
                v, r$converged, r$p[1], r$p[2], r$D, r$payment_capacity))
  }
  invisible(object)
}
