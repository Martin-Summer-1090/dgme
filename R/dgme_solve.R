#' Compute Monetary Equilibria
#'
#' Compute monetary equilibria for one or more model variants given a parameter
#' set.  This is the computational core of the package.
#'
#' @section Numeraire convention:
#' All prices \code{p} are expressed in \strong{absolute money units}
#' (outside money is the numeraire).  No normalization (such as
#' \eqn{p_1 = 1} or \eqn{\|p\|_1 = 1}) is applied.  The absolute price
#' level is pinned down by the aggregate outside-money supply \eqn{M} via
#' the flow-of-funds identity \eqn{M = (1-q) D}.
#'
#' @section Solver strategy:
#' The solver finds goods prices \code{p} such that excess demand
#' \eqn{z(p) = \sum_h x^h(p) - \sum_h e^h = 0}.  It uses a three-tier
#' cascade: (1) Newton's method from a heuristic starting value,
#' (2) Broyden's method as fallback, (3) Newton from a grid of rescaled
#' starting values.  A post-solution consistency check
#' (\code{\link{dgme_check}}) verifies all equilibrium conditions.
#'
#' @param params A \code{dgme_params} object from \code{\link{dgme_parametrize}}.
#' @param variants Character.  Which variants to solve: one or more of
#'   \code{"baseline"}, \code{"substitution"}, \code{"treasury"},
#'   \code{"competing"}, or \code{"all"} (default).  Variants whose required
#'   parameters are \code{NULL} in \code{params} are silently skipped.
#' @param tol Numeric.  Convergence tolerance for the nonlinear solver.
#'   Default \code{1e-8}.
#' @param check_tol Numeric.  Tolerance for the post-solution consistency
#'   check.  Default \code{1e-6} (slightly more relaxed than solver tol).
#' @param maxit Integer.  Maximum iterations.
#' @param check Logical.  If \code{TRUE} (default), run
#'   \code{\link{dgme_check}} on each solved variant and store the result.
#' @param ... Additional arguments passed to the solver.
#' @importFrom nleqslv nleqslv
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
                       tol = 1e-8,
                       check_tol = 1e-6,
                       maxit = 1000,
                       check = TRUE,
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
      baseline     = .solve_baseline(params, tol, maxit, ...),
      substitution = .solve_substitution(params, tol, maxit, ...),
      treasury     = .solve_treasury(params, tol, maxit, ...),
      competing    = .solve_competing(params, tol, maxit, ...)
    )
    if (!is.null(res)) {
      # Run post-solution consistency check
      if (check) {
        res$check <- dgme_check(res, tol = check_tol, verbose = FALSE)
      }
      results[[v]] <- res
    }
  }

  if (length(results) == 0L) {
    warning("No variants could be solved (check that the required ",
            "parameters are set).")
  }

  structure(
    list(
      params          = params,
      results         = results,
      variants_solved = names(results)
    ),
    class = "dgme_results"
  )
}


# =============================================================================
# BASELINE SOLVER
# =============================================================================

#' Solve the baseline DG monetary equilibrium (2 households, 2 goods)
#'
#' All prices are in absolute money units (money is the numeraire).
#' No normalization is applied to p.
#'
#' @keywords internal
.solve_baseline <- function(params, tol, maxit, ...) {

  e     <- params$e
  alpha <- params$alpha
  m     <- params$m
  q     <- params$q
  M     <- sum(m)

  # Use same tolerance for regime boundary detection as solver
  regime_tol <- tol

  # --- Household demand function ----------------------------------------------
  # For given prices (p1, p2) in money units, compute optimal consumption
  # for household h under the collapsed NA constraint:
  #   p . tau^+ - q * p . tau^- <= m^h

  hh_demand <- function(p, h) {
    .cobb_douglas_demand(p, e[h, ], alpha[h], m[h], q, regime_tol)
  }

  # --- Excess demand ----------------------------------------------------------
  # z(p) = sum_h x^h(p) - sum_h e^h.  By Walras' law one equation is
  # redundant, but solving both helps nleqslv with numerical stability.
  excess_demand <- function(p) {
    if (any(p <= 0)) return(c(1e6, 1e6))  # guard against negative prices
    x1 <- hh_demand(p, 1)
    x2 <- hh_demand(p, 2)
    (x1 + x2) - colSums(e)
  }

  # --- Starting values --------------------------------------------------------
  # Heuristic: rough Walrasian relative price from average Cobb-Douglas
  # preferences, then scale absolute level using flow-of-funds identity
  # M = (1-q)D and the approximation p . total ~ D.
  total <- colSums(e)
  agg_alpha <- mean(alpha)
  p2_init <- (agg_alpha / (1 - agg_alpha)) * (total[1] / total[2])
  scale_factor <- 2 * M / ((1 - q) * (total[1] + p2_init * total[2]))
  p_init <- c(1, p2_init) * scale_factor
  p_init <- pmax(p_init, 0.01)  # ensure positive

  # --- Solve: three-tier cascade ----------------------------------------------

  # Tier 1: Newton
  sol <- nleqslv::nleqslv(
    x       = p_init,
    fn      = excess_demand,
    method  = "Newton",
    control = list(ftol = tol, xtol = tol, maxit = maxit)
  )

  # Tier 2: Broyden fallback
  if (sol$termcd != 1) {
    sol2 <- nleqslv::nleqslv(
      x       = p_init,
      fn      = excess_demand,
      method  = "Broyden",
      control = list(ftol = tol, xtol = tol, maxit = maxit)
    )
    if (sol2$termcd == 1 ||
        max(abs(sol2$fvec)) < max(abs(sol$fvec))) {
      sol <- sol2
    }
  }

  # Tier 3: Newton from rescaled starting values
  if (sol$termcd != 1) {
    best_sol <- sol
    for (s in c(0.5, 2, 5, 0.1, 10)) {
      trial <- nleqslv::nleqslv(
        x       = p_init * s,
        fn      = excess_demand,
        method  = "Newton",
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

  # Net nominal income transfers (money flows at each stage)
  # nu0: cash remaining at t=0 after purchases and borrowing
  # nu1: cash at t=1 after sales revenue and debt repayment
  # With monotone preferences both should be zero (all cash spent).
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

  # Flow-of-funds residual (should be ~0)
  fof_check <- abs(M - (1 - q) * D_eq)

  # Utilities: reported in level form x1^alpha * x2^(1-alpha).
  # Note: demands are computed by maximising the monotonically equivalent
  # log form alpha*log(x1) + (1-alpha)*log(x2), which yields the same
  # optimal allocation.  Level form is more intuitive for display.
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
      variant           = "baseline",
      params            = params,
      p                 = p_eq,
      x                 = x_eq,
      b                 = b_eq,
      tau               = tau_eq,
      nu                = nu_eq,
      D                 = D_eq,
      payment_capacity  = payment_capacity,
      utilities         = utilities,
      autarky_utilities = autarky_utilities,
      fof_residual      = fof_check,
      s                 = NULL,
      b_S               = NULL,
      D_S               = NULL,
      sigma             = NULL,
      check             = NULL,
      converged         = converged,
      solver_info       = list(
        iterations = sol$iter,
        tolerance  = max(abs(sol$fvec)),
        tol_used   = tol,
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
#' The collapsed NA budget set is kinked: for each sign pattern of the net
#' trade vector \eqn{\tau^h = x^h - e^h}, the constraint is linear with
#' different effective prices.  Purchases cost \eqn{p} per unit; sales yield
#' only \eqn{q \cdot p} per unit (discounted by the state price).
#'
#' This function evaluates the Cobb-Douglas optimum on each linear face,
#' checks which regime is consistent with the assumed sign pattern, and
#' returns the consistent solution.  Near kink boundaries, a fallback picks
#' the regime with the smallest inconsistency.
#'
#' @param p Price vector (length 2), in money units.
#' @param eh Endowment vector (length 2).
#' @param alpha_h Cobb-Douglas exponent for good 1.
#' @param mh Outside-money endowment.
#' @param q Bond price (state price \eqn{\pi_1}).
#' @param regime_tol Tolerance for regime boundary detection.
#' @return Optimal consumption vector (length 2).
#' @keywords internal
.cobb_douglas_demand <- function(p, eh, alpha_h, mh, q, regime_tol = 1e-8) {

  # --- Regime S1B2: sell good 1, buy good 2 ---
  # Sign pattern: x1 < e1, x2 > e2
  # Effective prices: good 1 valued at q*p1 (sale revenue discounted)
  # Income: q*p1*e1 + p2*e2 + m
  p_eff_s1b2 <- c(q * p[1], p[2])
  I_s1b2     <- sum(p_eff_s1b2 * eh) + mh
  x_s1b2     <- c(alpha_h * I_s1b2 / p_eff_s1b2[1],
                   (1 - alpha_h) * I_s1b2 / p_eff_s1b2[2])

  if (x_s1b2[1] < eh[1] + regime_tol && x_s1b2[2] > eh[2] - regime_tol) {
    return(x_s1b2)
  }

  # --- Regime B1S2: buy good 1, sell good 2 ---
  # Sign pattern: x1 > e1, x2 < e2
  # Effective prices: good 2 valued at q*p2 (sale revenue discounted)
  # Income: p1*e1 + q*p2*e2 + m
  p_eff_b1s2 <- c(p[1], q * p[2])
  I_b1s2     <- sum(p_eff_b1s2 * eh) + mh
  x_b1s2     <- c(alpha_h * I_b1s2 / p_eff_b1s2[1],
                   (1 - alpha_h) * I_b1s2 / p_eff_b1s2[2])

  if (x_b1s2[1] > eh[1] - regime_tol && x_b1s2[2] < eh[2] + regime_tol) {
    return(x_b1s2)
  }

  # --- Regime BB: buy both (pure cash spending, no sales revenue) ---
  # Sign pattern: x1 >= e1, x2 >= e2
  # Effective prices: full prices p (no discounting)
  # Income: p . e + m
  I_bb <- sum(p * eh) + mh
  x_bb <- c(alpha_h * I_bb / p[1],
             (1 - alpha_h) * I_bb / p[2])

  if (x_bb[1] >= eh[1] - regime_tol && x_bb[2] >= eh[2] - regime_tol) {
    return(x_bb)
  }

  # --- Fallback: pick regime with smallest inconsistency ---
  # Near kink boundaries, numerical imprecision can cause none of the
  # regimes to pass their consistency check exactly.  Pick the best.
  candidates <- list(x_s1b2, x_b1s2, x_bb)

  scores <- c(
    # S1B2 wants x1 < e1, x2 > e2: penalise violations
    max(x_s1b2[1] - eh[1], eh[2] - x_s1b2[2], 0),
    # B1S2 wants x1 > e1, x2 < e2: penalise violations
    max(eh[1] - x_b1s2[1], x_b1s2[2] - eh[2], 0),
    # BB wants x1 >= e1, x2 >= e2: penalise violations
    max(eh[1] - x_bb[1], eh[2] - x_bb[2], 0)
  )

  best <- which.min(scores)
  candidates[[best]]
}


# =============================================================================
# SUBSTITUTION VARIANT (identical to baseline by Proposition 1)
# =============================================================================

#' @keywords internal
.solve_substitution <- function(params, tol, maxit, ...) {
  # By Proposition: substitution variant yields identical equilibrium
  # to the baseline.  Solve baseline and relabel.
  res <- .solve_baseline(params, tol, maxit, ...)
  if (is.null(res)) return(NULL)
  res$variant <- "substitution"
  res
}


# =============================================================================
# TREASURY CHANNEL (Variant 2)
# =============================================================================
#
# The stablecoin issuer collects deposits s^h from households, retains a
# fraction rho as settlement media, and deploys (1-rho) into Treasury
# securities.  A fraction eta of Treasury borrowing flows back as spending
# G = eta*(1-rho)*S distributed to households via g_shares.
#
# With both goods in category "B" (the default), the split payment
# constraints (T-O) and (T-S) simplify: only total payment capacity
# matters.  Household h's effective money endowment becomes
#   m^h_eff = m^h + g^h     where g^h = g_shares[h] * G
# and the budget constraint is identical in structure to the baseline
# with m^h replaced by m^h_eff.  The key difference: aggregate money
# supply M_eff = M + G, so the price level rises (inflationary).
#
# With segmented categories (good 1 in "O", good 2 in "T" or vice versa),
# the two constraints bind separately and the demand function must handle
# the split.  This creates regime interactions beyond the baseline.

#' @keywords internal
.solve_treasury <- function(params, tol, maxit, ...) {
  if (is.null(params$rho) || is.null(params$eta) || is.null(params$S)) {
    message("Treasury variant: skipped (rho, eta, or S not set).")
    return(NULL)
  }

  rho  <- params$rho
  eta  <- params$eta
  S    <- params$S
  g_sh <- params$g_shares
  cats <- params$categories

  G <- eta * (1 - rho) * S
  g <- g_sh * G  # length-2: spending received by each household

  # Determine which solver branch to use based on category assignment
  cat_vals <- unlist(cats)
  all_B <- all(cat_vals == "B")

  if (all_B) {
    .solve_treasury_both_B(params, g, G, tol, maxit, ...)
  } else {
    .solve_treasury_segmented(params, g, G, tol, maxit, ...)
  }
}


#' Treasury solver: both goods in category B (fungible payment media)
#'
#' When both goods accept either payment medium, the split between
#' outside-money and token payments is indeterminate.  Only total
#' payment capacity matters.  This reduces to the baseline solver
#' with augmented money endowments m^h_eff = m^h + g^h.
#'
#' @keywords internal
.solve_treasury_both_B <- function(params, g, G, tol, maxit, ...) {

  e     <- params$e
  alpha <- params$alpha
  m     <- params$m
  q     <- params$q
  S     <- params$S
  rho   <- params$rho
  eta   <- params$eta

  # Effective money endowments: original + Treasury spending share
  m_eff <- m + g
  M_eff <- sum(m_eff)   # = M + G

  regime_tol <- tol

  # Household demand: same as baseline but with m_eff
  hh_demand <- function(p, h) {
    .cobb_douglas_demand(p, e[h, ], alpha[h], m_eff[h], q, regime_tol)
  }

  excess_demand <- function(p) {
    if (any(p <= 0)) return(c(1e6, 1e6))
    x1 <- hh_demand(p, 1)
    x2 <- hh_demand(p, 2)
    (x1 + x2) - colSums(e)
  }

  # Starting values (same heuristic as baseline, with M_eff)
  total <- colSums(e)
  agg_alpha <- mean(alpha)
  p2_init <- (agg_alpha / (1 - agg_alpha)) * (total[1] / total[2])
  scale_factor <- 2 * M_eff / ((1 - q) * (total[1] + p2_init * total[2]))
  p_init <- c(1, p2_init) * scale_factor
  p_init <- pmax(p_init, 0.01)

  # Three-tier cascade (same as baseline)
  sol <- nleqslv::nleqslv(
    x = p_init, fn = excess_demand, method = "Newton",
    control = list(ftol = tol, xtol = tol, maxit = maxit)
  )
  if (sol$termcd != 1) {
    sol2 <- nleqslv::nleqslv(
      x = p_init, fn = excess_demand, method = "Broyden",
      control = list(ftol = tol, xtol = tol, maxit = maxit)
    )
    if (sol2$termcd == 1 || max(abs(sol2$fvec)) < max(abs(sol$fvec)))
      sol <- sol2
  }
  if (sol$termcd != 1) {
    best_sol <- sol
    for (s in c(0.5, 2, 5, 0.1, 10)) {
      trial <- nleqslv::nleqslv(
        x = p_init * s, fn = excess_demand, method = "Newton",
        control = list(ftol = tol, xtol = tol, maxit = maxit)
      )
      if (trial$termcd == 1) { best_sol <- trial; break }
      if (max(abs(trial$fvec)) < max(abs(best_sol$fvec))) best_sol <- trial
    }
    sol <- best_sol
  }

  converged <- (sol$termcd == 1)
  if (!converged) {
    warning("Treasury (both B) solver did not converge (termination code ",
            sol$termcd, ", max |f| = ",
            format(max(abs(sol$fvec)), digits = 4), ").")
  }

  p_eq <- sol$x

  # Construct solution
  x1 <- hh_demand(p_eq, 1)
  x2 <- hh_demand(p_eq, 2)
  x_eq <- rbind(x1, x2)
  rownames(x_eq) <- c("h1", "h2")
  colnames(x_eq) <- c("good1", "good2")
  tau_eq <- x_eq - e

  # Borrowing (same logic as baseline)
  b_eq <- numeric(2)
  for (h in 1:2) {
    tau_minus <- pmax(-tau_eq[h, ], 0)
    b_eq[h] <- -sum(p_eq * tau_minus)
  }
  D_eq <- -sum(b_eq)

  # Token deposits: with both goods in B, the optimal deposit is
  # indeterminate.  A natural convention: each household deposits
  # proportionally to their share of aggregate issuance.
  s_eq <- (params$g_shares) * S

  # Nominal transfers
  nu_eq <- matrix(0, nrow = 2, ncol = 2)
  colnames(nu_eq) <- c("nu0", "nu1")
  rownames(nu_eq) <- c("h1", "h2")
  for (h in 1:2) {
    tau_plus  <- pmax(tau_eq[h, ], 0)
    tau_minus <- pmax(-tau_eq[h, ], 0)
    nu_eq[h, 1] <- m_eff[h] - q * b_eq[h] - sum(p_eq * tau_plus)
    nu_eq[h, 2] <- sum(p_eq * tau_minus) + b_eq[h]
  }

  # Payment capacity: M + qD + G (or equivalently M_eff + qD)
  payment_capacity <- M_eff + q * D_eq

  # Flow-of-funds: M_eff = (1-q)*D (aggregate absorption with augmented M)
  fof_check <- abs(M_eff - (1 - q) * D_eq)

  # Utilities
  utilities <- numeric(2)
  autarky_utilities <- numeric(2)
  for (h in 1:2) {
    utilities[h] <- x_eq[h, 1]^alpha[h] * x_eq[h, 2]^(1 - alpha[h])
    autarky_utilities[h] <- e[h, 1]^alpha[h] * e[h, 2]^(1 - alpha[h])
  }

  structure(
    list(
      variant           = "treasury",
      params            = params,
      p                 = p_eq,
      x                 = x_eq,
      b                 = b_eq,
      tau               = tau_eq,
      nu                = nu_eq,
      D                 = D_eq,
      payment_capacity  = payment_capacity,
      utilities         = utilities,
      autarky_utilities = autarky_utilities,
      fof_residual      = fof_check,
      s                 = s_eq,
      b_S               = NULL,
      D_S               = NULL,
      sigma             = NULL,
      G                 = G,
      g                 = g,
      m_eff             = m_eff,
      check             = NULL,
      converged         = converged,
      solver_info       = list(
        iterations = sol$iter,
        tolerance  = max(abs(sol$fvec)),
        tol_used   = tol,
        termcd     = sol$termcd
      )
    ),
    class = "dgme_result"
  )
}


#' Treasury solver: segmented categories (O/T/B split)
#'
#' When goods are assigned to different categories, the two payment
#' constraints (T-O) and (T-S) bind separately.  The household must
#' allocate outside money to category-O and B purchases, and tokens
#' to category-T and B purchases.
#'
#' With |L|=2, the interesting case is: good 1 in "O" (outside money
#' only), good 2 in "T" (tokens only), or one in "O"/"T" and the other
#' in "B".  We handle the general case.
#'
#' @keywords internal
.solve_treasury_segmented <- function(params, g, G, tol, maxit, ...) {

  e     <- params$e
  alpha <- params$alpha
  m     <- params$m
  q     <- params$q
  S     <- params$S
  rho   <- params$rho
  eta   <- params$eta
  cats  <- params$categories

  m_eff <- m + g
  M_eff <- sum(m_eff)

  # Parse category assignment
  cat1 <- cats$good1   # "O", "T", or "B"
  cat2 <- cats$good2

  regime_tol <- tol

  # Household demand under split constraints
  # For given prices p, household h maximises Cobb-Douglas utility
  # subject to:
  #   outside-money constraint: p_O . tau^{h,+}_O + p_B . tau^{h,+,m}_B
  #                             <= (m^h + g^h) - q*b^h
  #   token constraint:         p_T . tau^{h,+}_T + p_B . tau^{h,+,s}_B
  #                             <= s^h
  #
  # With |L|=2, at most one good is in each non-B category.
  # The key insight: with Cobb-Douglas preferences, the household
  # spends fraction alpha on good 1 and (1-alpha) on good 2.
  # The binding question is whether each good's spending can be
  # covered by its designated payment channel.

  hh_demand_segmented <- function(p, h) {

    ah   <- alpha[h]
    eh   <- e[h, ]
    mh   <- m_eff[h]   # already includes Treasury spending
    sh   <- (params$g_shares[h]) * S  # token deposit

    # Total effective budget under DG constraint (same kink logic)
    # First solve as if both goods were B (total budget matters)
    x_total <- .cobb_douglas_demand(p, eh, ah, mh, q, regime_tol)

    # Now check if the payment split is feasible
    tau_total <- x_total - eh
    tau_plus  <- pmax(tau_total, 0)

    # Cost of purchasing each good
    cost <- p * tau_plus  # cost[1] for good 1, cost[2] for good 2

    # Available in each channel:
    # Outside money channel: mh - q*b^h (where b^h comes from repayment)
    tau_minus <- pmax(-tau_total, 0)
    bh <- -sum(p * tau_minus)  # borrowing (negative)
    om_available <- mh - q * bh

    # For the segmented case, we need to check if the payment
    # channel for each good can cover its cost.
    # Good in "O": must be paid with outside money
    # Good in "T": must be paid with tokens
    # Good in "B": can use either

    om_needed <- 0  # how much outside money needed for O-goods
    tk_needed <- 0  # how much tokens needed for T-goods

    if (cat1 == "O") om_needed <- om_needed + cost[1]
    if (cat1 == "T") tk_needed <- tk_needed + cost[1]
    if (cat2 == "O") om_needed <- om_needed + cost[2]
    if (cat2 == "T") tk_needed <- tk_needed + cost[2]

    # If both channels have enough, the unconstrained optimum is feasible
    om_feasible <- (om_needed <= om_available + regime_tol)
    tk_feasible <- (tk_needed <= sh + regime_tol)

    if (om_feasible && tk_feasible) {
      return(x_total)
    }

    # If a channel is binding, we need to constrain spending.
    # Fall back to a constrained optimisation.
    # With Cobb-Douglas, if the token channel binds for good l_T:
    #   x_{l_T} = e_{l_T} + s^h / p_{l_T}
    # and remaining budget goes to the other good via outside money.

    if (!tk_feasible && tk_needed > 0) {
      # Token channel is binding
      # Identify which good is T-only
      if (cat1 == "T") {
        # Good 1 token-constrained: max spend on good 1 is s^h
        x1_max <- eh[1] + sh / p[1]
        # Remaining budget (outside money) goes to good 2
        remaining_om <- om_available
        x2 <- eh[2] + remaining_om / p[2]
        return(c(x1_max, max(x2, 0)))
      } else if (cat2 == "T") {
        x2_max <- eh[2] + sh / p[2]
        remaining_om <- om_available
        x1 <- eh[1] + remaining_om / p[1]
        return(c(max(x1, 0), x2_max))
      }
    }

    if (!om_feasible && om_needed > 0) {
      # Outside-money channel is binding
      if (cat1 == "O") {
        x1_max <- eh[1] + om_available / p[1]
        remaining_tk <- sh
        x2 <- eh[2] + remaining_tk / p[2]
        return(c(max(x1_max, 0), max(x2, 0)))
      } else if (cat2 == "O") {
        x2_max <- eh[2] + om_available / p[2]
        remaining_tk <- sh
        x1 <- eh[1] + remaining_tk / p[1]
        return(c(max(x1, 0), max(x2_max, 0)))
      }
    }

    # Default: return unconstrained solution
    x_total
  }

  # Excess demand
  excess_demand <- function(p) {
    if (any(p <= 0)) return(c(1e6, 1e6))
    x1 <- hh_demand_segmented(p, 1)
    x2 <- hh_demand_segmented(p, 2)
    (x1 + x2) - colSums(e)
  }

  # Starting values
  total <- colSums(e)
  agg_alpha <- mean(alpha)
  p2_init <- (agg_alpha / (1 - agg_alpha)) * (total[1] / total[2])
  scale_factor <- 2 * M_eff / ((1 - q) * (total[1] + p2_init * total[2]))
  p_init <- c(1, p2_init) * scale_factor
  p_init <- pmax(p_init, 0.01)

  # Three-tier cascade
  sol <- nleqslv::nleqslv(
    x = p_init, fn = excess_demand, method = "Newton",
    control = list(ftol = tol, xtol = tol, maxit = maxit)
  )
  if (sol$termcd != 1) {
    sol2 <- nleqslv::nleqslv(
      x = p_init, fn = excess_demand, method = "Broyden",
      control = list(ftol = tol, xtol = tol, maxit = maxit)
    )
    if (sol2$termcd == 1 || max(abs(sol2$fvec)) < max(abs(sol$fvec)))
      sol <- sol2
  }
  if (sol$termcd != 1) {
    best_sol <- sol
    for (s in c(0.5, 2, 5, 0.1, 10)) {
      trial <- nleqslv::nleqslv(
        x = p_init * s, fn = excess_demand, method = "Newton",
        control = list(ftol = tol, xtol = tol, maxit = maxit)
      )
      if (trial$termcd == 1) { best_sol <- trial; break }
      if (max(abs(trial$fvec)) < max(abs(best_sol$fvec))) best_sol <- trial
    }
    sol <- best_sol
  }

  converged <- (sol$termcd == 1)
  if (!converged) {
    warning("Treasury (segmented) solver did not converge (termination code ",
            sol$termcd, ", max |f| = ",
            format(max(abs(sol$fvec)), digits = 4), ").")
  }

  p_eq <- sol$x

  # Construct solution
  x1 <- hh_demand_segmented(p_eq, 1)
  x2 <- hh_demand_segmented(p_eq, 2)
  x_eq <- rbind(x1, x2)
  rownames(x_eq) <- c("h1", "h2")
  colnames(x_eq) <- c("good1", "good2")
  tau_eq <- x_eq - e

  b_eq <- numeric(2)
  for (h in 1:2) {
    tau_minus <- pmax(-tau_eq[h, ], 0)
    b_eq[h] <- -sum(p_eq * tau_minus)
  }
  D_eq <- -sum(b_eq)

  s_eq <- params$g_shares * S

  nu_eq <- matrix(0, nrow = 2, ncol = 2)
  colnames(nu_eq) <- c("nu0", "nu1")
  rownames(nu_eq) <- c("h1", "h2")
  for (h in 1:2) {
    tau_plus  <- pmax(tau_eq[h, ], 0)
    tau_minus <- pmax(-tau_eq[h, ], 0)
    nu_eq[h, 1] <- m_eff[h] - q * b_eq[h] - sum(p_eq * tau_plus)
    nu_eq[h, 2] <- sum(p_eq * tau_minus) + b_eq[h]
  }

  payment_capacity <- M_eff + q * D_eq
  fof_check <- abs(M_eff - (1 - q) * D_eq)

  utilities <- numeric(2)
  autarky_utilities <- numeric(2)
  for (h in 1:2) {
    utilities[h] <- x_eq[h, 1]^alpha[h] * x_eq[h, 2]^(1 - alpha[h])
    autarky_utilities[h] <- e[h, 1]^alpha[h] * e[h, 2]^(1 - alpha[h])
  }

  structure(
    list(
      variant           = "treasury",
      params            = params,
      p                 = p_eq,
      x                 = x_eq,
      b                 = b_eq,
      tau               = tau_eq,
      nu                = nu_eq,
      D                 = D_eq,
      payment_capacity  = payment_capacity,
      utilities         = utilities,
      autarky_utilities = autarky_utilities,
      fof_residual      = fof_check,
      s                 = s_eq,
      b_S               = NULL,
      D_S               = NULL,
      sigma             = NULL,
      G                 = G,
      g                 = g,
      m_eff             = m_eff,
      check             = NULL,
      converged         = converged,
      solver_info       = list(
        iterations = sol$iter,
        tolerance  = max(abs(sol$fvec)),
        tol_used   = tol,
        termcd     = sol$termcd
      )
    ),
    class = "dgme_result"
  )
}


# =============================================================================
# COMPETING OUTSIDE MONEY (Variant 4)
# =============================================================================
#
# The stablecoin is a parallel outside money with its own credit circuit.
# An exchange rate e_bar > 0 (stablecoins per sovereign money unit) is
# a new equilibrium object, but real allocations are independent of e_bar.
#
# Aggregation trick (from the spec):
#   For any fixed e_bar, define sovereign-equivalent magnitudes:
#     m_tilde^h = m^h + m_S^h / e_bar
#   Then solve the baseline DG model with m_tilde and M_tilde = M + M_S/e_bar.
#   The composition into two circuits is determined ex post.
#
# The solver sweeps over the e_grid to illustrate the indeterminacy:
# different nominal prices, same real allocations.

#' @keywords internal
.solve_competing <- function(params, tol, maxit, ...) {
  if (is.null(params$m_S)) {
    message("Competing variant: skipped (m_S not set).")
    return(NULL)
  }

  m     <- params$m
  m_S   <- params$m_S
  M     <- sum(m)
  M_S   <- sum(m_S)
  e     <- params$e
  alpha <- params$alpha
  q     <- params$q

  # Exchange rate grid
  e_grid <- params$e_grid
  if (is.null(e_grid)) {
    e_grid <- seq(0.5, 2.0, by = 0.1)
  }

  # --- Solve for the reference allocation at e_bar = 1 -----------------------
  # (Real allocations are the same for all e_bar, so solve once to get x, tau)

  m_tilde_ref <- m + m_S  # e_bar = 1
  params_ref <- params
  params_ref$m <- m_tilde_ref

  ref_result <- .solve_baseline(params_ref, tol, maxit, ...)
  if (is.null(ref_result) || !ref_result$converged) {
    warning("Competing variant: baseline solver failed at reference e_bar = 1.")
    return(NULL)
  }

  # Reference real allocation (invariant to e_bar)
  x_ref   <- ref_result$x
  tau_ref <- ref_result$tau

  # --- Sweep over e_grid to show indeterminacy -------------------------------
  # For each e_bar: compute sovereign-equivalent money, solve for prices,
  # decompose into two circuits.

  n_grid <- length(e_grid)
  p_matrix <- matrix(NA, nrow = n_grid, ncol = 2)
  colnames(p_matrix) <- c("p1", "p2")

  # Per-e_bar details: borrowing, credit in each circuit
  b_list   <- vector("list", n_grid)
  b_S_list <- vector("list", n_grid)
  D_vec    <- numeric(n_grid)
  D_S_vec  <- numeric(n_grid)
  sigma_list <- vector("list", n_grid)
  converged_all <- TRUE

  regime_tol <- tol

  for (i in seq_len(n_grid)) {
    e_bar <- e_grid[i]

    # Sovereign-equivalent money
    m_tilde <- m + m_S / e_bar
    M_tilde <- sum(m_tilde)

    # Solve baseline with m_tilde
    params_i <- params
    params_i$m <- m_tilde

    res_i <- .solve_baseline(params_i, tol, maxit, ...)

    if (is.null(res_i) || !res_i$converged) {
      warning("Competing variant: solver failed at e_bar = ", e_bar)
      converged_all <- FALSE
      p_matrix[i, ] <- NA
      b_list[[i]]   <- c(NA, NA)
      b_S_list[[i]] <- c(NA, NA)
      D_vec[i]      <- NA
      D_S_vec[i]    <- NA
      sigma_list[[i]] <- c(NA, NA)
      next
    }

    p_matrix[i, ] <- res_i$p

    # --- Decompose into two circuits ---
    # Total borrowing b_tilde^h = b^h + b^h_S / e_bar
    # From the baseline solution: b_tilde^h = res_i$b[h]
    # Need to split into sovereign (b^h) and stablecoin (b^h_S).
    #
    # Flow-of-funds conditions:
    #   M       = (1-q) * D       (sovereign circuit)
    #   M_S     = (1-q) * D_S     (stablecoin circuit)
    # These determine aggregate credit in each circuit:
    D_sov <- M / (1 - q)
    D_sc  <- M_S / (1 - q)

    D_vec[i]   <- D_sov
    D_S_vec[i] <- D_sc

    # Individual decomposition: proportional to money endowments
    # sigma^h: share of h's sales revenue received in sovereign money
    # A natural assumption: sigma^h = m^h / m_tilde^h
    sigma_h <- m / m_tilde
    sigma_list[[i]] <- sigma_h

    # Individual borrowing decomposition (proportional)
    b_tilde <- res_i$b  # total borrowing per household
    b_sov   <- b_tilde * (m / m_tilde)
    b_sc    <- b_tilde * (m_S / (e_bar * m_tilde))

    b_list[[i]]   <- b_sov
    b_S_list[[i]] <- b_sc * e_bar  # convert back to stablecoin units
  }

  # Use the reference solution for the canonical allocation
  # (pick the grid point closest to e_bar = 1 for the main result)
  ref_idx <- which.min(abs(e_grid - 1))

  # Nominal transfers at reference e_bar
  p_ref <- p_matrix[ref_idx, ]
  nu_eq <- matrix(0, nrow = 2, ncol = 2)
  colnames(nu_eq) <- c("nu0", "nu1")
  rownames(nu_eq) <- c("h1", "h2")
  m_tilde_at_ref <- m + m_S / e_grid[ref_idx]
  for (h in 1:2) {
    tau_plus  <- pmax(tau_ref[h, ], 0)
    tau_minus <- pmax(-tau_ref[h, ], 0)
    bh <- -(b_list[[ref_idx]][h] + b_S_list[[ref_idx]][h] / e_grid[ref_idx])
    nu_eq[h, 1] <- m_tilde_at_ref[h] + q * bh - sum(p_ref * tau_plus)
    nu_eq[h, 2] <- sum(p_ref * tau_minus) - bh
  }

  M_tilde_ref <- sum(m_tilde_at_ref)
  D_ref <- D_vec[ref_idx] + D_S_vec[ref_idx] / e_grid[ref_idx]
  payment_capacity <- M_tilde_ref + q * D_ref

  fof_check <- abs(M_tilde_ref - (1 - q) * D_ref)

  utilities <- numeric(2)
  autarky_utilities <- numeric(2)
  for (h in 1:2) {
    utilities[h] <- x_ref[h, 1]^alpha[h] * x_ref[h, 2]^(1 - alpha[h])
    autarky_utilities[h] <- e[h, 1]^alpha[h] * e[h, 2]^(1 - alpha[h])
  }

  structure(
    list(
      variant           = "competing",
      params            = params,
      p                 = p_ref,
      x                 = x_ref,
      b                 = b_list[[ref_idx]],
      tau               = tau_ref,
      nu                = nu_eq,
      D                 = D_vec[ref_idx],
      payment_capacity  = payment_capacity,
      utilities         = utilities,
      autarky_utilities = autarky_utilities,
      fof_residual      = fof_check,
      s                 = NULL,
      b_S               = b_S_list[[ref_idx]],
      D_S               = D_S_vec[ref_idx],
      sigma             = sigma_list[[ref_idx]],
      # Grid results for indeterminacy analysis
      e_grid            = e_grid,
      p_grid            = p_matrix,
      D_grid            = D_vec,
      D_S_grid          = D_S_vec,
      b_grid            = b_list,
      b_S_grid          = b_S_list,
      sigma_grid        = sigma_list,
      check             = NULL,
      converged         = converged_all,
      solver_info       = list(
        n_grid     = n_grid,
        n_failed   = sum(is.na(D_vec)),
        tol_used   = tol
      )
    ),
    class = "dgme_result"
  )
}


# =============================================================================
# PRINT / SUMMARY METHODS
# =============================================================================

#' @export
print.dgme_result <- function(x, digits = 4, ...) {
  cat("DG Monetary Equilibrium (", x$variant, ")\n", sep = "")
  cat("Label:", x$params$label, "\n")
  cat("Converged:", x$converged, "\n\n")

  cat("Prices (money units): p = (",
      paste(round(x$p, digits), collapse = ", "), ")\n")
  cat("Interest rate: r =",
      round((1 - x$params$q) / x$params$q, digits), "\n\n")

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

  # Show consistency check summary if available
  if (!is.null(x$check)) {
    if (x$check$passed) {
      cat("Consistency check: all ", x$check$n_passed, " checks passed\n")
    } else {
      cat("Consistency check: ", x$check$n_failed, " of ",
          x$check$n_passed + x$check$n_failed, " checks FAILED\n")
    }
  }

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
    chk_status <- if (!is.null(r$check)) {
      if (r$check$passed) "OK" else "FAIL"
    } else {
      "n/a"
    }
    cat(sprintf("  %-15s  conv=%-5s  chk=%-4s  p=(%.3f, %.3f)  D=%.3f  PC=%.3f\n",
                v, r$converged, chk_status, r$p[1], r$p[2],
                r$D, r$payment_capacity))
  }
  invisible(object)
}
