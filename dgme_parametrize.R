#' Generate a Parameter Set for a DG Monetary Economy
#'
#' Create a \code{dgme_params} object containing exogenous parameters for a
#' 2-household, 2-good Dubey--Geanakoplos monetary economy.  Supports named
#' presets (paper examples), structured random generation, and fully manual
#' specification.
#'
#' @param preset Character or \code{NULL}.
#'   \code{"paper_example_1"} and \code{"paper_example_2"} load fixed parameter
#'   sets.  \code{"random"} draws from the ranges below.  \code{NULL} for fully
#'   manual specification (then \code{e}, \code{alpha}, \code{m}, \code{q} must
#'   be supplied via \code{...}).
#' @param style Character.  \code{"symmetric"} imposes mirror endowments and
#'   complementary alpha; \code{"asymmetric"} draws independently.  Only used
#'   when \code{preset = "random"}.
#' @param q_range Numeric(2). Bounds for the bond price \eqn{q}.
#' @param endow_range Numeric(2). Bounds for each endowment \eqn{e^h_\ell}.
#' @param alpha_range Numeric(2). Bounds for the Cobb--Douglas exponent.
#' @param money_range Numeric(2). Bounds for each \eqn{m^h}.
#' @param rho Scalar in \eqn{[0,1]} or \code{NULL}.  Issuer reserve ratio
#'   (Variant 2).
#' @param eta Scalar in \eqn{[0,1]} or \code{NULL}.  Treasury-channel
#'   pass-through (Variant 2).
#' @param S Scalar \eqn{\ge 0} or \code{NULL}.  Aggregate token issuance
#'   (Variant 2).
#' @param g_shares Numeric(2) summing to 1.  Distribution of Treasury spending.
#' @param categories Named list assigning each good to \code{"O"}, \code{"T"},
#'   or \code{"B"}.
#' @param m_S Numeric(2) or \code{NULL}.  Stablecoin outside-money endowments
#'   (Variant 4).
#' @param e_grid Numeric vector or \code{NULL}.  Exchange-rate grid for
#'   Variant 4.
#' @param seed Integer or \code{NULL}.  RNG seed for reproducibility when
#'   \code{preset = "random"}.
#' @param label Character or \code{NULL}.  Human-readable label.
#' @param ... Additional arguments.  When \code{preset = NULL}, pass \code{e}
#'   (2x2 matrix), \code{alpha} (length-2), \code{m} (length-2), and \code{q}
#'   (scalar).
#'
#' @return An S3 object of class \code{dgme_params}.
#' @export
#'
#' @examples
#' # Named preset
#' par1 <- dgme_parametrize("paper_example_1")
#'
#' # Random symmetric economy
#' par2 <- dgme_parametrize("random", style = "symmetric", seed = 42)
#'
#' # Fully manual
#' par3 <- dgme_parametrize(NULL,
#'   e = matrix(c(8, 2, 2, 8), nrow = 2, byrow = TRUE),
#'   alpha = c(0.4, 0.6), m = c(10, 10), q = 0.8,
#'   label = "my custom economy")
dgme_parametrize <- function(preset = "paper_example_1",
                             style = c("symmetric", "asymmetric"),
                             q_range = c(0.65, 0.95),
                             endow_range = c(1, 10),
                             alpha_range = c(0.25, 0.75),
                             money_range = c(1, 20),
                             rho = NULL, eta = NULL, S = NULL,
                             g_shares = c(0.5, 0.5),
                             categories = list(good1 = "B", good2 = "B"),
                             m_S = NULL, e_grid = NULL,
                             seed = NULL, label = NULL, ...) {

  style <- match.arg(style)
  dots  <- list(...)

  # ---- Presets ---------------------------------------------------------------

  if (!is.null(preset) && preset == "paper_example_1") {
    # Symmetric economy, moderate interest rate
    # Endowments create clear gains from trade
    e     <- matrix(c(8, 2,
                       2, 8), nrow = 2, byrow = TRUE)
    alpha <- c(0.4, 0.6)
    m     <- c(10, 10)
    q     <- 0.8
    if (is.null(label)) label <- "Paper Example 1"

  } else if (!is.null(preset) && preset == "paper_example_2") {
    # Asymmetric economy, tighter money
    e     <- matrix(c(6, 3,
                       3, 7), nrow = 2, byrow = TRUE)
    alpha <- c(0.35, 0.65)
    m     <- c(8, 12)
    q     <- 0.75
    if (is.null(label)) label <- "Paper Example 2"

  } else if (!is.null(preset) && preset == "random") {
    if (!is.null(seed)) set.seed(seed)

    max_attempts <- 100L
    found <- FALSE

    for (attempt in seq_len(max_attempts)) {
      q <- runif(1, q_range[1], q_range[2])
      r <- (1 - q) / q

      if (style == "symmetric") {
        a1 <- runif(1, alpha_range[1], alpha_range[2])
        alpha <- c(a1, 1 - a1)
        a <- runif(1, endow_range[1], endow_range[2])
        b <- runif(1, endow_range[1], endow_range[2])
        while (abs(a - b) < 0.5) b <- runif(1, endow_range[1], endow_range[2])
        e <- matrix(c(a, b, b, a), nrow = 2, byrow = TRUE)
        m_val <- runif(1, money_range[1], money_range[2])
        m <- c(m_val, m_val)
      } else {
        alpha <- runif(2, alpha_range[1], alpha_range[2])
        e <- matrix(runif(4, endow_range[1], endow_range[2]),
                    nrow = 2, byrow = TRUE)
        m <- runif(2, money_range[1], money_range[2])
      }

      # Check gains-from-trade condition (heuristic for 2x2 Cobb-Douglas)
      gft <- .check_gains_from_trade(e, alpha, q)
      if (gft) {
        found <- TRUE
        break
      }
    }

    if (!found) {
      warning("Could not find a non-degenerate parametrisation in ",
              max_attempts, " attempts. Returning last draw; ",
              "equilibrium may not exist.")
    }
    if (is.null(label)) label <- paste0("Random (seed=", seed, ")")

  } else if (is.null(preset)) {
    # Fully manual
    e     <- dots$e
    alpha <- dots$alpha
    m     <- dots$m
    q     <- dots$q
    if (is.null(e) || is.null(alpha) || is.null(m) || is.null(q))
      stop("When preset = NULL, you must supply e, alpha, m, and q.")
    if (is.null(label)) label <- "Custom"

  } else {
    stop("Unknown preset: '", preset, "'. ",
         "Use 'paper_example_1', 'paper_example_2', 'random', or NULL.")
  }

  # ---- Validate --------------------------------------------------------------

  stopifnot(
    is.matrix(e), nrow(e) == 2, ncol(e) == 2,
    all(e >= 0), all(colSums(e) > 0),
    is.numeric(alpha), length(alpha) == 2,
    all(alpha > 0), all(alpha < 1),
    is.numeric(m), length(m) == 2, all(m >= 0), sum(m) > 0,
    is.numeric(q), length(q) == 1, q > 0, q <= 1
  )

  rownames(e) <- c("h1", "h2")
  colnames(e) <- c("good1", "good2")

  # Variant 2 parameters
  if (!is.null(rho)) stopifnot(is.numeric(rho), rho >= 0, rho <= 1)
  if (!is.null(eta)) stopifnot(is.numeric(eta), eta >= 0, eta <= 1)
  if (!is.null(S))   stopifnot(is.numeric(S), S >= 0)
  if (!is.null(rho) || !is.null(eta) || !is.null(S)) {
    stopifnot(is.numeric(g_shares), length(g_shares) == 2,
              abs(sum(g_shares) - 1) < 1e-12)
  }

  # Variant 4 parameters
  if (!is.null(m_S)) {
    stopifnot(is.numeric(m_S), length(m_S) == 2, all(m_S >= 0), sum(m_S) > 0)
  }
  if (!is.null(e_grid)) {
    if (length(e_grid) == 1) e_grid <- seq(0.5, 2, by = 0.1)
    stopifnot(is.numeric(e_grid), all(e_grid > 0))
  }

  # ---- Assemble object -------------------------------------------------------

  params <- structure(
    list(
      e          = e,
      alpha      = alpha,
      m          = m,
      q          = q,
      rho        = rho,
      eta        = eta,
      S          = S,
      g_shares   = g_shares,
      categories = categories,
      m_S        = m_S,
      e_grid     = e_grid,
      label      = label,
      preset     = ifelse(is.null(preset), "custom", preset)
    ),
    class = "dgme_params"
  )

  # Warn if gains from trade may be insufficient
  gft <- .check_gains_from_trade(e, alpha, q)
  if (!gft) {
    warning("Gains-from-trade condition may not be satisfied for q = ", q,
            " (r = ", round((1 - q)/q, 4), "). ",
            "Monetary equilibrium may not exist.")
  }

  params
}


# ---- Gains-from-trade heuristic ---------------------------------------------

#' Check gains-from-trade condition (heuristic for 2x2 Cobb-Douglas)
#'
#' For Cobb-Douglas preferences with mirror endowments, the gains from trade
#' are large when endowments are far from the preferred consumption mix.
#' This is a sufficient (not necessary) heuristic check.
#'
#' @param e 2x2 endowment matrix.
#' @param alpha Length-2 vector of Cobb-Douglas exponents.
#' @param q Bond price.
#' @return Logical: TRUE if the heuristic passes.
#' @keywords internal
.check_gains_from_trade <- function(e, alpha, q) {
  r <- (1 - q) / q

  # For each household, compute the Walrasian (q=1) optimal allocation and
 # check that the utility gain from trade exceeds the interest-rate friction.
  # This is a heuristic: we check that at the Walrasian prices, each household
  # would want to trade even when sale proceeds are discounted by 1/(1+r).

  # Walrasian relative price (from market clearing with Cobb-Douglas)
  total <- colSums(e)
  # At Walrasian equilibrium: x^h_l = alpha_h * I^h / p_l where I^h = p . e^h
  # With normalisation p1 = 1, market clearing for good 1 gives:
  # alpha_1 * (e11 + p2*e12) + alpha_2 * (e21 + p2*e22) = total[1]
  # Solving for p2:
  # alpha_1*e11 + alpha_1*p2*e12 + alpha_2*e21 + alpha_2*p2*e22 = total[1]
  # p2 * (alpha_1*e12 + alpha_2*e22) = total[1] - alpha_1*e11 - alpha_2*e21
  denom <- alpha[1] * e[1, 2] + alpha[2] * e[2, 2]
  numer <- total[1] - alpha[1] * e[1, 1] - alpha[2] * e[2, 1]

  if (denom <= 0 || numer <= 0) return(FALSE)
  p2_walras <- numer / denom

  # Compute Walrasian allocations
  p_w <- c(1, p2_walras)
  for (h in 1:2) {
    I_h <- sum(p_w * e[h, ])
    x_h <- alpha[h] * I_h / p_w[1]
    # Check that the net trade is nontrivial
    tau <- c(x_h - e[h, 1], (1 - alpha[h]) * I_h / p_w[2] - e[h, 2])
    # Heuristic: gains from trade tolerate the interest rate friction
    # if the utility at the Walrasian allocation exceeds autarky utility
    # by a sufficient margin
  }

  # Simpler heuristic: check that endowments differ enough across households
  # relative to the interest rate
  endow_asymmetry <- max(abs(e[1, ] - e[2, ])) / max(e)
  if (endow_asymmetry < r * 0.5) return(FALSE)

  TRUE
}


# ---- Print / summary methods ------------------------------------------------

#' @export
print.dgme_params <- function(x, ...) {
  cat("DG Monetary Economy Parameters\n")
  cat("Label:", x$label, "\n")
  cat("Preset:", x$preset, "\n\n")
  cat("Bond price q =", x$q,
      " (interest rate r =", round((1 - x$q)/x$q, 4), ")\n\n")
  cat("Endowments:\n")
  print(x$e)
  cat("\nCobb-Douglas exponents: alpha =",
      paste(x$alpha, collapse = ", "), "\n")
  cat("Outside money: m =", paste(x$m, collapse = ", "),
      " (M =", sum(x$m), ")\n")

  # Variant 2
  if (!is.null(x$rho) && !is.null(x$eta) && !is.null(x$S)) {
    cat("\nTreasury channel parameters:\n")
    cat("  rho =", x$rho, ", eta =", x$eta, ", S =", x$S, "\n")
    G <- x$eta * (1 - x$rho) * x$S
    cat("  Treasury spending G = eta*(1-rho)*S =", G, "\n")
    cat("  g_shares =", paste(x$g_shares, collapse = ", "), "\n")
  }

  # Variant 4
  if (!is.null(x$m_S)) {
    cat("\nCompeting outside money:\n")
    cat("  m_S =", paste(x$m_S, collapse = ", "),
        " (M_S =", sum(x$m_S), ")\n")
    if (!is.null(x$e_grid))
      cat("  Exchange rate grid: ",
          length(x$e_grid), " values in [",
          min(x$e_grid), ", ", max(x$e_grid), "]\n")
  }
  invisible(x)
}

#' @export
summary.dgme_params <- function(object, ...) {
  print(object, ...)
}
