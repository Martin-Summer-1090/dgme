#' Generate a Parameter Set for a DG Monetary Economy
#'
#' Create a \code{dgme_params} object containing exogenous parameters for a
#' 2-household, 2-good Dubey--Geanakoplos monetary economy.  Supports named
#' presets (paper examples), structured random generation, and fully manual
#' specification.
#'
#' @section Computational design:
#' This function is a \strong{factory} that constructs a validated,
#' self-contained data object representing all exogenous parameters of a
#' Dubey--Geanakoplos monetary economy.  It separates the
#' \emph{specification} of an economy from the \emph{computation} of its
#' equilibrium (handled by \code{\link{dgme_solve}}).
#'
#' The construction follows a three-stage pipeline:
#' \enumerate{
#'   \item \strong{Dispatch and populate.}
#'     Depending on the input mode (named preset, random draw, or manual
#'     specification), fill in the primitive parameters: the 2\eqn{\times}2
#'     endowment matrix \code{e}, Cobb--Douglas exponents \code{alpha},
#'     outside-money endowments \code{m}, and the bond price \code{q}
#'     (equivalently \eqn{\pi_1}).
#'     For random generation this includes a rejection-sampling loop that
#'     draws candidate economies and filters for a gains-from-trade
#'     condition to ensure the parameterisation is likely to support a
#'     non-degenerate monetary equilibrium.
#'   \item \strong{Validate.}
#'     Apply domain constraints (matrix dimensions, positivity, bounds on
#'     \code{alpha} and \code{q}, etc.) via \code{.validate_dgme_params()}
#'     to guarantee downstream solvers receive well-formed input.
#'   \item \strong{Assemble and diagnose.}
#'     Pack everything into an S3 object of class \code{dgme_params} and
#'     run a final gains-from-trade heuristic as a diagnostic warning.
#' }
#'
#' The function handles both the \strong{baseline model} (parameters
#' \code{e}, \code{alpha}, \code{m}, \code{q}) and \strong{variant
#' extensions}: the treasury/reserve-deployment channel (Variant 2) via
#' \code{rho}, \code{eta}, \code{S}; and competing settlement assets
#' (Variant 4) via \code{m_S}, \code{e_grid}.
#'
#' @param preset Character or \code{NULL}.
#'   \code{"paper_example_1"} loads the canonical example used for the
#'   paper's TikZ figures.  \code{"paper_example_2"} loads an asymmetric
#'   variant.  \code{"random"} draws from the ranges below.  \code{NULL}
#'   for fully manual specification (then \code{e}, \code{alpha}, \code{m},
#'   \code{q} must be supplied).
#' @param style Character.  \code{"symmetric"} imposes mirror endowments and
#'   complementary alpha; \code{"asymmetric"} draws independently.  Only used
#'   when \code{preset = "random"}.
#' @param e A 2x2 endowment matrix (rows = households, cols = goods).
#'   Required when \code{preset = NULL}; ignored otherwise.
#' @param alpha Numeric(2). Cobb--Douglas exponents for each household.
#'   Each \eqn{\alpha^h \in (0,1)} defines the utility
#'   \eqn{u^h(x) = \alpha^h \log x_1 + (1-\alpha^h) \log x_2}.
#'   Note: the two exponents are independent (they need not sum to 1).
#'   Required when \code{preset = NULL}; ignored otherwise.
#' @param m Numeric(2). Outside-money endowments for each household.
#'   Required when \code{preset = NULL}; ignored otherwise.
#' @param q Scalar bond price in \eqn{(0, 1)}.
#'   Equivalently the state price \eqn{\pi_1 = q}.  The interest rate is
#'   \eqn{r = (1-q)/q}.  Note: \eqn{q = 1} (zero friction) is excluded
#'   because the monetary model degenerates to a Walrasian economy.
#'   Required when \code{preset = NULL}; ignored otherwise.
#' @param q_range Numeric(2). Bounds for the bond price \eqn{q}.
#' @param endow_range Numeric(2). Bounds for each endowment \eqn{e^h_\ell}.
#' @param alpha_range Numeric(2). Bounds for the Cobb--Douglas exponent.
#' @param money_range Numeric(2). Bounds for each \eqn{m^h}.
#' @param rho Scalar in \eqn{[0,1]} or \code{NULL}.  Issuer reserve ratio
#'   (Variant 2).  If any of \code{rho}, \code{eta}, \code{S} is non-NULL,
#'   all three must be supplied.
#' @param eta Scalar in \eqn{[0,1]} or \code{NULL}.  Treasury-channel
#'   pass-through (Variant 2).
#' @param S Scalar \eqn{\ge 0} or \code{NULL}.  Aggregate token issuance
#'   (Variant 2).
#' @param g_shares Numeric(2) summing to 1.  Distribution of Treasury spending.
#' @param categories Named list assigning each good to \code{"O"}, \code{"T"},
#'   or \code{"B"}.
#' @param m_S Numeric(2) or \code{NULL}.  Stablecoin outside-money endowments
#'   (Variant 4).
#' @param e_grid Numeric vector, \code{TRUE}, or \code{NULL}.
#'   Exchange-rate grid for Variant 4.  \code{NULL} means Variant 4 is
#'   inactive.  \code{TRUE} uses a default grid \code{seq(0.5, 2, by = 0.1)}.
#'   A numeric vector specifies the grid explicitly.
#' @param seed Integer or \code{NULL}.  RNG seed for reproducibility when
#'   \code{preset = "random"}.
#' @param label Character or \code{NULL}.  Human-readable label.
#'
#' @return An S3 object of class \code{dgme_params}.
#' @export
#' @importFrom stats runif
#'
#' @examples
#' # Named preset (canonical paper example)
#' par1 <- dgme_parametrize("paper_example_1")
#'
#' # Random symmetric economy
#' par2 <- dgme_parametrize("random", style = "symmetric", seed = 42)
#'
#' # Fully manual
#' par3 <- dgme_parametrize(NULL,
#'   e = matrix(c(2.8, 1.7, 0.3, 2.1), nrow = 2, byrow = TRUE),
#'   alpha = c(0.22, 0.84), m = c(0.4, 1.8), q = 0.6,
#'   label = "my custom economy")
dgme_parametrize <- function(preset = "paper_example_1",
                             style = c("symmetric", "asymmetric"),
                             e = NULL, alpha = NULL, m = NULL, q = NULL,
                             q_range = c(0.3, 0.95),
                             endow_range = c(0.2, 5),
                             alpha_range = c(0.15, 0.85),
                             money_range = c(0.2, 5),
                             rho = NULL, eta = NULL, S = NULL,
                             g_shares = c(0.5, 0.5),
                             categories = list(good1 = "B", good2 = "B"),
                             m_S = NULL, e_grid = NULL,
                             seed = NULL, label = NULL) {

  style <- match.arg(style)

  # ---- Presets ---------------------------------------------------------------

  if (!is.null(preset) && preset == "paper_example_1") {
    # Canonical example matching the TikZ figures in the paper.
    # Asymmetric endowments and preferences create clear gains from trade
    # with a moderate monetary friction (pi1 = q = 0.6).
    e     <- matrix(c(2.8, 1.7,
                      0.3, 2.1), nrow = 2, byrow = TRUE)
    alpha <- c(0.22, 0.84)
    m     <- c(0.4, 1.8)
    q     <- 0.6

    # Treasury channel defaults (Variant 2)
    # rho = 0.10: midpoint of empirical range (0.05--0.15)
    # eta = 0.80: substantial departure from Ricardian benchmark
    # S   = 1.0:  injection G = eta*(1-rho)*S = 0.72, ~13% of baseline PC
    if (is.null(rho)) rho <- 0.10
    if (is.null(eta)) eta <- 0.80
    if (is.null(S))   S   <- 1.0

    # Competing money defaults (Variant 4)
    # m_S proportional to m ensures real-allocation invariance
    # across exchange rates (Proposition 4(iii))
    if (is.null(m_S))   m_S   <- c(0.2, 0.9)
    if (is.null(e_grid)) e_grid <- seq(0.5, 3.0, by = 0.5)

    if (is.null(label)) label <- "Paper Example 1 (canonical)"

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

      if (style == "symmetric") {
        a1 <- runif(1, alpha_range[1], alpha_range[2])
        alpha <- c(a1, 1 - a1)
        a <- runif(1, endow_range[1], endow_range[2])
        b <- runif(1, endow_range[1], endow_range[2])
        # Ensure endowments differ enough to generate meaningful trade
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

      # Check gains-from-trade condition
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
    if (is.null(label)) {
      label <- if (!is.null(seed)) paste0("Random (seed=", seed, ")") else "Random"
    }

  } else if (is.null(preset)) {
    # Fully manual
    if (is.null(e) || is.null(alpha) || is.null(m) || is.null(q))
      stop("When preset = NULL, you must supply e, alpha, m, and q.")
    if (is.null(label)) label <- "Custom"

  } else {
    stop("Unknown preset: '", preset, "'. ",
         "Use 'paper_example_1', 'paper_example_2', 'random', or NULL.")
  }

  # ---- Prepare e_grid --------------------------------------------------------

  # Expand TRUE to default grid before validation
  if (is.logical(e_grid) && isTRUE(e_grid)) {
    e_grid <- seq(0.5, 2, by = 0.1)
  }

  # ---- Label endowment matrix ------------------------------------------------

  rownames(e) <- c("h1", "h2")
  colnames(e) <- c("good1", "good2")

  # ---- Assemble S3 object ----------------------------------------------------

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
      preset     = if (is.null(preset)) "custom" else preset
    ),
    class = "dgme_params"
  )

  # ---- Validate via shared helper --------------------------------------------

  .validate_dgme_params(params)

  # ---- Post-construction diagnostic ------------------------------------------

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
#' For Cobb-Douglas preferences in a 2-household, 2-good economy, a monetary
#' equilibrium requires that the gains from trade are large enough to
#' compensate for the interest-rate friction imposed by the bid-ask spread
#' \eqn{(1, q)} on money.  This function applies two checks:
#'
#' \enumerate{
#'   \item \strong{Utility comparison.}  Compute the Walrasian equilibrium
#'     (frictionless, \eqn{q=1}) and compare each household's utility at
#'     the Walrasian allocation to autarky.  The utility gain must exceed
#'     a threshold that scales with the interest rate \eqn{r = (1-q)/q}.
#'   \item \strong{Endowment asymmetry.}  Check that endowments differ
#'     sufficiently across households relative to the interest rate.
#' }
#'
#' Both checks are heuristic (sufficient but not necessary).  A \code{TRUE}
#' return is a good indicator that equilibrium exists; \code{FALSE} is a
#' warning, not a proof of non-existence.
#'
#' @param e 2x2 endowment matrix.
#' @param alpha Length-2 vector of Cobb-Douglas exponents.
#' @param q Bond price.
#' @return Logical: TRUE if the heuristic passes.
#' @keywords internal
.check_gains_from_trade <- function(e, alpha, q) {
  r <- (1 - q) / q

  # --- Step 1: Compute Walrasian equilibrium (q = 1, no friction) ---
  #
  # With Cobb-Douglas utility u^h = alpha_h log x1 + (1-alpha_h) log x2,
  # Marshallian demands are:
  #   x1^h = alpha_h * I^h / p1
  #   x2^h = (1 - alpha_h) * I^h / p2
  # where I^h = p . e^h is income.
  #
  # Normalise p1 = 1.  Market clearing for good 1:
  #   sum_h alpha_h * I^h / 1 = total[1]
  # where I^h = e^h_1 + p2 * e^h_2.  Solving for p2:

  total <- colSums(e)
  denom <- alpha[1] * e[1, 2] + alpha[2] * e[2, 2]
  numer <- total[1] - alpha[1] * e[1, 1] - alpha[2] * e[2, 1]

  if (denom <= 0 || numer <= 0) return(FALSE)
  p2_walras <- numer / denom
  p_w <- c(1, p2_walras)

  # --- Step 2: Utility comparison (Walrasian vs autarky) ---
  #
  # For each household, compute:
  #   u_autarky  = alpha_h * log(e^h_1) + (1-alpha_h) * log(e^h_2)
  #   u_walras   = alpha_h * log(x1_w)  + (1-alpha_h) * log(x2_w)
  # The utility gain must exceed a friction-adjusted threshold.
  # Rationale: in the monetary economy, a seller receives only q*p per unit
  # sold (discounted by 1/(1+r)).  The utility cost of this friction scales
  # roughly as log(1+r) = log(1/q) for Cobb-Douglas preferences.

  friction_cost <- log(1 / q)  # = log(1 + r) approximately

  for (h in 1:2) {
    I_h <- sum(p_w * e[h, ])
    x1_w <- alpha[h] * I_h / p_w[1]
    x2_w <- (1 - alpha[h]) * I_h / p_w[2]

    u_walras  <- alpha[h] * log(x1_w) + (1 - alpha[h]) * log(x2_w)
    u_autarky <- alpha[h] * log(max(e[h, 1], 1e-12)) +
                 (1 - alpha[h]) * log(max(e[h, 2], 1e-12))

    # Gains from trade must exceed friction cost for both households
    if ((u_walras - u_autarky) < friction_cost) return(FALSE)
  }

  # --- Step 3: Endowment asymmetry check ---
  #
  # Even if the utility comparison passes, very similar endowments
  # across households mean little trade volume, which may not generate
  # enough inside money to sustain equilibrium.

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
  cat("  (each alpha^h is independent; they need not sum to 1)\n")
  cat("Outside money: m =", paste(x$m, collapse = ", "),
      " (M =", sum(x$m), ")\n")

  # Variant 2
  if (!is.null(x$rho)) {
    cat("\nTreasury channel parameters (Variant 2):\n")
    cat("  rho =", x$rho, ", eta =", x$eta, ", S =", x$S, "\n")
    G <- x$eta * (1 - x$rho) * x$S
    cat("  Treasury spending G = eta*(1-rho)*S =", G, "\n")
    cat("  g_shares =", paste(x$g_shares, collapse = ", "), "\n")
  }

  # Variant 4
  if (!is.null(x$m_S)) {
    cat("\nCompeting outside money (Variant 4):\n")
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
