#' Compute Geometric Primitives for Visualisation
#'
#' Given a \code{dgme_result} object (from \code{\link{dgme_solve}}), compute
#' all geometric quantities needed to draw the NA-equilibrium in net-trade
#' space and the ME-equilibrium in nominal-income-transfer space.  This
#' function performs \strong{no TikZ generation}---it is pure geometry.
#'
#' @param result A \code{dgme_result} object.
#' @param max_width_cm Numeric.  Target width of the TikZ figure in
#'   centimetres.  Default \code{12}.
#' @param pad Numeric.  Fractional padding around the data bounding box.
#'   Default \code{0.12}.
#' @param axis_band_frac Numeric.  Width of the background axis bands as a
#'   fraction of the panel span.  Default \code{0.035}.
#' @param stripes_n Integer.  Number of indifference-curve stripes above
#'   the tangent IC.  Default \code{9}.
#' @param stripes_step Numeric.  Multiplicative step between successive
#'   stripes (applied to the IC constant \eqn{C}).  Default \code{0.03}.
#' @param y_clamp_pad Numeric.  Extra padding for IC clamping.  Default
#'   \code{0.5}.
#' @param ic_f_low,ic_f_high Numeric.  Scaling factors for lower/higher
#'   reference ICs.  Default \code{0.6} and \code{1.4}.
#'
#' @return An S3 object of class \code{dgme_geometry}, a list with:
#'   \describe{
#'     \item{\code{params}}{The underlying \code{dgme_params}.}
#'     \item{\code{result}}{The underlying \code{dgme_result}.}
#'     \item{\code{na_real}}{Geometry for the net-trade-space figure.}
#'     \item{\code{me_nominal}}{Geometry for the nominal-transfer figure.}
#'   }
#' @export
dgme_geometry <- function(result,
                          max_width_cm   = 12,
                          pad            = 0.12,
                          axis_band_frac = 0.035,
                          stripes_n      = 9L,
                          stripes_step   = 0.03,
                          y_clamp_pad    = 0.5,
                          ic_f_low       = 0.6,
                          ic_f_high      = 1.4) {

  stopifnot(inherits(result, "dgme_result"))

  params <- result$params
  p  <- result$p
  x  <- result$x      # 2x2 matrix
  e  <- params$e       # 2x2 matrix
  m  <- params$m
  q  <- params$q
  alpha <- params$alpha
  tau <- result$tau    # 2x2 matrix
  b  <- result$b
  D  <- result$D
  M  <- sum(m)

  p1 <- p[1]; p2 <- p[2]

  # =========================================================================
  # NA-REAL (net-trade space) geometry
  # =========================================================================

  na <- .compute_na_geometry(
    p1, p2, q, e, x, alpha, m, tau,
    max_width_cm, pad, axis_band_frac,
    stripes_n, stripes_step, y_clamp_pad
  )

  # =========================================================================
  # ME-NOMINAL (income-transfer space) geometry
  # =========================================================================

  me <- .compute_me_geometry(p1, p2, q, e, m, b, D, M)

  # =========================================================================
  # Assemble
  # =========================================================================

  structure(
    list(
      params     = params,
      result     = result,
      na_real    = na,
      me_nominal = me
    ),
    class = "dgme_geometry"
  )
}


# ===========================================================================
# NA-real geometry (net-trade space)
# ===========================================================================

#' @keywords internal
.compute_na_geometry <- function(p1, p2, q, e, x, alpha, m, tau,
                                 max_width_cm, pad, axis_band_frac,
                                 stripes_n, stripes_step, y_clamp_pad) {

  # --- Budget kink points in consumption space (x-space) ---
  #
  # The kinked NA budget for household h has four vertices A,B,C,D:
  #   A: sell all of good 1 for good 2 (x1 = 0)
  #   B: kink at endowment good 1 (x1 = e1, no trade in good 1)
  #   C: kink at endowment good 2 (x2 = e2, no trade in good 2)
  #   D: sell all of good 2 for good 1 (x2 = 0)
  #
  # On the sell-1-buy-2 face (A→B): effective prices (q*p1, p2)
  #   Budget: q*p1*x1 + p2*x2 = q*p1*e1 + p2*e2 + m
  # On the buy-both face (B→C): effective prices (p1, p2)
  #   Budget: p1*x1 + p2*x2 = p1*e1 + p2*e2 + m
  # On the sell-2-buy-1 face (C→D): effective prices (p1, q*p2)
  #   Budget: p1*x1 + q*p2*x2 = p1*e1 + q*p2*e2 + m

  kinks_x <- list()
  kinks_tau <- list()

  for (h in 1:2) {
    eh <- e[h, ]; mh <- m[h]

    Ax <- 0
    Ay <- eh[2] + mh/p2 + (p1 * eh[1] / p2) * q

    Bx <- eh[1]
    By <- eh[2] + mh/p2

    Cx <- eh[1] + mh/p1
    Cy <- eh[2]

    Dx <- eh[1] + mh/p1 + (p2 * eh[2] / p1) * q
    Dy <- 0

    kinks_x[[h]] <- list(
      A = c(Ax, Ay), B = c(Bx, By),
      C = c(Cx, Cy), D = c(Dx, Dy)
    )

    # Translate to tau-space: tau = x - e
    kinks_tau[[h]] <- list(
      A = c(Ax - eh[1], Ay - eh[2]),
      B = c(Bx - eh[1], By - eh[2]),
      C = c(Cx - eh[1], Cy - eh[2]),
      D = c(Dx - eh[1], Dy - eh[2])
    )
  }

  # Optimal points in tau-space
  tau_opt <- list(
    h1 = c(tau[1, 1], tau[1, 2]),
    h2 = c(tau[2, 1], tau[2, 2])
  )

  # --- Window and scale ---

  all_x <- c(
    sapply(kinks_tau[[1]], `[`, 1), sapply(kinks_tau[[2]], `[`, 1),
    tau_opt$h1[1], tau_opt$h2[1]
  )
  all_y <- c(
    sapply(kinks_tau[[1]], `[`, 2), sapply(kinks_tau[[2]], `[`, 2),
    tau_opt$h1[2], tau_opt$h2[2]
  )

  x_span <- max(all_x) - min(all_x)
  y_span <- max(all_y) - min(all_y)
  Xmin <- min(all_x) - pad * x_span
  Xmax <- max(all_x) + pad * x_span
  Ymin <- min(all_y) - pad * y_span
  Ymax <- max(all_y) + pad * y_span

  span <- max(Xmax - Xmin, Ymax - Ymin)
  Unit <- max_width_cm / span
  axis_band <- axis_band_frac * span

  Ylo <- Ymin - y_clamp_pad
  Yhi <- Ymax + y_clamp_pad

  # --- Indifference curve parameters ---
  #
  # For Cobb-Douglas u = x1^alpha * x2^(1-alpha), the IC through
  # (x1*, x2*) in tau-space is:
  #   x2 = C * x1^eta  where eta = -alpha/(1-alpha)
  #   C  = U*^{1/(1-alpha)}  with U* = x1*^alpha * x2*^(1-alpha)

  ic <- list()
  for (h in 1:2) {
    ah <- alpha[h]
    xh <- x[h, ]
    eta <- -ah / (1 - ah)
    Ustar <- xh[1]^ah * xh[2]^(1 - ah)
    C_tan <- Ustar^(1 / (1 - ah))

    # Safe left bound for plotting
    base_left <- ((Yhi + e[h, 2]) / C_tan)^(1 / eta) - e[h, 1]
    tmin <- max(Xmin, base_left)

    ic[[h]] <- list(
      alpha = ah, eta = eta,
      Ustar = Ustar, C_tan = C_tan,
      tmin = tmin,
      endow = e[h, ]  # needed for tau -> x conversion in IC formula
    )
  }

  # --- Regime detection and effective prices ---
  #
  # Determine which face each household's optimum lies on, and compute
  # the effective price vector (price normal) at the optimum.

  regime <- list()
  price_normal <- list()

  for (h in 1:2) {
    xh <- x[h, ]; eh <- e[h, ]
    # Classify regime
    is_left  <- (xh[1] <= eh[1]) && (xh[2] >= eh[2])  # sell 1, buy 2
    is_mid   <- (xh[1] >= eh[1]) && (xh[2] >= eh[2])  # buy both
    is_right <- (xh[1] >= eh[1]) && (xh[2] <= eh[2])  # buy 1, sell 2

    reg <- if (is_left) "S1B2" else if (is_right) "B1S2" else "BB"
    regime[[h]] <- list(
      name = reg,
      is_left = is_left, is_mid = is_mid, is_right = is_right
    )

    # Effective prices at the optimum face
    vx <- p1 * (if (is_left) q else 1)
    vy <- p2 * (if (is_right) q else 1)
    price_normal[[h]] <- c(vx, vy)
  }

  # --- Price normal arrow scaling ---
  min_xy <- min(Xmax - Xmin, Ymax - Ymin)
  Lnorm <- 0.18 * min_xy

  arrow_end <- list()
  for (h in 1:2) {
    v <- price_normal[[h]]
    s <- Lnorm / sqrt(sum(v^2))
    arrow_end[[h]] <- tau_opt[[h]] + s * v
  }

  # --- Sign flags for tau labels ---
  tau_signs <- list(
    h1 = c(if (tau_opt$h1[1] < 0) "-" else "+",
            if (tau_opt$h1[2] < 0) "-" else "+"),
    h2 = c(if (tau_opt$h2[1] < 0) "-" else "+",
            if (tau_opt$h2[2] < 0) "-" else "+")
  )

  list(
    kinks_x     = kinks_x,
    kinks_tau   = kinks_tau,
    tau_opt     = tau_opt,
    window      = list(Xmin = Xmin, Xmax = Xmax, Ymin = Ymin, Ymax = Ymax,
                       Ylo = Ylo, Yhi = Yhi),
    scale       = list(Unit = Unit, axis_band = axis_band, Lnorm = Lnorm),
    ic          = ic,
    regime      = regime,
    price_normal = price_normal,
    arrow_end   = arrow_end,
    tau_signs   = tau_signs,
    config      = list(stripes_n = stripes_n, stripes_step = stripes_step)
  )
}


# ===========================================================================
# ME-nominal geometry (income-transfer space)
# ===========================================================================

#' @keywords internal
.compute_me_geometry <- function(p1, p2, q, e, m, b, D, M) {

  # Face values D^h = -b^h >= 0
  Dh <- pmax(-b, 0)

  # Household bond vectors in (nu0, nu1) space:
  #   nu^h = (q * D^h, -D^h)
  # This lies on the marketed cone C(q) = {(qt, -t) : t >= 0}
  bond_pts <- list(
    h1 = c(q * Dh[1], -Dh[1]),
    h2 = c(q * Dh[2], -Dh[2])
  )

  # Aggregate
  bond_agg <- c(q * D, -D)

  # Feasible trapezoids in (nu0, nu1) space
  # For household h, the feasible set is bounded by:
  #   nu0 >= -m^h  (left edge: money endowment)
  #   nu0 <= 0 to the origin, then along C(q) (right edge)
  #   nu1 >= -p . e^h (bottom: maximum sales revenue at settlement)
  # The trapezoid vertices are:
  #   (-m^h, 0), (0, 0), (q * p.e^h / (1), -(p.e^h)), (-m^h, -(p.e^h))
  # More precisely, the maximum D^h is limited by p.e^h (can't sell more
  # than you have), so the rightmost point on C(q) is at D_max = p.e^h.

  trapezoids <- list()
  for (h in 1:2) {
    pe <- p1 * e[h, 1] + p2 * e[h, 2]  # p . e^h
    mh <- m[h]
    D_max <- pe
    trapezoids[[h]] <- list(
      top_left  = c(-mh, 0),
      top_right = c(0, 0),
      bot_right = c(q * D_max, -D_max),
      bot_left  = c(-mh, -D_max),
      pe = pe
    )
  }

  # Dual cone C(q)* (optional, for GEI-aware readers)
  # Banking technology vector B = (-q, 1)

  # Axis range: accommodate all elements
  all_x <- c(
    trapezoids[[1]]$top_left[1], trapezoids[[1]]$bot_right[1],
    trapezoids[[2]]$top_left[1], trapezoids[[2]]$bot_right[1],
    bond_pts$h1[1], bond_pts$h2[1], bond_agg[1], -q, 1
  )
  all_y <- c(
    trapezoids[[1]]$bot_left[2], trapezoids[[1]]$top_right[2],
    trapezoids[[2]]$bot_left[2], trapezoids[[2]]$top_right[2],
    bond_pts$h1[2], bond_pts$h2[2], bond_agg[2], 1, q
  )

  pad <- 0.15
  x_span <- max(all_x) - min(all_x)
  y_span <- max(all_y) - min(all_y)
  Xmin <- min(all_x) - pad * x_span
  Xmax <- max(all_x) + pad * x_span
  Ymin <- min(all_y) - pad * y_span
  Ymax <- max(all_y) + pad * y_span

  span <- max(Xmax - Xmin, Ymax - Ymin)
  Unit <- 12 / span  # fixed 12cm target for this panel

  # Marketed cone C(q) ray endpoint (for drawing)
  t_max <- max(1e-6, D) * 1.25
  cone_end <- c(q * t_max, -t_max)
  cone_bank <- c(-q * t_max * 0.8, t_max * 0.8)

  # Dual cone wedge endpoint
  dual_cone_len <- 1.2

  list(
    bond_pts     = bond_pts,
    bond_agg     = bond_agg,
    Dh           = Dh,
    trapezoids   = trapezoids,
    cone_end     = cone_end,
    cone_bank    = cone_bank,
    dual_cone_len = dual_cone_len,
    window       = list(Xmin = Xmin, Xmax = Xmax, Ymin = Ymin, Ymax = Ymax),
    scale        = list(Unit = Unit)
  )
}


# ===========================================================================
# Print method
# ===========================================================================

#' @export
print.dgme_geometry <- function(x, ...) {
  cat("DG Monetary Equilibrium Geometry\n")
  cat("  Label:", x$params$label, "\n")
  cat("  Variant:", x$result$variant, "\n\n")

  na <- x$na_real
  cat("NA-real (net-trade space):\n")
  cat(sprintf("  Window: [%.2f, %.2f] x [%.2f, %.2f]\n",
              na$window$Xmin, na$window$Xmax, na$window$Ymin, na$window$Ymax))
  cat(sprintf("  Scale: %.3f cm/unit\n", na$scale$Unit))
  for (h in 1:2) {
    cat(sprintf("  h%d regime: %s  |  tau_opt = (%.4f, %.4f)\n",
                h, na$regime[[h]]$name,
                na$tau_opt[[h]][1], na$tau_opt[[h]][2]))
  }

  me <- x$me_nominal
  cat("\nME-nominal (income-transfer space):\n")
  cat(sprintf("  Window: [%.2f, %.2f] x [%.2f, %.2f]\n",
              me$window$Xmin, me$window$Xmax, me$window$Ymin, me$window$Ymax))
  for (h in 1:2) {
    cat(sprintf("  h%d bond point: (%.4f, %.4f)  D^h = %.4f\n",
                h, me$bond_pts[[h]][1], me$bond_pts[[h]][2], me$Dh[h]))
  }
  invisible(x)
}
