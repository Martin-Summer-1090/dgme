# ===========================================================================
# ME-nominal geometry (income-transfer space) — REVISED
# ===========================================================================
# Drop-in replacement for .compute_me_geometry() in dgme_geometry.R
#
# Design principles (matching the hand-crafted template):
#   - The dominant extent is downward (−p·eʰ).  Padding is proportional
#     to this extent, giving tight, well-scaled figures.
#   - The marketed cone C(q) ray is drawn to exactly reach the y-axis
#     boundaries (Ymin on the positive side, Ymax on the negative side),
#     so the ray fills the figure without overshooting.
#   - The x-axis extends just past the cone ray endpoints, with generous
#     room on the left for the −mʰ label and tight on the right.
#   - Returns cone endpoints and a feasibility-label position for each
#     household's trapezoid.
# ===========================================================================

#' @keywords internal
.compute_me_geometry <- function(p1, p2, q, e, m, b, D, M) {

  # Face values D^h = -b^h >= 0
  Dh <- pmax(-b, 0)

  # Household bond vectors in (nu0, nu1) space:
  #   nu^h = (q * D^h, -D^h)
  bond_pts <- list(
    h1 = c(q * Dh[1], -Dh[1]),
    h2 = c(q * Dh[2], -Dh[2])
  )

  # Aggregate
  bond_agg <- c(q * D, -D)

  # Feasible trapezoids
  trapezoids <- list()
  for (h in 1:2) {
    pe <- p1 * e[h, 1] + p2 * e[h, 2]
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

  # --- Window computation ---
  pad <- 0.12
  pe_max <- max(sapply(trapezoids, function(tr) tr$pe))

  # Y-axis: the negative extent (−p·eʰ) dominates
  Ymin <- -pe_max * (1 + pad)
  Ymax <- max(1, pad * pe_max)   # ensures room past calibration box (y = 1)

  # Cone C(q) ray endpoints, reaching exactly Ymin / Ymax
  cone_pos <- c(q * (-Ymin), Ymin)       # positive direction (lower-right)
  cone_neg <- c(q * (-Ymax), Ymax)       # negative direction (upper-left)

  # X-axis: tight on the right, generous on the left (label room)
  x_left  <- min(cone_neg[1], min(sapply(trapezoids, function(tr) tr$top_left[1])))
  Xmin <- x_left - pad * cone_pos[1]     # scale left pad by the large positive extent
  Xmax <- cone_pos[1] + pad              # small fixed pad on the right

  # Scale
  x_range <- Xmax - Xmin
  y_range <- Ymax - Ymin
  span <- max(x_range, y_range)
  Unit <- 12 / span

  # Dual cone wedge endpoint (optional overlay)
  dual_cone_len <- 1.2

  # Feasibility region label position (lower-center of trapezoid)
  label_pos <- list()
  for (h in 1:2) {
    tr <- trapezoids[[h]]
    lx <- 0.5 * tr$bot_right[1]
    ly <- 0.80 * tr$bot_left[2]
    label_pos[[h]] <- c(lx, ly)
  }

  list(
    bond_pts      = bond_pts,
    bond_agg      = bond_agg,
    Dh            = Dh,
    trapezoids    = trapezoids,
    cone_pos      = cone_pos,
    cone_neg      = cone_neg,
    dual_cone_len = dual_cone_len,
    label_pos     = label_pos,
    window        = list(Xmin = Xmin, Xmax = Xmax, Ymin = Ymin, Ymax = Ymax),
    scale         = list(Unit = Unit)
  )
}
