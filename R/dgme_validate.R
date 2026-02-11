#' Validate a dgme_params object
#'
#' Runs domain checks on all parameters of a \code{dgme_params} object
#' (or a list with the same structure).  Used internally by
#' \code{\link{dgme_parametrize}} and \code{\link{dgme_vary}}.
#'
#' @param p A list with at minimum the fields \code{e}, \code{alpha},
#'   \code{m}, \code{q}.  Optionally \code{rho}, \code{eta}, \code{S},
#'   \code{g_shares}, \code{m_S}, \code{e_grid}.
#' @return Invisible \code{TRUE} on success; stops with an error otherwise.
#' @keywords internal
.validate_dgme_params <- function(p) {

  # ---- Core parameters -------------------------------------------------------

  e <- p$e; alpha <- p$alpha; m <- p$m; q <- p$q

  stopifnot(
    is.matrix(e), nrow(e) == 2, ncol(e) == 2,
    all(e >= 0), all(colSums(e) > 0),
    is.numeric(alpha), length(alpha) == 2,
    all(alpha > 0), all(alpha < 1),
    is.numeric(m), length(m) == 2, all(m >= 0), sum(m) > 0,
    is.numeric(q), length(q) == 1, q > 0, q < 1
  )

  # ---- Variant 2 (treasury channel): all-or-none -----------------------------

  v2 <- c(!is.null(p$rho), !is.null(p$eta), !is.null(p$S))
  if (any(v2) && !all(v2)) {
    missing_v2 <- c("rho", "eta", "S")[!v2]
    stop("Variant 2 parameters must be supplied together (all or none). ",
         "Missing: ", paste(missing_v2, collapse = ", "), ".")
  }
  if (all(v2)) {
    stopifnot(is.numeric(p$rho), length(p$rho) == 1, p$rho >= 0, p$rho <= 1)
    stopifnot(is.numeric(p$eta), length(p$eta) == 1, p$eta >= 0, p$eta <= 1)
    stopifnot(is.numeric(p$S),   length(p$S) == 1,   p$S >= 0)
    stopifnot(is.numeric(p$g_shares), length(p$g_shares) == 2,
              abs(sum(p$g_shares) - 1) < 1e-12)
  }

  # ---- Variant 4 (competing settlement assets) -------------------------------

  if (!is.null(p$m_S)) {
    stopifnot(is.numeric(p$m_S), length(p$m_S) == 2,
              all(p$m_S >= 0), sum(p$m_S) > 0)
  }
  if (!is.null(p$e_grid)) {
    stopifnot(is.numeric(p$e_grid), length(p$e_grid) >= 1,
              all(p$e_grid > 0))
  }

  invisible(TRUE)
}
