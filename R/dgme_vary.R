#' Generate a Family of Economies for Comparative Statics
#'
#' Given a base \code{dgme_params} object (from \code{\link{dgme_parametrize}}),
#' produce a list of alternative economies in which one or more parameters
#' vary across a specified grid while all other parameters remain fixed.
#' The base economy is stored in the result for comparison but is \emph{not}
#' duplicated in the list of alternatives.
#'
#' @section Design and usage:
#' \code{dgme_vary} implements the comparative-statics step of the
#' modelling pipeline:
#' \preformatted{
#'   parametrize -> vary -> solve each -> compare
#' }
#'
#' The simplest use is a single alternative economy:
#' \preformatted{
#'   dgme_vary(base, q = 0.5)
#' }
#' which produces one economy identical to \code{base} except with
#' \eqn{q = 0.5}.  Supply a vector for a sweep:
#' \preformatted{
#'   dgme_vary(base, q = seq(0.3, 0.9, 0.1))
#' }
#'
#' The function supports three modes of variation:
#' \describe{
#'   \item{\strong{Single parameter.}}{
#'     Vary one scalar or vector parameter.  The number of alternative
#'     economies equals the number of values supplied.
#'   }
#'   \item{\strong{Multiple parameters (cross product).}}{
#'     With \code{grid_type = "cross"} (the default), the function forms
#'     the full Cartesian product of all grids.
#'     Example: \code{dgme_vary(base, q = c(0.5, 0.7), S = c(0, 5, 10))}
#'     produces \eqn{2 \times 3 = 6} alternative economies.
#'   }
#'   \item{\strong{Multiple parameters (parallel).}}{
#'     With \code{grid_type = "parallel"}, grids are stepped through
#'     in lockstep (all must have the same length).
#'   }
#' }
#'
#' @section Varying non-scalar parameters:
#' Parameters that are vectors (\code{m}, \code{alpha}, \code{m_S},
#' \code{g_shares}) or matrices (\code{e}) must be varied by supplying
#' a \strong{list} of values, where each element has the same shape as
#' the original parameter:
#' \preformatted{
#'   dgme_vary(base, m = list(c(0.4, 1.8), c(1, 1), c(3, 0.5)))
#' }
#'
#' @section Accessing results:
#' The base economy is available as \code{result$base}.  Alternative
#' economies are accessed via \code{result[[i]]}.  A typical downstream
#' workflow:
#' \preformatted{
#'   eq_base <- dgme_solve(family$base)
#'   eq_alt  <- dgme_solve(family[[1]])
#' }
#'
#' @param base A \code{dgme_params} object (the reference economy).
#' @param ... Named arguments specifying which parameters to vary and
#'   their values.  For scalar parameters (\code{q}, \code{rho},
#'   \code{eta}, \code{S}), supply a numeric vector.  For vector or
#'   matrix parameters (\code{m}, \code{alpha}, \code{e}, \code{m_S},
#'   \code{g_shares}), supply a list of vectors/matrices.
#' @param grid_type Character.  \code{"cross"} (default) forms the Cartesian
#'   product of all grids; \code{"parallel"} steps through them in lockstep.
#' @param label_prefix Character or \code{NULL}.  Prefix for auto-generated
#'   labels.  Defaults to the base economy's label.
#' @param validate Logical.  If \code{TRUE} (default), each generated
#'   economy is validated via \code{.validate_dgme_params()}.  Set to
#'   \code{FALSE} to skip validation for speed on large grids.
#'
#' @return An S3 object of class \code{dgme_vary}, which is a list with:
#'   \describe{
#'     \item{\code{base}}{The original \code{dgme_params} object.}
#'     \item{\code{varied}}{Character vector of parameter names being varied.}
#'     \item{\code{grid_type}}{The grid type used.}
#'     \item{\code{grid}}{A data frame describing parameter combinations.}
#'     \item{\code{economies}}{A list of \code{dgme_params} objects (the
#'       alternatives; does \emph{not} include the base).}
#'     \item{\code{n}}{Integer: number of alternative economies.}
#'   }
#'
#' @export
#'
#' @examples
#' base <- dgme_parametrize("paper_example_1")
#'
#' # Single alternative (change q from 0.6 to 0.5)
#' alt <- dgme_vary(base, q = 0.5)
#' alt$base$q   # 0.6  (original)
#' alt[[1]]$q   # 0.5  (alternative)
#'
#' # Sweep over bond price
#' fam <- dgme_vary(base, q = seq(0.3, 0.9, by = 0.1))
#' print(fam)
#'
#' # Cross product of q and S (requires Variant 2 base)
#' base_v2 <- dgme_parametrize("paper_example_1", rho = 0.2, eta = 0.5, S = 5)
#' fam_qs <- dgme_vary(base_v2, q = c(0.5, 0.7), S = c(0, 5, 10))
#'
#' # Vary money endowments (non-scalar)
#' fam_m <- dgme_vary(base, m = list(c(0.4, 1.8), c(1, 1), c(3, 3)))
#'
#' # Parallel variation (lockstep)
#' fam_par <- dgme_vary(base, q = c(0.4, 0.6, 0.8),
#'                      m = list(c(0.2, 1), c(0.4, 1.8), c(1, 3)),
#'                      grid_type = "parallel")
dgme_vary <- function(base,
                      ...,
                      grid_type = c("cross", "parallel"),
                      label_prefix = NULL,
                      validate = TRUE) {

  stopifnot(inherits(base, "dgme_params"))
  grid_type <- match.arg(grid_type)

  if (is.null(label_prefix)) label_prefix <- base$label

  # ---- Parse variation specs -------------------------------------------------

  vary_specs <- list(...)
  if (length(vary_specs) == 0) {
    stop("No parameters to vary. Supply at least one named argument ",
         "(e.g., q = 0.5 or q = seq(0.3, 0.9, 0.1)).")
  }

  # Parameter classification
  scalar_params <- c("q", "rho", "eta", "S")
  vector_params <- c("m", "alpha", "m_S", "g_shares")
  matrix_params <- c("e")
  all_params    <- c(scalar_params, vector_params, matrix_params)

  varied_names <- names(vary_specs)
  if (is.null(varied_names) || any(varied_names == "")) {
    stop("All variation arguments must be named ",
         "(e.g., q = 0.5).")
  }

  unknown <- setdiff(varied_names, all_params)
  if (length(unknown) > 0) {
    stop("Cannot vary parameter(s): ", paste(unknown, collapse = ", "), ". ",
         "Allowed: ", paste(all_params, collapse = ", "), ".")
  }

  # ---- Normalise each spec to a list of values -------------------------------

  grid_lists <- list()
  grid_lengths <- integer(length(vary_specs))

  for (k in seq_along(vary_specs)) {
    nm  <- varied_names[k]
    val <- vary_specs[[k]]

    if (nm %in% scalar_params) {
      if (!is.numeric(val) || length(val) < 1) {
        stop("Parameter '", nm, "' is scalar: supply a numeric vector ",
             "(e.g., ", nm, " = 0.5 or ", nm, " = c(0.3, 0.5, 0.7)).")
      }
      grid_lists[[nm]] <- as.list(val)

    } else if (nm %in% vector_params) {
      if (!is.list(val)) {
        stop("Parameter '", nm, "' is a vector: supply a list of vectors ",
             "(e.g., ", nm, " = list(c(1,2), c(3,4))).")
      }
      grid_lists[[nm]] <- val

    } else if (nm %in% matrix_params) {
      if (!is.list(val)) {
        stop("Parameter '", nm, "' is a matrix: supply a list of matrices.")
      }
      grid_lists[[nm]] <- val
    }

    grid_lengths[k] <- length(grid_lists[[nm]])
  }

  names(grid_lengths) <- varied_names

  # ---- Build the grid index --------------------------------------------------

  if (grid_type == "cross") {
    idx_list <- lapply(grid_lengths, seq_len)
    grid_idx <- expand.grid(idx_list, KEEP.OUT.ATTRS = FALSE)
  } else {
    # Parallel: all grids must have the same length
    if (length(unique(grid_lengths)) > 1) {
      stop("For grid_type = 'parallel', all parameter grids must have ",
           "the same length. Got: ",
           paste(varied_names, "=", grid_lengths, collapse = ", "), ".")
    }
    n_pts <- grid_lengths[1]
    grid_idx <- as.data.frame(
      matrix(rep(seq_len(n_pts), length(varied_names)),
             ncol = length(varied_names))
    )
    names(grid_idx) <- varied_names
  }

  n_economies <- nrow(grid_idx)

  # ---- Check Variant 2 completeness ------------------------------------------

  v2_params <- c("rho", "eta", "S")
  v2_varied <- intersect(varied_names, v2_params)
  if (length(v2_varied) > 0) {
    v2_fixed <- setdiff(v2_params, v2_varied)
    v2_missing <- v2_fixed[vapply(v2_fixed, function(p) is.null(base[[p]]),
                                  logical(1))]
    if (length(v2_missing) > 0) {
      stop("Variant 2 parameters must all be present. You are varying ",
           paste(v2_varied, collapse = ", "), " but ",
           paste(v2_missing, collapse = ", "),
           " is/are NULL in the base economy. Either vary them too or ",
           "set them in the base economy via dgme_parametrize().")
    }
  }

  # ---- Generate alternative economies ----------------------------------------

  economies <- vector("list", n_economies)

  for (i in seq_len(n_economies)) {
    # Start from the base
    p <- base

    # Override each varied parameter
    desc_parts <- character(length(varied_names))
    for (k in seq_along(varied_names)) {
      nm  <- varied_names[k]
      idx <- grid_idx[i, k]
      val <- grid_lists[[nm]][[idx]]
      p[[nm]] <- val

      # Build label description
      if (nm %in% scalar_params) {
        desc_parts[k] <- paste0(nm, "=", signif(val, 4))
      } else if (nm %in% vector_params) {
        desc_parts[k] <- paste0(nm, "=(",
                                paste(signif(val, 4), collapse = ","), ")")
      } else {
        desc_parts[k] <- paste0(nm, "=[matrix]")
      }
    }

    p$label <- paste0(label_prefix, " | ", paste(desc_parts, collapse = ", "))

    if (validate) {
      .validate_dgme_params(p)
    }

    class(p) <- "dgme_params"
    economies[[i]] <- p
  }

  # ---- Build grid summary for display ----------------------------------------

  grid_values <- grid_idx
  for (k in seq_along(varied_names)) {
    nm <- varied_names[k]
    if (nm %in% scalar_params) {
      grid_values[[nm]] <- vapply(
        grid_idx[[nm]],
        function(j) grid_lists[[nm]][[j]],
        numeric(1)
      )
    }
  }

  # ---- Assemble return object ------------------------------------------------

  result <- structure(
    list(
      base      = base,
      varied    = varied_names,
      grid_type = grid_type,
      grid      = grid_values,
      economies = economies,
      n         = n_economies
    ),
    class = "dgme_vary"
  )

  result
}


# ---- Methods for dgme_vary --------------------------------------------------

#' @export
print.dgme_vary <- function(x, ...) {
  cat("DG Monetary Economy \u2014 Comparative Statics\n")
  cat("Base economy:", x$base$label, "\n")
  cat("Varying:", paste(x$varied, collapse = ", "), "\n")
  cat("Grid type:", x$grid_type, "\n")
  cat("Alternative economies:", x$n, "\n\n")

  if (x$n <= 20) {
    cat("Parameter grid:\n")
    print(x$grid, row.names = FALSE)
  } else {
    cat("Parameter grid (first 10 of ", x$n, "):\n")
    print(utils::head(x$grid, 10), row.names = FALSE)
    cat("  ... (", x$n - 10, " more rows)\n")
  }
  invisible(x)
}

#' Extract an alternative economy by index
#'
#' @param x A \code{dgme_vary} object.
#' @param i Integer index of the alternative economy to extract.
#' @param ... Further arguments (currently unused).
#'
#' @return A \code{dgme_params} object.
#' @export
`[[.dgme_vary` <- function(x, i, ...) {
  x$economies[[i, ...]]
}

#' @export
length.dgme_vary <- function(x) {
  x$n
}

#' @export
summary.dgme_vary <- function(object, ...) {
  cat("DG Monetary Economy \u2014 Comparative Statics Summary\n\n")
  cat("Base economy:", object$base$label, "\n")
  cat("Parameters varied:", paste(object$varied, collapse = ", "), "\n")
  cat("Grid type:", object$grid_type, "\n")
  cat("Alternative economies:", object$n, "\n\n")

  scalar_params <- c("q", "rho", "eta", "S")
  for (nm in intersect(object$varied, scalar_params)) {
    vals <- object$grid[[nm]]
    cat(sprintf("  %s: %s to %s (%d values)\n",
                nm, signif(min(vals), 4), signif(max(vals), 4),
                length(vals)))
  }

  non_scalar <- setdiff(object$varied, scalar_params)
  for (nm in non_scalar) {
    n_vals <- length(unique(object$grid[[nm]]))
    cat(sprintf("  %s: %d distinct values\n", nm, n_vals))
  }

  invisible(object)
}
