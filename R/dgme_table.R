#' Generate Tables for DG Monetary Equilibria
#'
#' Produce publication-quality tables summarising equilibrium quantities:
#' parameters, allocations, prices, payment capacity, and cross-variant
#' comparisons.  Output can be LaTeX (ready for \code{\\input{}}),
#' Markdown, or a \code{data.frame} for further manipulation.
#'
#' @param results A \code{dgme_results} or \code{dgme_result} object
#'   from \code{\link{dgme_solve}}.
#' @param type Character.  Table type:
#'   \describe{
#'     \item{\code{"equilibrium"}}{Full equilibrium for one variant.}
#'     \item{\code{"parameters"}}{Input parameters.}
#'     \item{\code{"comparison"}}{Side-by-side across variants.}
#'     \item{\code{"indeterminacy"}}{Table of \eqn{(\bar e, p, x)} across
#'       exchange rates (Variant 4 only).}
#'   }
#' @param variants Character or \code{NULL}.  Which variants to include.
#'   Default: all solved variants.
#' @param digits Integer.  Decimal places.  Default 4.
#' @param caption Character or \code{NULL}.  LaTeX table caption.
#'   Auto-generated if \code{NULL}.
#' @param label Character or \code{NULL}.  LaTeX label.  Auto-generated
#'   if \code{NULL}.
#' @param format Character.  Output format: \code{"latex"} (default),
#'   \code{"markdown"}, or \code{"data.frame"}.
#' @param save Logical.  Write to file?
#' @param path Character or \code{NULL}.  Directory.  Default:
#'   \code{getOption("dgme.tab_dir", "tables/")}.
#' @param filename Character or \code{NULL}.  File name (without extension).
#'   Auto-generated if \code{NULL}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A character string (LaTeX or Markdown), or a \code{data.frame}.
#'   When \code{save = TRUE}, also writes to file and returns invisibly.
#' @export
#'
#' @examples
#' par <- dgme_parametrize("paper_example_1")
#' res <- dgme_solve(par)
#' dgme_table(res, "equilibrium", variants = "baseline")
#' dgme_table(res, "parameters")
dgme_table <- function(results,
                       type = c("equilibrium", "parameters",
                                "comparison", "indeterminacy"),
                       variants = NULL,
                       digits = 4,
                       caption = NULL,
                       label = NULL,
                       format = c("latex", "markdown", "data.frame"),
                       save = FALSE,
                       path = NULL,
                       filename = NULL,
                       ...) {

  type   <- match.arg(type)
  format <- match.arg(format)

  # Normalise input: accept both dgme_results and dgme_result
  if (inherits(results, "dgme_result")) {
    # Wrap single result
    params <- results$params
    res_list <- list()
    res_list[[results$variant]] <- results
    variants_solved <- results$variant
  } else if (inherits(results, "dgme_results")) {
    params <- results$params
    res_list <- results$results
    variants_solved <- results$variants_solved
  } else {
    stop("'results' must be a dgme_results or dgme_result object.")
  }

  if (is.null(variants)) variants <- variants_solved
  variants <- intersect(variants, variants_solved)

  if (length(variants) == 0 && type != "parameters") {
    stop("No matching variants found.")
  }

  # Dispatch
  df <- switch(type,
    parameters    = .table_parameters(params, digits),
    equilibrium   = .table_equilibrium(res_list[[variants[1]]], digits),
    comparison    = .table_comparison(res_list, variants, digits),
    indeterminacy = .table_indeterminacy(res_list, variants, digits)
  )

  if (format == "data.frame") {
    return(df)
  }

  # Auto-generate caption and label
  if (is.null(caption)) {
    caption <- switch(type,
      parameters    = paste0("Parameters: ", params$label),
      equilibrium   = paste0("Equilibrium (", variants[1], "): ", params$label),
      comparison    = paste0("Comparison: ", paste(variants, collapse = " vs.\\ "),
                             " --- ", params$label),
      indeterminacy = paste0("Exchange-rate indeterminacy: ", params$label)
    )
  }
  if (is.null(label)) {
    label <- paste0("tab:", type, "_",
                    gsub("[^A-Za-z0-9]", "_", params$label))
  }

  # Format output
  out <- if (format == "latex") {
    .df_to_latex(df, caption, label, digits)
  } else {
    .df_to_markdown(df, digits)
  }

  # Save
  if (save) {
    if (is.null(path)) path <- getOption("dgme.tab_dir", "tables/")
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (is.null(filename)) {
      filename <- paste0(type, "_",
                         gsub("[^A-Za-z0-9_.-]", "_", params$label))
    }
    ext <- if (format == "latex") ".tex" else ".md"
    out_path <- file.path(path, paste0(filename, ext))
    writeLines(out, out_path)
    message("Table written to: ", normalizePath(out_path, mustWork = FALSE))
    return(invisible(out))
  }

  class(out) <- "dgme_table_output"
  out
}


# ---- Print method ------------------------------------------------------------

#' @export
print.dgme_table_output <- function(x, ...) {
  cat(x, sep = "\n")
  invisible(x)
}


# =============================================================================
# Table builders (return data.frames)
# =============================================================================

#' @keywords internal
.table_parameters <- function(params, digits) {
  e <- params$e
  r <- (1 - params$q) / params$q

  rows <- list(
    c("$q$ (bond price)",          formatC(params$q, digits = digits, format = "f")),
    c("$r$ (interest rate)",       formatC(r, digits = digits, format = "f")),
    c("$e^1$ (endowment h1)",     paste0("(", paste(formatC(e[1, ], digits = digits, format = "f"), collapse = ", "), ")")),
    c("$e^2$ (endowment h2)",     paste0("(", paste(formatC(e[2, ], digits = digits, format = "f"), collapse = ", "), ")")),
    c("$\\alpha^1$ (CD exp. h1)", formatC(params$alpha[1], digits = digits, format = "f")),
    c("$\\alpha^2$ (CD exp. h2)", formatC(params$alpha[2], digits = digits, format = "f")),
    c("$m^1$ (money h1)",         formatC(params$m[1], digits = digits, format = "f")),
    c("$m^2$ (money h2)",         formatC(params$m[2], digits = digits, format = "f")),
    c("$M$ (total money)",        formatC(sum(params$m), digits = digits, format = "f"))
  )

  # Variant 2 parameters
  if (!is.null(params$rho)) {
    G <- params$eta * (1 - params$rho) * params$S
    rows <- c(rows, list(
      c("$\\rho$ (reserve ratio)",       formatC(params$rho, digits = digits, format = "f")),
      c("$\\eta$ (pass-through)",        formatC(params$eta, digits = digits, format = "f")),
      c("$S$ (token issuance)",          formatC(params$S, digits = digits, format = "f")),
      c("$G = \\eta(1-\\rho)S$",         formatC(G, digits = digits, format = "f"))
    ))
  }

  # Variant 4 parameters
  if (!is.null(params$m_S)) {
    rows <- c(rows, list(
      c("$m^1_S$ (stablecoin h1)", formatC(params$m_S[1], digits = digits, format = "f")),
      c("$m^2_S$ (stablecoin h2)", formatC(params$m_S[2], digits = digits, format = "f")),
      c("$M_S$ (total stablecoin)", formatC(sum(params$m_S), digits = digits, format = "f"))
    ))
  }

  df <- data.frame(
    Parameter = sapply(rows, `[`, 1),
    Value     = sapply(rows, `[`, 2),
    stringsAsFactors = FALSE
  )
  df
}


#' @keywords internal
.table_equilibrium <- function(result, digits) {
  p   <- result$p
  x   <- result$x
  tau <- result$tau
  b   <- result$b
  nu  <- result$nu
  par <- result$params

  fmt <- function(v) formatC(v, digits = digits, format = "f")

  rows <- list(
    c("$\\bar p_1$",                 fmt(p[1])),
    c("$\\bar p_2$",                 fmt(p[2])),
    c("$\\bar x^1$",                 paste0("(", paste(fmt(x[1, ]), collapse = ", "), ")")),
    c("$\\bar x^2$",                 paste0("(", paste(fmt(x[2, ]), collapse = ", "), ")")),
    c("$\\bar \\tau^1$",             paste0("(", paste(fmt(tau[1, ]), collapse = ", "), ")")),
    c("$\\bar \\tau^2$",             paste0("(", paste(fmt(tau[2, ]), collapse = ", "), ")")),
    c("$\\bar b^1$",                 fmt(b[1])),
    c("$\\bar b^2$",                 fmt(b[2])),
    c("$\\bar D$",                   fmt(result$D)),
    c("$M + q\\bar D$",             fmt(result$payment_capacity)),
    c("$\\bar \\nu^1$",             paste0("(", paste(fmt(nu[1, ]), collapse = ", "), ")")),
    c("$\\bar \\nu^2$",             paste0("(", paste(fmt(nu[2, ]), collapse = ", "), ")")),
    c("$u^1(\\bar x^1)$",           fmt(result$utilities[1])),
    c("$u^2(\\bar x^2)$",           fmt(result$utilities[2])),
    c("$u^1(e^1)$ (autarky)",       fmt(result$autarky_utilities[1])),
    c("$u^2(e^2)$ (autarky)",       fmt(result$autarky_utilities[2]))
  )

  # Treasury extras
  if (!is.null(result$G)) {
    rows <- c(rows, list(
      c("$G$ (Treasury spending)",  fmt(result$G)),
      c("$M_{\\text{eff}}$",       fmt(sum(result$m_eff)))
    ))
  }

  # Competing extras
  if (!is.null(result$D_S)) {
    rows <- c(rows, list(
      c("$\\bar D_S$",             fmt(result$D_S)),
      c("$\\sigma^1$",             fmt(result$sigma[1])),
      c("$\\sigma^2$",             fmt(result$sigma[2]))
    ))
  }

  data.frame(
    Quantity = sapply(rows, `[`, 1),
    Value    = sapply(rows, `[`, 2),
    stringsAsFactors = FALSE
  )
}


#' @keywords internal
.table_comparison <- function(res_list, variants, digits) {
  fmt <- function(v) formatC(v, digits = digits, format = "f")

  # Row labels (common quantities)
  row_labels <- c(
    "$\\bar p_1$", "$\\bar p_2$",
    "$\\bar x^1_1$", "$\\bar x^1_2$",
    "$\\bar x^2_1$", "$\\bar x^2_2$",
    "$\\bar D$",
    "$M + q\\bar D$",
    "$u^1(\\bar x^1)$", "$u^2(\\bar x^2)$"
  )

  df <- data.frame(Quantity = row_labels, stringsAsFactors = FALSE)

  for (v in variants) {
    r <- res_list[[v]]
    col <- c(
      fmt(r$p[1]), fmt(r$p[2]),
      fmt(r$x[1, 1]), fmt(r$x[1, 2]),
      fmt(r$x[2, 1]), fmt(r$x[2, 2]),
      fmt(r$D),
      fmt(r$payment_capacity),
      fmt(r$utilities[1]), fmt(r$utilities[2])
    )
    df[[v]] <- col
  }

  df
}


#' @keywords internal
.table_indeterminacy <- function(res_list, variants, digits) {

  # Find the competing variant
  comp_v <- intersect(variants, "competing")
  if (length(comp_v) == 0) {
    stop("Indeterminacy table requires a 'competing' variant.")
  }

  r <- res_list[["competing"]]
  if (is.null(r$e_grid) || is.null(r$p_grid)) {
    stop("Competing variant has no exchange-rate grid results.")
  }

  fmt <- function(v) formatC(v, digits = digits, format = "f")

  n <- length(r$e_grid)
  df <- data.frame(
    e_bar = fmt(r$e_grid),
    p1    = fmt(r$p_grid[, 1]),
    p2    = fmt(r$p_grid[, 2]),
    D     = fmt(r$D_grid),
    D_S   = fmt(r$D_S_grid),
    stringsAsFactors = FALSE
  )

  # Add stablecoin prices p_S = e_bar * p
  df$p1_S <- fmt(r$e_grid * r$p_grid[, 1])
  df$p2_S <- fmt(r$e_grid * r$p_grid[, 2])

  # Column names for LaTeX
  names(df) <- c("$\\bar e$", "$\\bar p_1$", "$\\bar p_2$",
                  "$\\bar D$", "$\\bar D_S$",
                  "$\\bar p^S_1$", "$\\bar p^S_2$")
  df
}


# =============================================================================
# Formatters
# =============================================================================

#' @keywords internal
.df_to_latex <- function(df, caption, label, digits) {

  nc <- ncol(df)
  col_spec <- paste0("l", paste(rep("r", nc - 1), collapse = ""))

  lines <- character()
  a <- function(...) lines <<- c(lines, paste0(...))

  a("\\begin{table}[htbp]")
  a("  \\centering")
  a("  \\caption{", caption, "}")
  a("  \\label{", label, "}")
  a("  \\begin{tabular}{", col_spec, "}")
  a("    \\toprule")

  # Header
  a("    ", paste(names(df), collapse = " & "), " \\\\")
  a("    \\midrule")

  # Body
  for (i in seq_len(nrow(df))) {
    row_str <- paste(df[i, ], collapse = " & ")
    a("    ", row_str, " \\\\")
  }

  a("    \\bottomrule")
  a("  \\end{tabular}")
  a("\\end{table}")

  paste(lines, collapse = "\n")
}


#' @keywords internal
.df_to_markdown <- function(df, digits) {

  # Header
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep    <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")

  # Body
  rows <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    rows[i] <- paste0("| ", paste(df[i, ], collapse = " | "), " |")
  }

  paste(c(header, sep, rows), collapse = "\n")
}
