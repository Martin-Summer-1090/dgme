#' Generate publication-ready tables
#'
#' Produces LaTeX, Markdown, or data.frame tables for various model
#' outputs: equilibrium summaries, cross-variant comparisons,
#' comparative-statics sweeps, and exchange-rate indeterminacy.
#'
#' @param results A \code{dgme_results} or \code{dgme_result} object
#'   returned by \code{dgme_solve()}.
#' @param type Character.  Table type: \code{"equilibrium"},
#'   \code{"parameters"}, \code{"comparison"}, \code{"indeterminacy"},
#'   \code{"rho_sweep"}, \code{"S_sweep"}, or \code{"scenarios"}.
#' @param variants Character vector of variant names to include.
#' @param digits Integer.  Number of decimal places.
#' @param caption Character or \code{NULL}.  LaTeX caption.
#'   Auto-generated if \code{NULL}.
#' @param label Character or \code{NULL}.  LaTeX label.
#'   Auto-generated if \code{NULL}.
#' @param format Character.  Output format: \code{"latex"} (default),
#'   \code{"markdown"}, or \code{"data.frame"}.
#' @param save Logical.  Write to file?
#' @param path Character or \code{NULL}.  Directory.  Default:
#'   \code{getOption("dgme.tab_dir", "tables/")}.
#' @param filename Character or \code{NULL}.  File name (without extension).
#'   Auto-generated if \code{NULL}.
#' @param ... Additional arguments passed to sweep-table builders
#'   (e.g. \code{rho_grid}, \code{S_grid}, \code{scenarios}).
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
                                "comparison", "indeterminacy",
                                "rho_sweep", "S_sweep", "scenarios"),
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
    indeterminacy = .table_indeterminacy(res_list, variants, digits),
    rho_sweep     = .table_rho_sweep(params, digits, ...),
    S_sweep       = .table_S_sweep(params, digits, ...),
    scenarios     = .table_scenarios(params, digits, ...)
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
      indeterminacy = paste0("Exchange-rate indeterminacy with two outside monies. ",
                             "Sovereign prices $\\bar p$ vary with the exchange rate ",
                             "$\\bar{\\varepsilon}$; stablecoin prices $\\bar p^S = \\bar{\\varepsilon} ",
                             "\\cdot \\bar p$ adjust inversely. Relative prices and ",
                             "real allocations are constant across rows."),
      rho_sweep     = paste0("Effect of reserve composition ($\\rho$) on equilibrium ",
                             "prices. Each row varies the share of reserves held as ",
                             "settlement media ($\\rho$), with stablecoin issuance ",
                             "$S = ", formatC(params$S, format = "f", digits = 0),
                             "$ and fiscal pass-through $\\eta = ",
                             formatC(params$eta, format = "f", digits = 2),
                             "$. $G$: Treasury spending financed by stablecoin reserves. ",
                             "$\\bar p_1, \\bar p_2$: baseline commodity prices ",
                             "(no Treasury channel). ",
                             "$\\bar p^T_1, \\bar p^T_2$: commodity prices under the ",
                             "Treasury channel. ",
                             "$\\Delta p_1, \\Delta p_2$: percentage price increases ",
                             "due to the Treasury channel. ",
                             "PC, PC$^T$: aggregate payment capacity in the baseline ",
                             "and Treasury-channel equilibria, respectively."),
      S_sweep       = paste0("Effect of aggregate stablecoin issuance ($S$) on ",
                             "equilibrium prices. Each row varies the size of the ",
                             "stablecoin sector, with reserve composition $\\rho = ",
                             formatC(params$rho, format = "f", digits = 2),
                             "$ (",
                             round(100 * (1 - params$rho)),
                             "\\% of reserves invested in credit claims) and fiscal ",
                             "pass-through $\\eta = ",
                             formatC(params$eta, format = "f", digits = 2),
                             "$. $G$: Treasury spending financed by stablecoin ",
                             "reserves. $\\Delta p_1, \\Delta p_2$: percentage ",
                             "increases in commodity prices relative to the baseline ",
                             "(no Treasury channel). PC$^T$: aggregate payment ",
                             "capacity under the Treasury channel (baseline ",
                             "PC $= 5.5$ throughout)."),
      scenarios     = paste0("Calibrated stablecoin scenarios. Each row represents a ",
                             "stylised issuer profile defined by three parameters: ",
                             "the share of reserves held as settlement media ($\\rho$), ",
                             "the fiscal pass-through rate ($\\eta$), and aggregate ",
                             "stablecoin issuance ($S$). $G$: Treasury spending ",
                             "financed by stablecoin reserves. $\\Delta p_1, ",
                             "\\Delta p_2$: percentage increases in commodity prices ",
                             "relative to the baseline. PC$^T$: aggregate payment ",
                             "capacity under the Treasury channel. ",
                             "Baseline payment capacity is $M + q\\bar D = 5.50$ ",
                             "throughout. The ``fully sterilised'' scenario ($\\rho = 1$) ",
                             "keeps all reserves as settlement media; the ``full ",
                             "Ricardian'' scenario ($\\eta = 0$) assumes complete ",
                             "offset of issuer-financed sovereign borrowing. Both ",
                             "are neutral benchmarks.")
    )
  }
  if (is.null(label)) {
    label <- switch(type,
      rho_sweep     = "tab:rho_sweep",
      S_sweep       = "tab:S_sweep",
      scenarios     = "tab:scenarios",
      indeterminacy = "tab:indeterminacy",
      paste0("tab:", type, "_",
             gsub("[^A-Za-z0-9]", "_", params$label))
    )
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
      filename <- switch(type,
        rho_sweep = "rho_sweep",
        S_sweep   = "S_sweep",
        scenarios = "scenarios",
        indeterminacy = "indeterminacy",
        paste0(type, "_",
               gsub("[^A-Za-z0-9_.-]", "_", params$label))
      )
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
    c("$m^1$",                     formatC(params$m[1], digits = digits, format = "f")),
    c("$m^2$",                     formatC(params$m[2], digits = digits, format = "f")),
    c("$M$",                       formatC(sum(params$m), digits = digits, format = "f"))
  )

  if (!is.null(params$rho)) {
    rows <- c(rows, list(
      c("$\\rho$ (reserve comp.)", formatC(params$rho, digits = digits, format = "f")),
      c("$\\eta$ (fiscal pass-through)", formatC(params$eta, digits = digits, format = "f")),
      c("$S$ (issuance)",          formatC(params$S, digits = digits, format = "f"))
    ))
  }

  if (!is.null(params$m_S)) {
    rows <- c(rows, list(
      c("$m^1_S$ (stablecoin money h1)", formatC(params$m_S[1], digits = digits, format = "f")),
      c("$m^2_S$ (stablecoin money h2)", formatC(params$m_S[2], digits = digits, format = "f")),
      c("$M_S$ (stablecoin aggregate)",  formatC(sum(params$m_S), digits = digits, format = "f"))
    ))
  }

  data.frame(
    Parameter = sapply(rows, `[`, 1),
    Value     = sapply(rows, `[`, 2),
    stringsAsFactors = FALSE
  )
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

  fmt  <- function(v) formatC(v, digits = digits, format = "f")
  fmt3 <- function(v) formatC(v, digits = 3, format = "f")

  # Compute M_tilde = M + M_S / e_bar for each grid point
  params <- r$params
  M   <- sum(params$m)
  M_S <- sum(params$m_S)

  # Real allocations are invariant (Prop 4(iii)) — use the reference x
  x_ref <- r$x

  n <- length(r$e_grid)

  # Build data frame without row names
  rows <- lapply(seq_len(n), function(i) {
    ebar    <- r$e_grid[i]
    M_tilde <- M + M_S / ebar
    data.frame(
      ebar    = fmt(ebar),
      M_tilde = fmt3(M_tilde),
      p1      = fmt3(r$p_grid[i, 1]),
      p2      = fmt3(r$p_grid[i, 2]),
      p1_S    = fmt3(ebar * r$p_grid[i, 1]),
      p2_S    = fmt3(ebar * r$p_grid[i, 2]),
      p2_p1   = fmt3(r$p_grid[i, 2] / r$p_grid[i, 1]),
      x1_1    = fmt(x_ref[1, 1]),
      x1_2    = fmt(x_ref[1, 2]),
      x2_1    = fmt(x_ref[2, 1]),
      x2_2    = fmt(x_ref[2, 2]),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  rownames(df) <- NULL

  names(df) <- c("$\\bar{\\varepsilon}$", "$\\tilde{M}$",
                  "$\\bar p_1$", "$\\bar p_2$",
                  "$\\bar p^S_1$", "$\\bar p^S_2$",
                  "$\\bar p_2/\\bar p_1$",
                  "$\\bar x^1_1$", "$\\bar x^1_2$",
                  "$\\bar x^2_1$", "$\\bar x^2_2$")
  df
}


# =============================================================================
# Sweep table builders (new)
# =============================================================================

#' @keywords internal
.table_rho_sweep <- function(params, digits, ...) {
  dots <- list(...)
  rho_grid <- if (!is.null(dots$rho_grid)) dots$rho_grid else c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0)

  fmt  <- function(v) formatC(v, digits = 2, format = "f")
  fmt1 <- function(v) formatC(v, digits = 1, format = "f")

  rows <- lapply(rho_grid, function(rho_val) {
    p_i <- dgme_parametrize(params$label, rho = rho_val,
                            eta = params$eta, S = params$S,
                            e = params$e, alpha = params$alpha,
                            m = params$m, q = params$q,
                            m_S = params$m_S)
    r_i <- dgme_solve(p_i, variants = c("baseline", "treasury"))
    bl  <- r_i$results$baseline
    tr  <- r_i$results$treasury
    G   <- p_i$eta * (1 - rho_val) * p_i$S

    data.frame(
      rho    = fmt(rho_val),
      G      = fmt(G),
      p1     = fmt(bl$p[1]),
      p2     = fmt(bl$p[2]),
      p1T    = fmt(tr$p[1]),
      p2T    = fmt(tr$p[2]),
      dp1    = fmt(100 * (tr$p[1] / bl$p[1] - 1)),
      dp2    = fmt(100 * (tr$p[2] / bl$p[2] - 1)),
      PC     = fmt1(bl$payment_capacity),
      PCT    = fmt1(tr$payment_capacity),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  rownames(df) <- NULL

  names(df) <- c("$\\rho$", "$G$",
                  "$\\bar p_1$", "$\\bar p_2$",
                  "$\\bar p^T_1$", "$\\bar p^T_2$",
                  "$\\Delta p_1$ (\\%)", "$\\Delta p_2$ (\\%)",
                  "PC", "PC$^T$")
  df
}


#' @keywords internal
.table_S_sweep <- function(params, digits, ...) {
  dots <- list(...)
  S_grid <- if (!is.null(dots$S_grid)) dots$S_grid else c(0, 0.5, 1.0, 2.0, 3.0, 5.0)

  fmt  <- function(v) formatC(v, digits = 2, format = "f")
  fmt1 <- function(v) formatC(v, digits = 1, format = "f")

  rows <- lapply(S_grid, function(S_val) {
    p_i <- dgme_parametrize(params$label, rho = params$rho,
                            eta = params$eta, S = S_val,
                            e = params$e, alpha = params$alpha,
                            m = params$m, q = params$q,
                            m_S = params$m_S)
    r_i <- dgme_solve(p_i, variants = c("baseline", "treasury"))
    bl  <- r_i$results$baseline
    tr  <- r_i$results$treasury
    G   <- p_i$eta * (1 - p_i$rho) * S_val

    data.frame(
      S      = fmt1(S_val),
      G      = fmt(G),
      dp1    = fmt(100 * (tr$p[1] / bl$p[1] - 1)),
      dp2    = fmt(100 * (tr$p[2] / bl$p[2] - 1)),
      PCT    = fmt1(tr$payment_capacity),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  rownames(df) <- NULL

  names(df) <- c("$S$", "$G$",
                  "$\\Delta p_1$ (\\%)", "$\\Delta p_2$ (\\%)",
                  "PC$^T$")
  df
}


#' @keywords internal
.table_scenarios <- function(params, digits, ...) {
  dots <- list(...)

  # Default scenarios matching the paper
  if (!is.null(dots$scenarios)) {
    scenarios <- dots$scenarios
  } else {
    scenarios <- data.frame(
      label = c("Tether-like", "Circle-like", "Fully sterilised", "Full Ricardian"),
      rho   = c(0.05,          0.12,          1.00,               0.10),
      eta   = c(0.80,          0.80,          0.80,               0.00),
      S     = c(2.00,          1.00,          1.00,               1.00),
      stringsAsFactors = FALSE
    )
  }

  fmt  <- function(v) formatC(v, digits = 2, format = "f")
  fmt1 <- function(v) formatC(v, digits = 1, format = "f")

  rows <- lapply(seq_len(nrow(scenarios)), function(i) {
    sc <- scenarios[i, ]
    p_i <- dgme_parametrize(params$label, rho = sc$rho,
                            eta = sc$eta, S = sc$S,
                            e = params$e, alpha = params$alpha,
                            m = params$m, q = params$q,
                            m_S = params$m_S)
    r_i <- dgme_solve(p_i, variants = c("baseline", "treasury"))
    bl  <- r_i$results$baseline
    tr  <- r_i$results$treasury
    G   <- sc$eta * (1 - sc$rho) * sc$S

    data.frame(
      Scenario = sc$label,
      rho      = fmt(sc$rho),
      eta      = fmt1(sc$eta),
      S        = fmt1(sc$S),
      G        = fmt(G),
      dp1      = fmt(100 * (tr$p[1] / bl$p[1] - 1)),
      dp2      = fmt(100 * (tr$p[2] / bl$p[2] - 1)),
      PCT      = fmt1(tr$payment_capacity),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  rownames(df) <- NULL

  names(df) <- c("Scenario", "$\\rho$", "$\\eta$", "$S$", "$G$",
                  "$\\Delta p_1$ (\\%)", "$\\Delta p_2$ (\\%)",
                  "PC$^T$")
  df
}


# =============================================================================
# Formatters
# =============================================================================

#' @keywords internal
.df_to_latex <- function(df, caption, label, digits) {

  nc <- ncol(df)
  # Determine column alignment: first col left-aligned, rest right-aligned
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

  # Body — explicitly suppress row names by indexing columns only
  for (i in seq_len(nrow(df))) {
    vals <- vapply(seq_len(nc), function(j) as.character(df[i, j]),
                   character(1))
    row_str <- paste(vals, collapse = " & ")
    a("    ", row_str, " \\\\")
  }

  a("    \\bottomrule")
  a("  \\end{tabular}")
  a("\\end{table}")

  paste(lines, collapse = "\n")
}


#' @keywords internal
.df_to_markdown <- function(df, digits) {

  nc <- ncol(df)

  # Header
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep    <- paste0("|", paste(rep("---", nc), collapse = "|"), "|")

  # Body — explicitly suppress row names
  rows <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    vals <- vapply(seq_len(nc), function(j) as.character(df[i, j]),
                   character(1))
    rows[i] <- paste0("| ", paste(vals, collapse = " | "), " |")
  }

  paste(c(header, sep, rows), collapse = "\n")
}
