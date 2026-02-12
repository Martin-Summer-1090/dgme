#' Generate TikZ Figures for DG Monetary Equilibria
#'
#' Write standalone TikZ \code{.tex} files for the NA-equilibrium in
#' net-trade space and/or the ME-equilibrium in nominal-income-transfer
#' space.  These functions take a \code{dgme_result} object, compute
#' the geometry via \code{\link{dgme_geometry}}, and produce publication-
#' quality TikZ code following the visual conventions of the paper.
#'
#' @name dgme_tikz
NULL


# ===========================================================================
# Shared helpers
# ===========================================================================

.n6 <- function(x) sprintf("%.6f", as.numeric(x))
.n3 <- function(x) sprintf("%.3f", as.numeric(x))


# ===========================================================================
# NA-REAL: Net-trade-space figure
# ===========================================================================

#' @rdname dgme_tikz
#'
#' @param result A \code{dgme_result} object.
#' @param households Integer vector.  Which households to display:
#'   \code{c(1, 2)} for the full equilibrium, \code{1} or \code{2} for
#'   a single-household panel (for the paper main text).
#' @param geom Optional pre-computed \code{dgme_geometry} object.  If
#'   \code{NULL}, computed automatically.
#' @param out_dir Character.  Directory for the output \code{.tex} file.
#' @param file_name Character or \code{NULL}.  File name (without extension).
#'   If \code{NULL}, an auto-generated name is used.
#' @param max_width_cm Numeric.  Target figure width.
#' @param stripes_n Integer.  Number of IC stripes.
#' @param stripes_step Numeric.  Step between stripes.
#' @param write Logical.  If \code{TRUE} (default), write the file.
#'   If \code{FALSE}, return the TikZ string without writing.
#'
#' @return A list with \code{path} (file path) and \code{tex} (the TikZ
#'   string), invisibly.
#' @export
dgme_tikz_na_real <- function(result,
                               households   = c(1, 2),
                               geom         = NULL,
                               out_dir      = "~/Projects/Research_Projects/Money_and_Inflation_Theory/shared/figures",
                               file_name    = NULL,
                               max_width_cm = 12,
                               stripes_n    = 9L,
                               stripes_step = 0.03,
                               write        = TRUE) {

  stopifnot(inherits(result, "dgme_result"))
  households <- as.integer(households)
  stopifnot(all(households %in% 1:2))

  if (is.null(geom)) {
    geom <- dgme_geometry(result, max_width_cm = max_width_cm,
                          stripes_n = stripes_n, stripes_step = stripes_step)
  }

  na    <- geom$na_real
  par   <- geom$params
  p     <- result$p
  q     <- par$q
  e     <- par$e
  x     <- result$x
  alpha <- par$alpha
  m     <- par$m

  n6 <- .n6

  # --- Unpack window ---
  w <- na$window; sc <- na$scale

  # --- Colour scheme ---
  col <- list(
    h1_line = "blue!65!black",  h1_fill = "blue!75!black", h1_bg = "blue!60",
    h2_line = "red!80!black",   h2_fill = "red!80!black",  h2_bg = "red!70"
  )

  # ===== Build TikZ string =====

  tex <- character()
  a <- function(...) tex <<- c(tex, paste0(...))

  # --- Preamble ---
  a("\\documentclass[tikz]{standalone}")
  a("\\usepackage{lmodern}")
  a("\\usepackage{amsmath}")
  a("\\usetikzlibrary{arrows.meta,calc,backgrounds}")
  a("\\begin{document}")
  a("")

  # --- Parameters as pgfmath macros ---
  a("% ===== Inputs =====")
  a(sprintf("\\pgfmathsetmacro{\\pOne}{%s}", n6(p[1])))
  a(sprintf("\\pgfmathsetmacro{\\pTwo}{%s}", n6(p[2])))
  a(sprintf("\\pgfmathsetmacro{\\q}{%s}", n6(q)))

  for (h in households) {
    tag <- if (h == 1) "A" else "B"
    a(sprintf("\\pgfmathsetmacro{\\eOne%s}{%s}", tag, n6(e[h, 1])))
    a(sprintf("\\pgfmathsetmacro{\\eTwo%s}{%s}", tag, n6(e[h, 2])))
    a(sprintf("\\pgfmathsetmacro{\\m%s}{%s}", tag, n6(m[h])))
    a(sprintf("\\pgfmathsetmacro{\\alpha%s}{%s}", tag, n6(alpha[h])))
    a(sprintf("\\pgfmathsetmacro{\\xOne%s}{%s}", tag, n6(x[h, 1])))
    a(sprintf("\\pgfmathsetmacro{\\xTwo%s}{%s}", tag, n6(x[h, 2])))
  }
  a("")

  # --- Budget kink points (x-space) ---
  a("% original budget points (x-space)")
  for (h in households) {
    tag <- if (h == 1) "one" else "two"
    Tag <- if (h == 1) "A" else "B"
    kx <- na$kinks_x[[h]]
    for (nm in c("A", "B", "C", "D")) {
      pt <- kx[[nm]]
      a(sprintf("\\pgfmathsetmacro{\\%s%sX}{%s}\\pgfmathsetmacro{\\%s%sY}{%s}",
                nm, tag, n6(pt[1]), nm, tag, n6(pt[2])))
    }
  }
  a("")

  # --- Tau-space translation ---
  a("% tau-space translation")
  for (h in households) {
    tag <- if (h == 1) "one" else "two"
    Tag <- if (h == 1) "A" else "B"
    for (nm in c("A", "B", "C", "D")) {
      a(sprintf("\\pgfmathsetmacro{\\%s%sXt}{\\%s%sX-\\eOne%s} \\pgfmathsetmacro{\\%s%sYt}{\\%s%sY-\\eTwo%s}",
                nm, tag, nm, tag, Tag, nm, tag, nm, tag, Tag))
    }
    a(sprintf("\\pgfmathsetmacro{\\tOne%s}{\\xOne%s-\\eOne%s} \\pgfmathsetmacro{\\tTwo%s}{\\xTwo%s-\\eTwo%s}",
              Tag, Tag, Tag, Tag, Tag, Tag))
  }
  a("")

  # --- Window and scale ---
  a("% window, scale, clamps")
  a(sprintf("\\pgfmathsetmacro{\\Xmin}{%s}", n6(w$Xmin)))
  a(sprintf("\\pgfmathsetmacro{\\Xmax}{%s}", n6(w$Xmax)))
  a(sprintf("\\pgfmathsetmacro{\\Ymin}{%s}", n6(w$Ymin)))
  a(sprintf("\\pgfmathsetmacro{\\Ymax}{%s}", n6(w$Ymax)))
  a(sprintf("\\pgfmathsetmacro{\\Unit}{%s}", n6(sc$Unit)))
  a(sprintf("\\pgfmathsetmacro{\\AxisBand}{%s}", n6(sc$axis_band)))
  a(sprintf("\\pgfmathsetmacro{\\Ylo}{\\Ymin-%s}", n6(0.5)))
  a(sprintf("\\pgfmathsetmacro{\\Yhi}{\\Ymax+%s}", n6(0.5)))
  a(sprintf("\\pgfmathsetmacro{\\StripeStep}{%s}", n6(stripes_step)))
  a("")

  # --- IC parameters ---
  a("% CD IC params in tau-space (correct C from U)")
  for (h in households) {
    Tag <- if (h == 1) "A" else "B"
    a(sprintf("\\pgfmathsetmacro{\\eta%s}{-(\\alpha%s)/(1-\\alpha%s)}", Tag, Tag, Tag))
    a(sprintf("\\pgfmathsetmacro{\\U%s}{pow(\\xOne%s,\\alpha%s)*pow(\\xTwo%s,1-\\alpha%s)}", Tag, Tag, Tag, Tag, Tag))
    a(sprintf("\\pgfmathsetmacro{\\C%s}{pow(\\U%s,1/(1-\\alpha%s))}", Tag, Tag, Tag))
  }
  a("")

  # --- Safe left bounds ---
  a("% safe left bounds for base ICs")
  for (h in households) {
    Tag <- if (h == 1) "A" else "B"
    a(sprintf("\\pgfmathsetmacro{\\BaseLeft%s}{ pow( max( (\\Yhi+\\eTwo%s)/\\C%s, 1e-6 ), 1.0/\\eta%s ) - \\eOne%s }",
              Tag, Tag, Tag, Tag, Tag))
    a(sprintf("\\pgfmathsetmacro{\\tmin%s}{ max( \\Xmin, \\BaseLeft%s ) }", Tag, Tag))
  }
  a("")

  # --- Price normal helpers ---
  a("% price-normal helpers (face-dependent effective prices)")
  for (h in households) {
    Tag <- if (h == 1) "A" else "B"
    a(sprintf("\\pgfmathtruncatemacro{\\%sleft}{ifthenelse((\\xOne%s<=\\eOne%s) && (\\xTwo%s>=\\eTwo%s),1,0)}",
              Tag, Tag, Tag, Tag, Tag))
    a(sprintf("\\pgfmathtruncatemacro{\\%smid}{ ifthenelse((\\xOne%s>=\\eOne%s) && (\\xTwo%s>=\\eTwo%s),1,0)}",
              Tag, Tag, Tag, Tag, Tag))
    a(sprintf("\\pgfmathtruncatemacro{\\%sright}{ifthenelse((\\xOne%s>=\\eOne%s) && (\\xTwo%s<=\\eTwo%s),1,0)}",
              Tag, Tag, Tag, Tag, Tag))
    a(sprintf("\\pgfmathsetmacro{\\v%sx}{\\pOne*(\\%sleft*\\q + (\\%smid+\\%sright))}",
              Tag, Tag, Tag, Tag))
    a(sprintf("\\pgfmathsetmacro{\\v%sy}{\\pTwo*(\\%sleft + \\%smid + \\%sright*\\q)}",
              Tag, Tag, Tag, Tag))
  }
  a("\\pgfmathsetmacro{\\MinXY}{min(\\Xmax-\\Xmin,\\Ymax-\\Ymin)}")
  a("\\pgfmathsetmacro{\\Lnorm}{0.18*\\MinXY}")
  for (h in households) {
    Tag <- if (h == 1) "A" else "B"
    a(sprintf("\\pgfmathsetmacro{\\s%s}{\\Lnorm / sqrt(\\v%sx*\\v%sx + \\v%sy*\\v%sy)}",
              Tag, Tag, Tag, Tag, Tag))
  }
  a("")

  # --- Price labels (regime-dependent) ---
  a("% price labels")
  for (h in households) {
    Tag <- if (h == 1) "A" else "B"
    a(sprintf("\\def\\priceLabel%s{(\\bar p_1,\\ \\bar p_2)}%%", Tag))
    a(sprintf("\\ifnum\\%sleft=1  \\def\\priceLabel%s{(\\bar q\\,\\bar p_1,\\ \\bar p_2)}\\fi",
              Tag, Tag))
    a(sprintf("\\ifnum\\%sright=1 \\def\\priceLabel%s{(\\bar p_1,\\ \\bar q\\,\\bar p_2)}\\fi",
              Tag, Tag))
  }
  a("")

  # --- Tau sign flags ---
  a("% sign selection for tau superscripts")
  for (h in households) {
    Tag <- if (h == 1) "A" else "B"
    a(sprintf("\\def\\tau%soneSign{+} \\ifdim \\tOne%s pt < 0pt \\def\\tau%soneSign{-} \\fi",
              Tag, Tag, Tag))
    a(sprintf("\\def\\tau%stwoSign{+} \\ifdim \\tTwo%s pt < 0pt \\def\\tau%stwoSign{-} \\fi",
              Tag, Tag, Tag))
  }
  a("")

  # ===== Begin tikzpicture =====
  a("\\begin{tikzpicture}[x=\\Unit cm, y=\\Unit cm]")
  a("")

  # --- Global tau-axes ---
  a("  % === global \\tau-axes (double-headed) ===")
  a("  \\draw[thin,<->] (\\Xmin,0) -- (\\Xmax,0);")
  a("  \\draw[thin,<->] (0,\\Ymin) -- (0,\\Ymax);")
  a("  \\node[below right, font=\\scriptsize] at (\\Xmax,0) {$\\tau_1^{+}$};")
  a("  \\node[below left,  font=\\scriptsize] at (\\Xmin,0) {$\\tau_1^{-}$};")
  a("  \\node[above left,  font=\\scriptsize] at (0,\\Ymax) {$\\tau_2^{+}$};")
  a("  \\node[below left,  font=\\scriptsize] at (0,\\Ymin) {$\\tau_2^{-}$};")
  a("")

  # --- Consumption-space origins ---
  a("  % === consumption-space origins in \\tau: (-e^i_1, -e^i_2) ===")
  for (h in households) {
    Tag <- if (h == 1) "A" else "B"
    ltag <- tolower(Tag)
    a(sprintf("  \\pgfmathsetmacro{\\%sx}{-\\eOne%s} \\pgfmathsetmacro{\\%sy}{-\\eTwo%s}",
              ltag, Tag, ltag, Tag))
    a(sprintf("  \\pgfmathsetmacro{\\%sstartX}{max(\\Xmin,\\%sx)} \\pgfmathsetmacro{\\%sstartY}{max(\\Ymin,\\%sy)}",
              ltag, ltag, ltag, ltag))
  }
  a("")

  # Mapping from household index to variable prefix (a/b) and colour
  hh_map <- list(
    list(var = "a", Tag = "A", tag = "one", sup = "1",
         col_line = "blue!70!black", col_fill = "blue!75!black",
         col_bg = "blue!60", col_kink = "blue!65!black"),
    list(var = "b", Tag = "B", tag = "two", sup = "2",
         col_line = "red!80!black", col_fill = "red!80!black",
         col_bg = "red!70", col_kink = "red!80!black")
  )

  # --- Background shading ---
  a("  % === background shading ===")
  a("  \\begin{scope}[on background layer]")
  for (h in households) {
    hm <- hh_map[[h]]
    a(sprintf("    \\fill[%s, opacity=.08] (\\%sstartX, \\%sy) rectangle (\\Xmax, \\%sy+\\AxisBand);",
              hm$col_bg, hm$var, hm$var, hm$var))
    a(sprintf("    \\fill[%s, opacity=.08] (\\%sx, \\%sstartY) rectangle (\\%sx+\\AxisBand, \\Ymax);",
              hm$col_bg, hm$var, hm$var, hm$var))
  }
  a("  \\end{scope}")
  a("")

  # --- Consumption axes (coloured) ---
  a("  % === consumption axes (colored) ===")
  for (h in households) {
    hm <- hh_map[[h]]
    a(sprintf("  \\draw[->, thin, draw=%s] (\\%sx,\\%sy) -- (\\Xmax,\\%sy) node[below right, font=\\scriptsize, text=%s] {$x_1^{%s}$};",
              hm$col_line, hm$var, hm$var, hm$var, hm$col_line, hm$sup))
    a(sprintf("  \\draw[->, thin, draw=%s] (\\%sx,\\%sy) -- (\\%sx,\\Ymax) node[above left,  font=\\scriptsize, text=%s] {$x_2^{%s}$};",
              hm$col_line, hm$var, hm$var, hm$var, hm$col_line, hm$sup))
    a(sprintf("  \\fill[%s] (\\%sx,\\%sy) circle (1.3pt);",
              hm$col_line, hm$var, hm$var))
    a(sprintf("  \\node[font=\\scriptsize, text=%s, below right=2pt] at (\\%sx,\\%sy) {$-e^{%s}$};",
              hm$col_line, hm$var, hm$var, hm$sup))
  }
  a("")

  # --- Budget frontiers and ICs (clipped) ---
  a("  % === tau-budget frontiers & ICs ===")

  # Kink coordinates
  for (h in households) {
    hm <- hh_map[[h]]
    kt <- na$kinks_tau[[h]]
    for (nm in c("A", "B", "C", "D")) {
      a(sprintf("  \\coordinate (%s%d) at (%s,%s);",
                nm, h, n6(kt[[nm]][1]), n6(kt[[nm]][2])))
    }
  }
  a("")
  a("  \\begin{scope}")
  a("    \\clip (\\Xmin,\\Ymin) rectangle (\\Xmax,\\Ymax);")
  a("")

  # IC stripes
  a(sprintf("    %% ===== utility stripes ====="))
  a(sprintf("    \\foreach \\k in {1,...,%d} {", stripes_n))
  for (h in households) {
    hm <- hh_map[[h]]
    a(sprintf("      \\pgfmathsetmacro{\\C%sk}{\\C%s*(1+\\StripeStep*\\k)}",
              hm$Tag, hm$Tag))
    a(sprintf("      \\pgfmathsetmacro{\\Left%sk}{ max( \\Xmin, pow( max( (\\Yhi+\\eTwo%s)/\\C%sk, 1e-6 ), 1.0/\\eta%s ) - \\eOne%s ) }",
              hm$Tag, hm$Tag, hm$Tag, hm$Tag, hm$Tag))
    a(sprintf("      \\draw[draw=%s, opacity=.10, line width=.6pt, line cap=round]",
              hm$col_kink))
    a(sprintf("        plot[domain=\\Left%sk:\\Xmax, samples=180]", hm$Tag))
    a(sprintf("        ({\\x},{ min( max( \\C%sk * pow(\\x + \\eOne%s,\\eta%s) - \\eTwo%s, \\Ylo), \\Yhi ) });",
              hm$Tag, hm$Tag, hm$Tag, hm$Tag))
  }
  a("    }")
  a("")

  # Base ICs
  a("    % ===== Base ICs =====")
  for (h in households) {
    hm <- hh_map[[h]]
    a(sprintf("    \\draw[draw=%s, line width=.7pt]", hm$col_fill))
    a(sprintf("      plot[domain=\\tmin%s:\\Xmax, samples=220]", hm$Tag))
    a(sprintf("      ({\\x},{ min( max( \\C%s * pow(\\x + \\eOne%s,\\eta%s) - \\eTwo%s, \\Ylo), \\Yhi ) });",
              hm$Tag, hm$Tag, hm$Tag, hm$Tag))
  }
  a("")

  # Budget frontiers
  a("    % tau-budget frontiers (thin)")
  for (h in households) {
    hm <- hh_map[[h]]
    a(sprintf("    \\draw[thin, %s] (A%d) -- (B%d) -- (C%d) -- (D%d);",
              hm$col_kink, h, h, h, h))
  }
  a("  \\end{scope}")
  a("")

  # --- Price normals at optima ---
  a("  % === price normals at optima ===")
  for (h in households) {
    hm <- hh_map[[h]]
    a(sprintf("  \\pgfmathsetmacro{\\s%sp}{\\Lnorm / sqrt(\\v%sx*\\v%sx + \\v%sy*\\v%sy)}",
              hm$Tag, hm$Tag, hm$Tag, hm$Tag, hm$Tag))
    a(sprintf("  \\coordinate (N%send) at ({\\tOne%s + \\s%sp*\\v%sx},{\\tTwo%s + \\s%sp*\\v%sy});",
              hm$Tag, hm$Tag, hm$Tag, hm$Tag, hm$Tag, hm$Tag, hm$Tag))
    a(sprintf("  \\draw[-Latex, line width=.6pt, draw=%s] (\\tOne%s,\\tTwo%s) -- (N%send)",
              hm$col_line, hm$Tag, hm$Tag, hm$Tag))
    a(sprintf("    node[font=\\scriptsize\\itshape, text=%s, pos=.85, above=2pt, sloped] {$\\priceLabel%s$};",
              hm$col_line, hm$Tag))
  }
  a("")

  # --- Dashed projections to consumption axes ---
  a("  % === dashed projections to consumption axes ===")
  for (h in households) {
    hm <- hh_map[[h]]
    a(sprintf("  \\draw[densely dashed, thin] (\\tOne%s,\\tTwo%s) -- (\\tOne%s,\\%sy);",
              hm$Tag, hm$Tag, hm$Tag, hm$var))
    a(sprintf("  \\draw[densely dashed, thin] (\\tOne%s,\\tTwo%s) -- (\\%sx,\\tTwo%s);",
              hm$Tag, hm$Tag, hm$var, hm$Tag))
    a(sprintf("  \\draw[draw=%s, fill=white, line width=.6pt] (\\tOne%s,\\%sy) circle (1.7pt);",
              hm$col_line, hm$Tag, hm$var))
    a(sprintf("  \\draw[draw=%s, fill=white, line width=.6pt] (\\%sx,\\tTwo%s) circle (1.7pt);",
              hm$col_line, hm$var, hm$Tag))
    a(sprintf("  \\node[font=\\scriptsize, text=%s, below=2pt] at (\\tOne%s,\\%sy) {$\\bar{x}_1^{%s}$};",
              hm$col_line, hm$Tag, hm$var, hm$sup))
    a(sprintf("  \\node[font=\\scriptsize, text=%s, left=2pt]  at (\\%sx,\\tTwo%s) {$\\bar{x}_2^{%s}$};",
              hm$col_line, hm$var, hm$Tag, hm$sup))
  }
  a("")

  # --- Dashed projections to global tau-axes ---
  a("  % === dashed projections to global tau-axes ===")
  for (h in households) {
    hm <- hh_map[[h]]
    a(sprintf("  \\draw[densely dashed, thin] (\\tOne%s,\\tTwo%s) -- (\\tOne%s,0);",
              hm$Tag, hm$Tag, hm$Tag))
    a(sprintf("  \\draw[densely dashed, thin] (\\tOne%s,\\tTwo%s) -- (0,\\tTwo%s);",
              hm$Tag, hm$Tag, hm$Tag))
    a(sprintf("  \\draw[draw=%s, fill=white, line width=.5pt] (\\tOne%s,0) circle (1.5pt);",
              hm$col_line, hm$Tag))
    a(sprintf("  \\draw[draw=%s, fill=white, line width=.5pt] (0,\\tTwo%s) circle (1.5pt);",
              hm$col_line, hm$Tag))
    a(sprintf("  \\node[font=\\scriptsize, text=%s, below=2pt] at (\\tOne%s,0) {$\\bar{\\tau}_1^{\\tau%soneSign}$};",
              hm$col_line, hm$Tag, hm$Tag))
    a(sprintf("  \\node[font=\\scriptsize, text=%s, left=2pt]  at (0,\\tTwo%s) {$\\bar{\\tau}_2^{\\tau%stwoSign}$};",
              hm$col_line, hm$Tag, hm$Tag))
  }
  a("")

  # --- Kink markers and labels ---
  a("  % === kinks & optima markers ===")
  greek <- c("\\alpha", "\\beta", "\\gamma", "\\delta")
  for (h in households) {
    hm <- hh_map[[h]]
    a(sprintf("  \\foreach \\pt in {A%d,B%d,C%d,D%d} \\draw[draw=%s, fill=white, line width=.5pt] (\\pt) circle (1.5pt);",
              h, h, h, h, hm$col_kink))
    for (k in 1:4) {
      nm <- c("A", "B", "C", "D")[k]
      a(sprintf("  \\node[font=\\scriptsize, text=%s, above=2pt] at (%s%d) {$%s^{%s}$};",
                hm$col_kink, nm, h, greek[k], hm$sup))
    }
  }
  a("  % optima markers")
  for (h in households) {
    hm <- hh_map[[h]]
    a(sprintf("  \\fill[draw=white, line width=.4pt, fill=%s] (\\tOne%s,\\tTwo%s) circle (1.8pt);",
              hm$col_fill, hm$Tag, hm$Tag))
  }
  a("")

  a("\\end{tikzpicture}")
  a("\\end{document}")

  tex_str <- paste(tex, collapse = "\n")

  # --- Write ---
  if (write) {
    if (is.null(file_name)) {
      hh_str <- paste0("h", paste(households, collapse = ""))
      file_name <- sprintf("na_real_%s_%s", hh_str, par$label)
      file_name <- gsub("[^A-Za-z0-9_.-]", "_", file_name)
    }
    out_dir <- path.expand(out_dir)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(out_dir, paste0(file_name, ".tex"))
    writeLines(tex_str, out_path)
    message("TEX: ", normalizePath(out_path, winslash = "/", mustWork = FALSE))
  } else {
    out_path <- NULL
  }

  invisible(list(path = out_path, tex = tex_str))
}


# ===========================================================================
# ME-NOMINAL: Income-transfer-space figure  (revised)
# ===========================================================================
#
# Key changes vs. previous version:
#   - Font: \scriptsize throughout (was \tiny + mathlabel scale=0.55)
#   - Marketed cone C(q): gray!70, very thick, extends to y-axis
#     boundaries via cone_pos / cone_neg (was: to window corners)
#   - No C(q) label on the ray; feasibility label F^h_p placed inside
#     each trapezoid instead
#   - Projection labels: \bar{\nu}_0^{h}, \bar{\nu}_1^{h} with
#     generic superscript in single-hh mode
#   - D vector and dashed bank extension only in two-hh (appendix) mode
#
# REQUIRES: the revised .compute_me_geometry() which returns
#   cone_pos, cone_neg, label_pos (replaces cone_end, cone_bank)
# ===========================================================================

#' @rdname dgme_tikz
#'
#' @param show_dual_cone Logical.  If \code{TRUE}, overlay the normal
#'   (dual) cone \eqn{\mathcal{C}(\bar q)^{*}} on the figure.
#'   Defaults to \code{FALSE}.
#' @param show_bank Logical.  If \code{TRUE} (default), draw the
#'   dashed extension of the marketed cone representing the bank's
#'   counterposition and the aggregate bank vector \eqn{D}.
#'   Only effective when \code{length(households) == 2}.
#'
#' @export
dgme_tikz_me_nominal <- function(result,
                                  households     = c(1, 2),
                                  geom           = NULL,
                                  out_dir        = "~/Projects/Research_Projects/Money_and_Inflation_Theory/shared/figures",
                                  file_name      = NULL,
                                  show_dual_cone = FALSE,
                                  show_bank      = TRUE,
                                  write          = TRUE) {

  stopifnot(inherits(result, "dgme_result"))
  households <- as.integer(households)
  stopifnot(all(households %in% 1:2))

  if (is.null(geom)) {
    geom <- dgme_geometry(result)
  }

  me    <- geom$me_nominal
  par   <- geom$params
  p     <- result$p
  q     <- par$q
  e     <- par$e
  m     <- par$m
  b     <- result$b
  D     <- result$D
  M     <- sum(m)
  p1    <- p[1]; p2 <- p[2]

  n6 <- .n6

  single_hh <- (length(households) == 1)

  # Household display mapping
  # Single-hh: generic superscript "h" (paper main text)
  # Two-hh:    superscripts "1" and "2" (appendix)
  hh_map <- list(
    list(sup       = if (single_hh) "h" else "1",
         col_line  = "blue!75!black",
         col_fill  = "blue!75!black!60"),
    list(sup       = if (single_hh) "h" else "2",
         col_line  = "red!80!black",
         col_fill  = "red!80!black!60")
  )

  # ===== Build TikZ =====

  tex <- character()
  a <- function(...) tex <<- c(tex, paste0(...))

  w  <- me$window
  sc <- me$scale

  # --- Preamble ---
  a("\\documentclass[tikz,10pt]{standalone}")
  a("\\usepackage{amsmath,amssymb}")
  a("\\usetikzlibrary{arrows.meta}")
  a("\\begin{document}")
  a("")

  # --- tikzpicture environment ---
  a("\\begin{tikzpicture}[")
  a("  >=Latex, line cap=round, line join=round,")
  a(sprintf("  x=%scm, y=%scm,", n6(sc$Unit), n6(sc$Unit)))
  a("  every node/.style={font=\\scriptsize}")
  a("]")
  a("")

  # ===== Axes =====
  a("  % === Axes ===")
  a(sprintf("  \\draw[<-] (%s,0) -- (0,0);", n6(w$Xmin)))
  a(sprintf("  \\draw[->] (0,0) -- (%s,0);", n6(w$Xmax)))
  a(sprintf("  \\draw[<-] (0,%s) -- (0,0);", n6(w$Ymin)))
  a(sprintf("  \\draw[->] (0,0) -- (0,%s);", n6(w$Ymax)))
  a("")
  a("  % Axis labels")
  a(sprintf("  \\node[below, font=\\scriptsize] at (%s,0) {$\\nu_0^-$};", n6(w$Xmin)))
  a(sprintf("  \\node[below, font=\\scriptsize] at (%s,0) {$\\nu_0^+$};", n6(w$Xmax)))
  a(sprintf("  \\node[right, font=\\scriptsize] at (0,%s) {$\\nu_1^+$};", n6(w$Ymax)))
  a(sprintf("  \\node[right, font=\\scriptsize] at (0,%s) {$\\nu_1^-$};", n6(w$Ymin)))
  a("")

  a(sprintf("  \\pgfmathsetmacro{\\q}{%s}", n6(q)))
  a("")

  # ===== Feasible trapezoid(s) =====
  for (h in households) {
    hm <- hh_map[[h]]
    tr <- me$trapezoids[[h]]
    a(sprintf("  %% === Feasibility trapezoid (h=%d) ===", h))
    a(sprintf("  \\path[fill=%s, fill opacity=0.15, draw=%s, line width=0.6pt]",
              hm$col_fill, hm$col_line))
    a(sprintf("    (%s,%s) --", n6(tr$top_left[1]),  n6(tr$top_left[2])))
    a(sprintf("    (%s,%s) --", n6(tr$top_right[1]), n6(tr$top_right[2])))
    a(sprintf("    (%s,%s) --", n6(tr$bot_right[1]), n6(tr$bot_right[2])))
    a(sprintf("    (%s,%s) -- cycle;", n6(tr$bot_left[1]), n6(tr$bot_left[2])))
    a("  ")
    a("  % Diagonal edge from origin to lower-right (banking constraint boundary)")
    a(sprintf("  \\draw[thin, %s] (0,0) -- (%s,%s);",
              hm$col_line, n6(tr$bot_right[1]), n6(tr$bot_right[2])))
  }
  a("")

  # ===== Banking technology ray (slope -1/q) =====
  cp <- me$cone_pos
  cn <- me$cone_neg
  a("  % === Banking technology ray (slope -1/q) ===")
  a(sprintf("  \\draw[very thick, gray!70] (0,0) -- (%s,%s);",
            n6(cp[1]), n6(cp[2])))
  a(sprintf("  \\draw[very thick, dashed, gray!70] (0,0) -- (%s,%s);",
            n6(cn[1]), n6(cn[2])))
  a("")

  # ===== Dual cone C(q)* (optional) =====
  if (show_dual_cone) {
    dcl <- me$dual_cone_len
    a("  % === Normal (dual) cone C(q)^* ===")
    a("  \\path[fill=orange!80, fill opacity=0.10, draw=none]")
    a(sprintf("    (0,0) -- (0,%s) -- (%s,%s) -- cycle;",
              n6(dcl), n6(dcl), n6(q * dcl)))
    a(sprintf("  \\draw[very thick, orange!85!black]"))
    a(sprintf("    (0,0) -- (%s,%s)", n6(dcl), n6(q * dcl)))
    a(sprintf("    node[pos=.95, right, font=\\scriptsize, yshift=4pt] {$\\pi_1=\\bar q$};"))
    a(sprintf("  \\draw[very thick, orange!60!black]"))
    a(sprintf("    (0,0) -- (0,%s);", n6(dcl)))
    a(sprintf("  \\node[orange!85!black, font=\\scriptsize, yshift=10pt, xshift=10pt]"))
    a(sprintf("    at (%s,%s)", n6(0.33 * dcl), n6(0.42 * dcl)))
    a("    {$\\mathcal{C}(\\bar q)^{*}$};")
    a("")
  }

  # ===== Bond vector(s) =====
  for (h in households) {
    hm <- hh_map[[h]]
    bp <- me$bond_pts[[h]]
    a(sprintf("  %% === Bond vector b^{%s} ===", hm$sup))
    a(sprintf("  \\draw[->, thick, %s] (0,0) -- (%s,%s);",
              hm$col_line, n6(bp[1]), n6(bp[2])))
    a(sprintf("  \\node[below left=1pt, text=%s, font=\\scriptsize] at (%s,%s) {$b^{%s}$};",
              hm$col_line, n6(bp[1]), n6(bp[2]), hm$sup))
    a("")

    # --- Projections from b^h to axes ---
    a(sprintf("  %% === Projections from b^{%s} to axes ===", hm$sup))
    a(sprintf("  \\draw[densely dashed, %s] (%s,%s) -- (%s,0);",
              hm$col_line, n6(bp[1]), n6(bp[2]), n6(bp[1])))
    a(sprintf("  \\fill[%s] (%s,0) circle (1.1pt);",
              hm$col_line, n6(bp[1])))
    a(sprintf("  \\node[above, text=%s, font=\\scriptsize] at (%s,0) {$\\bar{\\nu}_0^{%s}$};",
              hm$col_line, n6(bp[1]), hm$sup))
    a("")
    a(sprintf("  \\draw[densely dashed, %s] (%s,%s) -- (0,%s);",
              hm$col_line, n6(bp[1]), n6(bp[2]), n6(bp[2])))
    a(sprintf("  \\fill[%s] (0,%s) circle (1.1pt);",
              hm$col_line, n6(bp[2])))
    a(sprintf("  \\node[left, text=%s, font=\\scriptsize, xshift=3pt] at (0,%s) {$\\bar{\\nu}_1^{%s}$};",
              hm$col_line, n6(bp[2]), hm$sup))
    a("")
  }

  # ===== Reference markers: -m^h and -p·e^h =====
  for (h in households) {
    hm <- hh_map[[h]]
    tr <- me$trapezoids[[h]]
    a(sprintf("  %% === Reference markers (h=%d) ===", h))
    a(sprintf("  \\fill[%s] (%s,0) circle (1.2pt);", hm$col_line, n6(-m[h])))
    a(sprintf("  \\node[above right, text=%s, font=\\scriptsize, xshift=-9.5pt, yshift=-2pt] at (%s,0) {$-m^{%s}$};",
              hm$col_line, n6(-m[h]), hm$sup))
    a("")
    a(sprintf("  \\fill[%s] (0,%s) circle (1.2pt);", hm$col_line, n6(-tr$pe)))
    a(sprintf("  \\node[below left, text=%s, font=\\scriptsize] at (0,%s) {$-p \\cdot e^{%s}$};",
              hm$col_line, n6(-tr$pe), hm$sup))
    a("")
  }

  # ===== Bank counterposition D (appendix only) =====
  if (show_bank && !single_hh) {
    ba <- me$bond_agg
    bank_scale <- 0.6
    a("  % === Bank counterposition D ===")
    a(sprintf("  \\draw[->, very thick, black] (0,0) -- (%s,%s)",
              n6(-ba[1] * bank_scale), n6(-ba[2] * bank_scale)))
    a("    node[pos=.88, above, font=\\scriptsize] {$D$};")
    a("")
  }

  # ===== Slope calibration box =====
  a("  % === Slope calibration box ===")
  a("  \\draw[->, thick] (0,0) -- (-\\q, 1);")
  a("  \\draw[densely dashed] (-\\q,0) -- (-\\q,1) -- (0,1);")
  a("  \\fill (-\\q,0) circle (1.2pt) node[below, font=\\scriptsize, xshift=-4pt] {$-q$};")
  a("  \\fill (0,1) circle (1.2pt) node[above right, font=\\scriptsize, yshift=-2pt] {$1$};")
  a("")

  # ===== Optimal goods point (filled dot on top) =====
  a("  % === Optimal goods point ===")
  for (h in households) {
    hm <- hh_map[[h]]
    bp <- me$bond_pts[[h]]
    a(sprintf("  \\fill[draw=white, line width=0.4pt, fill=%s]",
              hm$col_line))
    a(sprintf("    (%s,%s) circle (1.9pt);", n6(bp[1]), n6(bp[2])))
  }
  a("")

  # ===== Feasibility region label =====
  for (h in households) {
    hm <- hh_map[[h]]
    lp <- me$label_pos[[h]]
    a("  % === Feasibility region label ===")
    a(sprintf("  \\node[font=\\scriptsize, text=%s] at (%s,%s) {$\\mathcal{F}^{%s}_p(m^{%s}, q)$};",
              hm$col_line, n6(lp[1]), n6(lp[2]), hm$sup, hm$sup))
  }
  a("")

  a("\\end{tikzpicture}")
  a("\\end{document}")
  a("")

  tex_str <- paste(tex, collapse = "\n")

  # --- Write ---
  if (write) {
    if (is.null(file_name)) {
      hh_str <- paste0("h", paste(households, collapse = ""))
      file_name <- sprintf("me_nominal_%s_%s", hh_str, par$label)
      file_name <- gsub("[^A-Za-z0-9_.-]", "_", file_name)
    }
    out_dir <- path.expand(out_dir)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(out_dir, paste0(file_name, ".tex"))
    writeLines(tex_str, out_path)
    message("TEX: ", normalizePath(out_path, winslash = "/", mustWork = FALSE))
  } else {
    out_path <- NULL
  }

  invisible(list(path = out_path, tex = tex_str))
}
