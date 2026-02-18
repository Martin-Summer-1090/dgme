## generate_paper_tables.R
## Generates all numerical illustration tables for the stablecoins paper.
## Run from the dgme package root or after library(dgme).
##
## Usage:
##   library(dgme)
##   source("inst/paper_scripts/generate_paper_tables.R")

library(dgme)

table_dir <- getOption("dgme.tab_dir",
  "/home/martinsummer/Projects/Research_Projects/Money_and_Inflation_Theory/shared/tables")
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

# ── Canonical parameterisation ────────────────────────────────
par <- dgme_parametrize("paper_example_1")

# ══════════════════════════════════════════════════════════════
# Table 2: Effect of reserve composition (rho)
# ══════════════════════════════════════════════════════════════
res <- dgme_solve(par, variants = c("baseline", "treasury"))

dgme_table(res, "rho_sweep",
           format = "latex",
           save = TRUE,
           path = table_dir,
           filename = "rho_sweep",
           rho_grid = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0))

# ══════════════════════════════════════════════════════════════
# Table 3: Effect of aggregate stablecoin issuance (S)
# ══════════════════════════════════════════════════════════════
dgme_table(res, "S_sweep",
           format = "latex",
           save = TRUE,
           path = table_dir,
           filename = "S_sweep",
           S_grid = c(0, 0.5, 1.0, 2.0, 3.0, 5.0))

# ══════════════════════════════════════════════════════════════
# Table 4: Calibrated stablecoin scenarios
# ══════════════════════════════════════════════════════════════
dgme_table(res, "scenarios",
           format = "latex",
           save = TRUE,
           path = table_dir,
           filename = "scenarios")

# ══════════════════════════════════════════════════════════════
# Table 5: Exchange-rate indeterminacy
# ══════════════════════════════════════════════════════════════
res_comp <- dgme_solve(par, variants = "competing")

dgme_table(res_comp, "indeterminacy",
           format = "latex",
           save = TRUE,
           path = table_dir,
           filename = "indeterminacy")

cat("\nAll paper tables written to:", table_dir, "\n")
