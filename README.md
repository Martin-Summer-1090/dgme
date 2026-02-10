# dgme

**Dubey–Geanakoplos Monetary Equilibria for Two-Household Economies**

Companion R package for *"Stablecoins and the Monetary Order: Inflationary Finance and Competing Settlement Assets"* (Summer, 2026).

## Installation

```r
# From local source (after cloning the repo):
install.packages("nleqslv")  # dependency
R CMD INSTALL dgme

# Or from within R:
install.packages("nleqslv")
install.packages("dgme", repos = NULL, type = "source")
```

## Quick start

```r
library(dgme)

# Load a preset parametrisation
par <- dgme_parametrize("paper_example_1")
print(par)

# Solve for monetary equilibrium
res <- dgme_solve(par, variants = "baseline")
print(res)

# Access the result
bl <- res$results$baseline
bl$p     # equilibrium prices
bl$x     # consumption allocations
bl$tau   # net trades
bl$D     # aggregate credit
```

## Package pipeline

```
dgme_parametrize()  →  dgme_solve()  →  dgme_plot() / dgme_table() / dgme_tikz()
```

Currently implemented (v0.1.0): `dgme_parametrize()` and `dgme_solve()` for baseline and substitution variants. Visualisation, TikZ output, and treasury/competing variants coming in v0.2.0.

## Verification

```r
source(system.file("verify.R", package = "dgme"))
```

## License

MIT
