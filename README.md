# dgme

Dubey–Geanakoplos Monetary Equilibria for Two-Household Economies

## Overview

`dgme` computes and visualises monetary equilibria for 2-household, 2-good
economies with Cobb–Douglas preferences, based on the Dubey–Geanakoplos
(1992, 2003) model.  It accompanies the paper:

> **Stablecoins and the Monetary Order: Inflationary Finance and Competing
> Settlement Assets**
> Martin Summer, Oesterreichische Nationalbank (2026)

The package implements four model variants:

| Variant | Description | Key result |
|---------|-------------|------------|
| **Baseline** | Outside + inside money | Flow of funds: M = (1−q)D |
| **Substitution** | Second credit source at same price | Neutral (Proposition 1) |
| **Treasury channel** | Issuer deploys reserves into T-bills | Inflationary (Proposition 3) |
| **Competing money** | Parallel outside money | Exchange-rate indeterminacy (Proposition 4) |

## Installation

The package is not on CRAN.  Install directly from GitHub:

```r
# Install remotes if needed
if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes")

# Install dgme
remotes::install_github("Martin-Summer-1090/dgme")
```

The only non-base dependency is
[nleqslv](https://CRAN.R-project.org/package=nleqslv) (nonlinear
equation solver), which is installed automatically.

To build the vignette locally you also need
[Quarto](https://quarto.org/docs/get-started/) installed on your system.

## Quick start

```r
library(dgme)

# Load canonical parameterisation
par <- dgme_parametrize("paper_example_1")

# Solve all four variants
res <- dgme_solve(par, variants = "all")

# Consistency checks
dgme_check(res$results$baseline)

# Publication tables
dgme_table(res, "comparison",
           variants = c("baseline", "treasury"),
           format = "latex")

# Indeterminacy table (Section 4)
dgme_table(res, "indeterminacy", format = "markdown")
```

## Package pipeline

```
dgme_parametrize()
       │
       ├──► dgme_solve()  ──► dgme_check()
       │         │
       │         ├──► dgme_table()        (LaTeX / markdown / data.frame)
       │         ├──► dgme_tikz_na_real() (TikZ: budget sets)
       │         └──► dgme_tikz_me_nominal() (TikZ: nominal transfers)
       │
       └──► dgme_vary()   ──► comparative statics over parameter grids
                │
                └──► dgme_solve() on each economy
```

## Canonical parameterisation

The preset `"paper_example_1"` includes defaults for all variants:

| Parameter | Value | Rationale |
|-----------|-------|-----------|
| e (endowments) | ((2.8, 1.7), (0.3, 2.1)) | Asymmetric, strong gains from trade |
| α (preferences) | (0.22, 0.84) | Each household wants the other's good |
| m (outside money) | (0.4, 1.8), M = 2.2 | Moderate money supply |
| q (bond price) | 0.6, r = 0.667 | Substantial monetary friction |
| ρ (reserve composition) | 0.10 | Midpoint of empirical range |
| η (fiscal pass-through) | 0.80 | Non-Ricardian benchmark |
| S (issuance) | 1.0 | ~13% payment capacity expansion |
| m_S (stablecoin money) | (0.2, 0.9) | Proportional to m (ensures Prop. 4(iii)) |

See `inst/canonical_parameterization.md` for the full derivation.

## Comparative statics

```r
# Sweep over reserve composition
family <- dgme_vary(par, rho = seq(0, 1, by = 0.1))

# Solve each and compare
for (i in seq_len(family$n)) {
  res_i <- dgme_solve(family[[i]], variants = c("baseline", "treasury"))
  # ... extract prices, payment capacity, etc.
}
```

## Vignette

The vignette reproduces all numerical examples from the paper:

```r
vignette("stablecoins-numerical-examples", package = "dgme")
```

Or render from source:

```bash
quarto render vignettes/stablecoins-numerical-examples.qmd
```

## References

- Dubey, P. and Geanakoplos, J. (1992). "The Value of Money in a
  Finite-Horizon Economy: A Role for Banks." In: *Economic Analysis
  of Markets and Games*, ed. by P. Dasgupta et al., MIT Press, 407–444.

- Dubey, P. and Geanakoplos, J. (2003). "Inside and Outside Fiat Money,
  Gains to Trade, and IS-LM." *Economic Theory* 21(2–3), 347–397.

## License

MIT
