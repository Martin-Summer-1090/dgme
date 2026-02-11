# dgme 0.2.0

## New features

* **Treasury channel solver** (Variant 2): `dgme_solve()` now computes
  equilibria with stablecoin reserve deployment via the Treasury channel.
  Supports both the simplified case (all goods in category B) and
  segmented product categories (O/T/B split).

* **Competing outside money solver** (Variant 4): `dgme_solve()` computes
  equilibria with two parallel settlement assets across an exchange-rate
  grid, demonstrating the indeterminacy result of Proposition 4.

* **Canonical parameterisation**: `dgme_parametrize("paper_example_1")` now
  includes default parameters for all four variants (ρ = 0.10, η = 0.80,
  S = 1.0, m_S proportional to m), so `dgme_solve(par, variants = "all")`
  works out of the box.

* **`dgme_table()`**: Publication-ready LaTeX, markdown, and data.frame
  tables for parameters, equilibrium summaries, cross-variant comparisons,
  and exchange-rate indeterminacy.

* **`dgme_tikz_na_real()` and `dgme_tikz_me_nominal()`**: TikZ code
  generation for budget-set diagrams in net-trade space and feasibility-set
  diagrams in nominal-transfer space.

* **`dgme_geometry()`**: Coordinate computation for all visualisation
  elements (kink points, indifference curves, feasibility trapezoids).

* **`dgme_vary()`**: Comparative-statics tool for sweeping over parameter
  grids with cross-product and parallel modes.

* **`dgme_check()`**: Post-solution consistency checks (market clearing,
  flow of funds, budget constraints, optimality, gains from trade).

* **Quarto vignette** reproducing all numerical examples from the paper.

## Bug fixes

* Fixed argument matching conflict in `dgme_parametrize()` where
  positional arguments clashed with named parameters.


# dgme 0.1.0

* Initial release with baseline and substitution solvers.
* `dgme_parametrize()` with presets, random generation, and manual
  specification.
* `dgme_solve()` for baseline (Newton/Broyden with grid fallback) and
  substitution (reuses baseline per Proposition 1).
* testthat test suite.
