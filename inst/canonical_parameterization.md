# Canonical Parameterization for the Stablecoins Paper

## Recommendation

**Keep `paper_example_1` unchanged** as the baseline and extend it with default
Treasury-channel and competing-money parameters. The existing values produce
equilibria with exactly the right properties for every section of the paper.

## The Baseline (Section 2)

```r
e     <- matrix(c(2.8, 1.7,
                  0.3, 2.1), nrow = 2, byrow = TRUE)
alpha <- c(0.22, 0.84)
m     <- c(0.4, 1.8)
q     <- 0.6
```

**Why it works:**

| Property | Value | Why it's good |
|---|---|---|
| M = 2.2 | Moderate | Prices ~2, fits A4/TikZ figures |
| r = 2/3 | Substantial | Monetary friction clearly visible in kink geometry |
| Trading pattern | h1 sells g1/buys g2, h2 opposite | Clean mirror structure |
| Both (T) constraints | Bind exactly | Illustrates the mechanism cleanly |
| M = (1−q)D | 2.2 = 0.4 × 5.5 ✓ | Flow-of-funds identity exact |
| Gains from trade | h1: +32%, h2: +295% | Large enough to show benefits |
| Prices | p = (2.00, 1.67) | Moderate, legible |
| Relative price | p₁/p₂ = 1.20 | Not too far from 1, not degenerate |

## Treasury Channel Defaults (Section 3)

```r
rho      <- 0.10        # 10% in settlement media
eta      <- 0.80        # 80% pass-through
S        <- 1.0         # moderate issuance
g_shares <- c(0.5, 0.5) # equal distribution
```

**Why these values:**

- **ρ = 0.10**: The paper argues empirical ρ ≈ 0.05–0.15. Setting ρ = 0.10
  is the midpoint and a round number.
- **η = 0.80**: Non-trivial departure from Ricardian benchmark (η = 0), but
  not full pass-through (η = 1). Makes the point that even partial
  pass-through generates inflation.
- **S = 1.0**: At this level, the Treasury injection G = η(1−ρ)S = 0.72,
  which is 13.1% of baseline payment capacity (5.50). This produces a ~33%
  price increase — large enough to be visible in figures, small enough to keep
  the economy in the same trading regime (no regime switches that would
  complicate the story).
- **g_shares = (0.5, 0.5)**: Equal distribution is the simplest case and
  shows the pure aggregate effect. Varying g_shares is a natural
  comparative-static exercise.

**Key results at default parameters:**

| | Baseline | Treasury (S=1, ρ=0.1, η=0.8) |
|---|---|---|
| p₁ | 2.004 | 2.646 (+32.0%) |
| p₂ | 1.674 | 2.268 (+35.5%) |
| p₁/p₂ | 1.197 | 1.167 |
| x₁ (h1) | 1.210 | 1.255 |
| x₂ (h1) | 3.081 | 3.117 |
| x₁ (h2) | 1.890 | 1.845 |
| x₂ (h2) | 0.719 | 0.683 |

The relative price changes slightly (from 1.197 to 1.167), confirming the
paper's observation that the injection "involves both a higher price level
and changed relative prices and real allocations."

## Competing Money Defaults (Section 4)

```r
m_S    <- c(0.2, 0.9)              # proportional to m
e_grid <- seq(0.5, 3.0, by = 0.5)  # exchange rate grid
```

**Critical design choice: m_S proportional to m.**

The proof of Proposition 4(iii) requires that the liquidity distribution
does not change with e. Formally: m̃ʰ/M̃ = (mʰ + mʰ_S/e)/(M + M_S/e)
must be independent of e. This holds iff mʰ_S/mʰ = M_S/M for all h —
i.e., m_S is proportional to m.

Setting m_S = 0.5 × m = (0.2, 0.9) satisfies this exactly. The numerical
results confirm: relative prices and real allocations are *identical* across
all exchange rates (p₁/p₂ = 1.1974, x = constant to machine precision),
while nominal prices scale with M̃ = M + M_S/e.

**Results:**

| e | p₁ | p₂ | p₁/p₂ | x₁(h1) | x₂(h1) | x₁(h2) | x₂(h2) |
|---|---|---|---|---|---|---|---|
| 0.5 | 4.009 | 3.348 | 1.197 | 1.210 | 3.081 | 1.890 | 0.719 |
| 1.0 | 3.007 | 2.511 | 1.197 | 1.210 | 3.081 | 1.890 | 0.719 |
| 1.5 | 2.673 | 2.232 | 1.197 | 1.210 | 3.081 | 1.890 | 0.719 |
| 2.0 | 2.506 | 2.092 | 1.197 | 1.210 | 3.081 | 1.890 | 0.719 |
| 3.0 | 2.339 | 1.953 | 1.197 | 1.210 | 3.081 | 1.890 | 0.719 |

This is exactly the table the paper's TODO placeholder asks for.

## Comparative-Static Exercises (for vignettes/figures)

The natural sweeps using `dgme_vary()` are:

1. **q-sweep** (baseline): `q = seq(0.3, 0.95, by = 0.05)` — shows how
   monetary friction affects allocations and prices
2. **ρ-sweep** (Treasury): `rho = seq(0, 1, by = 0.1)` at fixed S, η —
   shows the paper's key point that reserve *composition* matters
3. **S-sweep** (Treasury): `S = seq(0, 5, by = 0.5)` at fixed ρ, η —
   shows scaling of the inflation effect
4. **η-sweep** (Treasury): `eta = seq(0, 1, by = 0.1)` — isolates the
   Ricardian question
5. **e-sweep** (competing): `e_grid = seq(0.5, 3, by = 0.5)` — demonstrates
   indeterminacy

## Changes Needed in the Package

The current `paper_example_1` preset sets only baseline parameters.
It should be extended to also set the Treasury and competing-money defaults,
so that `dgme_parametrize("paper_example_1")` returns a `dgme_params` object
that is ready for *all* variants without any additional user input.

Specifically, in `dgme_parametrize.R`, the `paper_example_1` block should add:

```r
rho        <- 0.10
eta        <- 0.80
S          <- 1.0
g_shares   <- c(0.5, 0.5)
categories <- list(good1 = "B", good2 = "B")
m_S        <- c(0.2, 0.9)
e_grid     <- seq(0.5, 3.0, by = 0.5)
```

This means `dgme_solve(par, variants = "all")` will produce baseline,
substitution, treasury, and competing results in one call, all from the
same coherent parameterization.
