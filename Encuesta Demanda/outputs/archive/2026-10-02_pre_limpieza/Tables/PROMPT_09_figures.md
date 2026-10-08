# Prompt for Positron — `09_conference_figures.R`

Paste everything below into Positron, together with `04_models.R` (for the factor
coding) and `08_conference_models_and_figures.R` (for the palette and theme).

---

Write an R script called `09_conference_figures.R` that produces exploratory
versions of every figure I might use in a 15-minute conference talk. I will look
at all of them and then choose. Do not pre-select for me — produce the full set.

## Context

Factorial survey experiment on hiring evaluations. Respondents (supervisors in
Chile) each evaluated two applicant profiles. Six attributes were manipulated at
the profile level using a D-efficient design. Models are OLS with cluster-robust
standard errors (CR2) clustered by respondent, fitted with
`estimatr::lm_robust`, on the pooled sample with occupational level as a
control.

Read `04_models.R` and copy the `apply_sum_contrasts()` function and the entire
`df <- base_long_final %>% mutate(...)` block **verbatim**. The factors use sum
contrasts; any change to level order silently changes the contrast matrix and
produces wrong numbers with no error. After building `df`, assert:

```r
stopifnot(identical(levels(df$perfil_plan_futuro), c("en 2 anos", "en 5 anos", "no quiere")))
stopifnot(identical(levels(df$perfil_genero), c("Mujer", "Hombre")))
```

## Outcomes — nine, built before modelling

| Object | Label | Composition |
|---|---|---|
| `recommendation_probability` | Hiring recommendation | single item |
| `good_investment_score` | Good investment | single item |
| `operational_costly_score` | Operationally costly | single item |
| `salary_costly_score` | Costly (salary) | single item |
| `expected_accommodations` | Requests accommodations | single item |
| `expected_benefits_use` | Uses company benefits | single item |
| `idx_competencia` | Competence index | `competent_score`, `trustworthy_score` |
| `idx_crecimiento` | Growth index | `ambitious_score`, `promising_score`, `expected_new_challenges`, `expected_promotion` |
| `idx_riesgo` | Risk index | `long_term_commitment_score` **(reverse-coded)**, `expected_late_absence`, `expected_quit_work`, `risky_continuity_score` |

**Reverse-coding is mandatory for the risk index.** `long_term_commitment_score`
is favourable while the other three items are unfavourable. Reverse it as
`8 - long_term_commitment_score` before averaging, so that higher = riskier
throughout. Print `psych::alpha()` for all three indices and stop with an
informative message if any falls below .70 — the index compositions were revised
and the reliabilities need re-verifying.

Indices are unweighted means, computed with `na.rm = TRUE`.

## Models

Fit two preregistered sets on the pooled data, all nine outcomes:

```
controls <- "pantalla_orden + template_contexto"
covars   <- "perfil_exp + perfil_desempeno + condicion_ocupacional"

set2a: y ~ perfil_genero * perfil_parental + perfil_estado_civil +
           perfil_plan_futuro + covars + controls

set2b: y ~ perfil_genero * perfil_plan_futuro + perfil_estado_civil +
           perfil_parental + covars + controls
```

Additionally fit a **presentation model** that carries both two-way
interactions, because contrast A below conditions on parental status *and*
family plans simultaneously:

```
setP:  y ~ perfil_genero * perfil_parental + perfil_genero * perfil_plan_futuro +
           perfil_estado_civil + covars + controls
```

`setP` combines two preregistered sets and is not itself preregistered. Label
every estimate that comes from it as such in the output CSV, in a column called
`model_source`.

**Validation.** After fitting, verify that a two-level marginal contrast equals
twice its sum-contrast coefficient:

```r
m <- setP$recommendation_probability
b <- coef(m)[["perfil_generoMujer"]]
cc <- avg_comparisons(m, variables = "perfil_genero", newdata = df)$estimate[1]
stopifnot(abs(abs(cc) - abs(2 * b)) < 1e-6)
```

If this fails, the contrast coding was lost and everything downstream is halved.

## Estimation approach

Use `marginaleffects`, not `emmeans` — it reads the CR2 variance-covariance
matrix off the `lm_robust` object automatically. Verify once with
`all.equal(vcov(m), marginaleffects::get_vcov(m))`.

Use **`avg_predictions()` / `avg_comparisons()` with `newdata` set to the
relevant subset of the observed data** — never `datagrid()` with a balanced
grid. The design is D-efficient, so only 200 of the possible attribute
combinations exist; a balanced grid would average over profiles nobody ever saw.
Averaging over observed rows keeps every estimate inside the design's support.

For every contrast, output: estimate, std.error, conf.low, conf.high, df,
p.value, n_obs, n_clusters.

Apply Benjamini-Hochberg adjustment **within outcome family**, where the
families are: hiring (`recommendation_probability`, `good_investment_score`),
cost (`operational_costly_score`, `salary_costly_score`,
`expected_accommodations`, `expected_benefits_use`), risk (`idx_riesgo`), growth
(`idx_crecimiento`), competence (`idx_competencia`). Keep both raw and adjusted
p in the output.

## The five contrast families

For each, estimate **marginal means for each group** (for plotting) and the
**contrast between groups** (for annotation and the CSV). Where a
difference-in-differences is specified, compute it as a single estimate with its
own standard error using `hypothesis = "revpairwise"` — never by comparing
significance across two separately-estimated contrasts.

**A. Pure gender penalty.** Model `setP`. Among applicants who are childless
*and* state no plans to have children: women vs. men. Also produce a
subset-based version (filter to childless & no plans, refit `setP` minus the
parental and plan terms, take the gender contrast) as a robustness column, since
the conditional prediction and the subset estimate can diverge.

**B. Future motherhood penalty.** Model `set2b`. Among **childless** applicants:
plans in 2 years vs. no plans, estimated separately for women and for men, plus
the women-minus-men difference-in-differences.

**C. Five-year horizon.** Model `set2b`. Same structure as B but plans in 5
years vs. no plans, for childless applicants and for parents separately. This
supports the claim that the five-year horizon carries no penalty except on
long-term commitment — so **also run this contrast on the raw
`long_term_commitment_score` item**, not only the risk index.

**D. Additional child penalty.** Model `set2b`. Among applicants who **already
have children**: plans in 2 years vs. no further plans, separately by gender,
plus the difference-in-differences.

**E. Pure motherhood penalty.** Model `set2a`. Among applicants stating **no
plans to have children**: one child vs. none, and two children vs. none,
separately by gender, plus the two difference-in-differences.

## Figures

Use `ggplot2` and `patchwork`. Match the palette and `theme_conf()` in
`08_conference_models_and_figures.R`:

```r
PAL <- list(teal = "#1f5f63", accent = "#b4552d", ink = "#1c2b33",
            soft = "#4a5c66", rule = "#d8dee1", band = "#e6efee")
```

Teal = men, accent = women. Do not introduce further colours.

### Small multiples — the primary form

For contrast families A–E, produce **small-multiple panels: one panel per
outcome, free y-scales, marginal means with 95% CI**. Do not put all nine
outcomes on a single shared axis — the outcome SDs range from roughly 0.95 to
1.78 and a common scale-point axis would misrepresent their relative magnitude.

Critical formatting rules:

1. **Identical panel set and identical panel order in every figure.** Fixed
   order: Hiring recommendation, Good investment, Competence index, Growth
   index, Risk index, Operationally costly, Costly (salary), Requests
   accommodations, Uses company benefits. Use a factor with fixed levels; never
   let `facet_wrap` reorder.
2. **Identical y-axis range per outcome across all figures.** Compute the range
   for each outcome once, across every group appearing in any figure, pad by
   10%, and apply it everywhere. The central visual argument is that the same
   panels move for childless women and for mothers, then go flat for men — that
   only reads if the frames are literally identical. Implement with a lookup
   table and `facetted_pos_scales` from `ggh4x`, or by building each panel
   separately and assembling with `patchwork`.
3. Points at size 2.8, error bars width 0.09 and linewidth 0.4, connecting lines
   at linewidth 0.7 where the x-axis is ordered (plan horizon, parental status).
4. Panel strip labels are the outcome labels above, sentence case, left-aligned.
5. Layout 3 × 3.
6. Mark contrasts surviving BH adjustment with a small filled point; use a
   hollow point otherwise. Add a one-line legend explaining this.

Produce, for each of A–E:

- a **women-only** version
- a **men-only** version, with the same panels and the same y-ranges
- a **combined** version with both genders as two series in each panel

That is fifteen figures. I want all of them so I can compare.

### Dissociation figure — the one place a shared axis is warranted

One additional figure, for the discussion section. Horizontal dot plot, all nine
outcomes on a **single shared axis**, showing the **change in the probability of
a top-two-box response** (P(rating ≥ 6)) for the pure gender contrast.

- Compute the top-two-box shift directly from the data, not from the models
- Confidence intervals from a respondent-clustered bootstrap, 1000 replicates,
  resampling `numericalId` with replacement
- **Orient every outcome so that rightward means worse for the applicant** —
  reverse the sign for hiring recommendation, good investment, competence index
  and growth index. Note the reversal in the caption.
- x-axis in percentage points
- Vertical reference line at zero
- Separate the outcomes into two visually distinct blocks — predicted conduct
  and evaluation — using a horizontal rule and whitespace, **not colour**. The
  point is to demonstrate a distinction, not to assert one through colour.

Do **not** produce a standardized (Cohen's d or Glass's delta) version. The
top-two-box metric is comparable across outcomes without a denominator choice,
needs no benchmark, and is invariant to any monotonic transformation of the
scale — which matters because several evaluative outcomes have 22–25% of
responses at the ceiling.

## Output

Write to `Outputs/Tables/conference_figures/`:

- All figures as PNG, 300 dpi, `bg = "white"`. Small multiples at 11 × 8.5
  inches; the dissociation figure at 8 × 6.
- Filenames: `figA_pure_gender_{women,men,combined}.png`,
  `figB_future_motherhood_{...}.png`, `figC_five_year_{...}.png`,
  `figD_additional_child_{...}.png`, `figE_pure_motherhood_{...}.png`,
  `figF_dissociation_topbox.png`
- One tidy CSV, `all_contrasts.csv`, with columns: `contrast_family`,
  `contrast_label`, `model_source`, `outcome`, `outcome_family`, `gender`,
  `estimate`, `std.error`, `conf.low`, `conf.high`, `df`, `p.value`, `p.adj`,
  `n_obs`, `n_clusters`
- A second CSV, `topbox_shifts.csv`, with the top-two-box and bottom-two-box
  shifts for every contrast in A–E, so I can quote them alongside the means

## Also print to console

- The three index alphas
- Cell counts (observations and distinct respondents) for every subgroup used,
  so I can see which contrasts are thin
- A flag for any contrast where the Satterthwaite df falls below 30

## Style

Use tidyverse. Write functions rather than repeating blocks for each outcome —
the five contrast families should go through one plotting function parameterised
by the data and the grouping variable. Comment the reverse-coding, the
`newdata` choice, and the fixed-y-range logic, since those are the three places
a future reader would otherwise get it wrong.
