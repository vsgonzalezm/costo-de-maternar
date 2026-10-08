# Spec: `05b_marginal_means.R`

Instructions for writing a script that produces the predicted marginal means and
contrasts committed to in AsPredicted #297542, item 5. Hand this whole file to the
assistant along with `04_models.R`.

---

## 1. Context it needs to know

- Models were fit with `estimatr::lm_robust(..., clusters = numericalId, se_type = "CR2")`
  and saved as **named lists of model objects** (one element per outcome) in
  `Outputs/models/`: `set1_models_prof.rds`, `set1_models_tec.rds`, `set2a_models_prof.rds`,
  … and for pooled sets `set5_models.rds`, `set5a_models.rds`, … Index-based models
  (`*_idx_*`) follow the same pattern — confirm the exact filenames with `list.files()`
  before hard-coding anything.
- Some list elements are `NULL` (the `tryCatch` in `04_models.R` returns `NULL` on failure).
  Skip these with `purrr::compact()` rather than letting the script die.
- **All factors use sum contrasts.** This is the reason the script exists: coefficients are
  deviations from the design mean, so a two-level coefficient is *half* the between-level
  gap. Marginal means remove that ambiguity.

## 2. Critical setup requirement

`lm_robust` objects do **not** retain the model frame. Marginal means therefore require
the original data, factored **identically** to `04_models.R`.

Do not re-code the factors by hand. Source or copy `apply_sum_contrasts()` and the entire
`df <- base_long_final %>% mutate(...)` block verbatim from `04_models.R`, then build
`df_prof` and `df_tec` the same way. Any difference in level order silently changes the
contrast matrix and produces wrong numbers with no error.

Add an assertion after building `df`:

```r
stopifnot(identical(levels(df$perfil_plan_futuro), c("en 2 anos", "en 5 anos", "no quiere")))
```

(and equivalents for the other manipulated factors — this catches the `años`/`anos`
normalisation bug that already bit this project once).

## 3. Package choice

Use **`marginaleffects`**, not `emmeans`.

`marginaleffects` reads the CR2 variance-covariance matrix off the `lm_robust` object
automatically, so cluster-robust inference propagates into every contrast. Verify this once
rather than assuming it:

```r
m <- readRDS(...)[["recommendation_probability"]]
all.equal(vcov(m), marginaleffects::get_vcov(m))   # must be TRUE
```

If it is not TRUE, pass `vcov = vcov(m)` explicitly to every call.

## 4. Two different estimands — do not mix them up

**(a) Averaged marginal means → use for all hypothesis tests (H1–H16).**

Use `avg_predictions()` / `avg_comparisons()` with `newdata = df_prof` (or `df_tec`, or `df`
for pooled sets). This is G-computation: it averages predictions over the *empirical*
distribution of the remaining covariates.

This matters because the design is D-efficient, not fully crossed — only 200 of the possible
attribute combinations exist. Building a balanced reference grid with `datagrid()` would
average over cells that were never shown to anyone. Averaging over observed rows keeps every
estimate inside the design's support.

**(b) One fixed-profile contrast → the prereg anchor only.**

Item 5 commits to a contrast between two *specific* profiles from SET 1:
male vs. female, both single, no children, no family plans. This one is a conditional
prediction, so it does use `datagrid()`:

```r
predictions(
  m,
  newdata = datagrid(
    perfil_genero      = c("Hombre", "Mujer"),
    perfil_estado_civil = "soltero/a",
    perfil_parental     = "actualmente no tiene hijos",
    perfil_plan_futuro  = "no quiere",
    newdata = df_prof            # remaining covariates held at their sample means/modes
  ),
  hypothesis = "revpairwise"
)
```

Label these two estimands distinctly in the output. They answer different questions and a
discussant will ask which one you are quoting.

## 5. Contrasts to produce

Run each over all 16 outcomes plus the three indices (`Perceived risk`, `Growth potential`,
`Competence`). Note that `costo` was **not** aggregated (alpha = .631, below the pre-specified
.70 threshold), so its four items stay individual.

| ID | Model set | Contrast | Notes |
|---|---|---|---|
| Anchor | SET1 (prof, tec) | Male vs. female, single, childless, no plans | Fixed-profile, per §4b |
| H1 | SET2B | Within **childless women**: 2 years vs. no plans; 5 years vs. no plans | `by = perfil_genero`, subset to childless |
| H2 | SET4B | Within **mothers**: same two contrasts | Cells are thin — see §6 |
| H3 | SET2A | Mothers (1 child; 2 children) vs. childless women | Report both levels separately; the one-child and two-child effects diverge |
| H4–H9 | SET2A, SET2B | Difference-in-differences: the H1/H3 contrast for women **minus** the same contrast for men | Use `hypothesis = "revpairwise"` on the `by = perfil_genero` output |
| H16 | SET4A | Single vs. married mothers, estimated separately by gender, then differenced | |
| H10–H15 | SET5, SET5a–5e | Each of the above, contrasted across `condicion_ocupacional` | Pooled sample |

For H4–H9 and H10–H15 the quantity of interest is the **difference between differences**, not
two separately-reported contrasts. Compute it as a single estimate with its own standard
error — "significant for women, not for men" is not a test of moderation and reviewers in this
literature reliably call it out.

## 6. Guardrails to build in

- **Report cell counts.** For every contrast, emit the number of profile observations and the
  number of distinct respondents in each cell. SET4A and SET4B are three-way interactions with
  only 2 profiles per respondent; some cells will be small enough that the CR2 correction is
  doing heavy lifting. The `nd = 2` design makes this a real risk, not a formality.
- **Attach BH-adjusted p-values** within each outcome family (cost / risk / growth / competence
  / hiring), alongside raw p. Keep both columns.
- **Flag any contrast where CR2 degrees of freedom fall below ~30** — Satterthwaite df get
  small in thin cells and the normal approximation stops being safe there.
- Return everything as one tidy data frame with columns:
  `set_id, condition, outcome, estimand, contrast, estimate, std.error, conf.low, conf.high, df, p.value, p.adj, n_obs, n_clusters`.

## 7. Outputs

1. `Outputs/tables/marginal_means_all.csv` — the full tidy frame above.
2. Per-hypothesis `.docx` tables in `Outputs/tables/marginal_means/`, matching the existing
   `modelsummary` styling used in `05_Tables.R` so the two sets of tables can sit side by side.
3. A console summary listing, for each of H1–H16, the single contrast that most directly tests
   it, with estimate, CI, and adjusted p.

## 8. Sanity checks before trusting the output

Include these as assertions or printed checks:

- For a two-level factor, `avg_comparisons()` on gender must equal **2 ×** the corresponding
  sum-contrast coefficient from the fitted model. If it equals 1×, the contrast coding was lost.
- Predicted marginal means for the hiring outcome must fall inside the observed 1–7 range and
  sit near the SET1 intercept (≈ 5.78 in the profesional arm).
- Re-fitting one model with treatment contrasts must leave the marginal means unchanged. Marginal
  means are invariant to contrast coding; if they move, something upstream is wrong.
