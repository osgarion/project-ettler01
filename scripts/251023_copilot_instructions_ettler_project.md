# Copilot instructions: generate an R script for Ettler project analyses

> **Goal:** Create a single, self‑contained **R script** (English comments, function names, and outputs) that replicates the structure of `reports/ettler_project_03-c.Rmd` and performs a full EDA plus three families of statistical models, always including the covariate `stage_early`. The script should build two main result tables as specified below and save them to disk.

---

## 1) Files & project structure

### Required inputs to inspect
- `reports/ettler_project_03-c.Rmd`  
  *Use its structure and section order as a template (EDA, modeling, outputs).*
- `scripts/Script_project_ettler_03.R`  
  *Use it to source/confirm variable names, helper functions, and any custom factor codings.*

### Validate file existence (with typo tolerance)
- Before running, **check that both paths exist**. If not, attempt a **fuzzy match** for likely typos (e.g., `time_to_next_treatmen` vs `time_to_next_treatment`).
- If neither the exact nor a fuzzy match is found, **stop with a clear message** listing attempted paths and suggested closest matches in the repo.

### Suggested output locations
- Create (if missing) `outputs/` and `figures/` directories.
- Save final tables as:
  - `outputs/summary_main_models.csv`
  - `outputs/summary_adjusted_models.csv`
- Save EDA figures (PNG) into `figures/eda_*`.

---

## 2) Data expectations & core variables

Use `scripts/Script_project_ettler_03.R` to confirm canonical variable names and levels. At minimum, models use:

- **Binary outcome (logistic):** `response_achieved`
- **Survival outcomes (Cox PH):** `time_to_next_treatmen`, `treatment_duration`, `response_duration`
- **Continuous outcome (linear):** `response_time_to`
- **Mandatory covariate in all models:** `stage_early` (factor or binary; ensure correct coding)
- **Potential additional covariates for sequential adjustment:**
  - `age`, `sex`, `bmi`, `ps_ecog`, `first_syst_th`, `dyslipidemia_before`, `thyroid_disease_before`, `monotherapy`

**Assumptions & hygiene:**
- Ensure consistent factor levels, especially for `stage_early` (e.g., levels `0` and `1`, or `No`/`Yes`).
- For binary outcomes, ensure reference level is sensible and documented.
- For survival outcomes, verify event indicators are present (e.g., *_event variables). If missing, infer from script conventions or stop with a helpful error.
- Handle missingness via a documented approach (e.g., complete‑case for modeling, but report missingness in EDA). Do not silently drop rows without logging counts.

---

## 3) EDA block (replicate structure from the Rmd)

Implement an EDA section that **checks, visualizes, evaluates, and describes all variables**:
- Dataset snapshot (`str()`, `skimr::skim()`, counts, unique values).
- Missingness patterns (`naniar` or `visdat`).
- Distributions: hist/density for numeric; barplots for categorical.
- Bivariate quick looks:
  - Numeric vs numeric: scatter + correlation matrix.
  - Numeric vs binary: box/violin + statistic summary.
  - Categorical vs outcome(s): mosaic or grouped bar charts.
- Survival endpoints: quick Kaplan–Meier previews by `stage_early`.
- Save all plots to `figures/` and include short text summaries (printed to console/log).

**Tech details:** keep plots legible, add counts in facet titles, and save with DPI ≥ 150.

---

## 4) Modeling plan

### 4.1 Logistic regression (GLM with logit)
- Outcome: `response_achieved`.
- Base model formula: `response_achieved ~ stage_early`.
- Report: adjusted odds ratio (aOR), 95% CI, and p‑value for the effect of `stage_early` and for any other coefficient relevant to outputs (see Tables section).

### 4.2 Cox proportional hazards models
- Outcomes: `time_to_next_treatmen`, `treatment_duration`, `response_duration`.
- For each, fit: `Surv(time, event) ~ stage_early` using appropriate time/event variables.
- Report: hazard ratio (HR), 95% CI, p‑value for `stage_early` and the focal relationship specified in Tables section.
- **Check PH assumption** with `cox.zph()`. If violated, log a warning and note it in the output table (add a `ph_ok` boolean column in intermediate results; optional in final table).

### 4.3 Linear regression
- Outcome: `response_time_to`.
- Base model: `response_time_to ~ stage_early`.
- Report: coefficient for `stage_early` (interpret alongside units), 95% CI, p‑value.

### 4.4 Stage‑specific estimates (crucial)
- **Verify** whether stage‑specific estimates can be obtained via **post‑hoc** from a model with `stage_early` and optionally its interaction with the predictor of interest. For **logistic and linear** models, post‑hoc (e.g., `emmeans` or contrasts) is acceptable.
- For **survival models**, prefer **stratified analyses**: fit separate Cox models for `stage_early = 0` and `stage_early = 1` (unless a well‑specified interaction model with post‑hoc is demonstrably valid and simpler). **Implement both checks** and default to separate models for survival endpoints.
- Always document in the code which approach was used and why.

---

## 5) Adjustment strategy (second table)

For each endpoint, build **sequentially adjusted** models by **adding one covariate at a time** to the base model that already contains `stage_early`.

- Covariate order (exactly this sequence):
  1. `age`
  2. `sex`
  3. `bmi`
  4. `ps_ecog`
  5. `first_syst_th`
  6. `dyslipidemia_before`
  7. `thyroid_disease_before`
  8. `monotherapy`

For each added covariate **C**:
- Fit an **all‑patients** model including `stage_early + C` (+ outcome‑specific structures).
- Obtain stage‑specific estimates **for the relationship between C and the outcome** (post‑hoc for logistic/linear; separate Cox by stage for survival), as required in the output schema below.
- Extract p‑values and aOR/HR (or coefficient for linear), with 95% CIs (CIs may be optional in final table if schema does not include columns; however compute and keep internally for QA).

---

## 6) Output tables (required schema)

### 6.1 Main models table (first primary output)
**File:** `outputs/summary_main_models.csv`

Columns (in this exact order):
1. `outcome` — name of dependent variable (e.g., `response_achieved`, `time_to_next_treatmen`, `treatment_duration`, `response_duration`, `response_time_to`).
2. `p_all` — p‑value for the `all patients` model for the relationship of interest (base model with `stage_early`).
3. `effect_all` — **aOR** for logistic, **HR** for Cox, **beta** for linear (include sign).
4. `p_stage0` — p‑value for stage‑specific effect (stage_early = 0).
5. `effect_stage0` — effect size in stage 0 (aOR / HR / beta, as above).
6. `p_stage1` — p‑value for stage‑specific effect (stage_early = 1).
7. `effect_stage1` — effect size in stage 1 (aOR / HR / beta, as above).

Notes:
- The **relationship of interest** in the main table is the effect of `stage_early` on each outcome.
- Ensure consistent rounding (e.g., 2–3 decimals for effects; p‑values with `format.pval` and `<0.001` convention).

### 6.2 Sequentially adjusted table (second primary output)
**File:** `outputs/summary_adjusted_models.csv`

Columns (in this exact order):
1. `outcome` — dependent variable name.
2. `covariate` — the **added** covariate (one of the eight listed above).
3. `p_all` — p‑value for the relationship between the **added covariate** and the outcome in all patients (model includes `stage_early + covariate`).
4. `effect_all` — aOR / HR / beta for the covariate in all patients.
5. `p_stage0` — p‑value for the covariate–outcome relationship in stage_early = 0.
6. `effect_stage0` — stage‑specific effect for stage_early = 0.
7. `p_stage1` — p‑value for the covariate–outcome relationship in stage_early = 1.
8. `effect_stage1` — stage‑specific effect for stage_early = 1.

Notes:
- Keep the base presence of `stage_early` in the model; the table **reports the incremental covariate’s effect**.
- For survival endpoints, produce stage‑specific results via separate Cox models.

---

## 7) Implementation details & robustness

- **Language:** All code, comments, and printed outputs in **English**.
- **Packages:** Prefer `tidyverse`, `broom`, `survival`, `survminer`, `emmeans`, `broom.helpers`, `dplyr`, `readr`, `stringr`, `purrr`, `janitor`, `skimr`, `naniar`, `ggplot2`.
- **Helpers:** Create small helper functions:
  - `check_and_fuzzy_path(path)`: existence + closest match lookup.
  - `coerce_binary_factor(x, ref)`: sets reference level.
  - `fmt_effect(effect, ci_low, ci_high, model_type)`: returns formatted aOR/HR/beta.
  - `extract_logistic_effects(model, term)`, `extract_cox_effects(model, term)`, `extract_linear_effects(model, term)` to return tidy rows with `estimate`, `conf.low`, `conf.high`, `p.value`.
  - `fit_stage_specific_*()` wrappers to produce stage‑0 and stage‑1 results per model type.
- **PH checks:** Add `ph_ok` to intermediate summaries for Cox (`TRUE/FALSE` from `cox.zph`).
- **Missing data:** Log the number of rows dropped per model. Optionally consider simple imputation only if it mirrors conventions in `Script_project_ettler_03.R`.
- **Reproducibility:** Set a seed at top (`set.seed(123)`), write session info at the end.
- **File outputs:** Use `readr::write_csv()` for tables; ensure UTF‑8.

---

## 8) Script skeleton (what you should generate)

```r
# 00) Setup ---------------------------------------------------------------
library(tidyverse)
library(skimr)
library(janitor)
library(naniar)
library(broom)
library(broom.helpers)
library(survival)
library(survminer)
library(emmeans)
set.seed(123)

# paths
template_rmd <- "reports/ettler_project_03-c.Rmd"
helper_script <- "scripts/Script_project_ettler_03.R"

# helper: fuzzy path check (stringdist recommended)
# helper: coerce factors, effect extractors, stage-specific wrappers

# 01) Validate inputs & source helpers -----------------------------------
# check_and_fuzzy_path(template_rmd)
# check_and_fuzzy_path(helper_script)
source(helper_script)

# 02) Load data -----------------------------------------------------------
# (Follow conventions from Script_project_ettler_03.R to load/prepare data)
# df <- load_ettler_data()  # or similar helper if defined

# 03) EDA -----------------------------------------------------------------
# summaries, missingness, distributions, bivariates, KM previews
# ggsave("figures/eda_*.png", ...)

# 04) Models: main effects -----------------------------------------------
# Logistic: response_achieved ~ stage_early
# Cox: Surv(time, event) ~ stage_early for three endpoints
# Linear: response_time_to ~ stage_early
# Extract all-patient + stage-specific effects (post-hoc for logistic/linear; split for Cox)
# Bind into summary_main_models and write_csv

# 05) Models: sequential adjustments -------------------------------------
# For each covariate in list, fit models with stage_early + covariate
# Extract all-patient + stage-specific effects for the covariate
# Bind into summary_adjusted_models and write_csv

# 06) Session info & done -------------------------------------------------
# sessionInfo()
```

---

## 9) QA checklist (must pass)

- [ ] Both input files found (or closest matches reported clearly).
- [ ] `stage_early` present, correctly coded; reference level documented.
- [ ] EDA figures saved and key summaries printed.
- [ ] **Two CSV tables** saved with exact schemas defined above.
- [ ] Survival models include **PH assumption checks** and notes.
- [ ] Stage‑specific results obtained correctly (post‑hoc vs split Cox) and documented.
- [ ] Clear console messages guiding the user through each step.

---

## 10) Deliverable

Generate a single R script file (English) named, for example:
- `scripts/ettler_analysis_autogen.R`

When executed, it should:
1) Verify inputs; 2) run EDA; 3) fit models; 4) produce the two summary tables; 5) save figures and CSVs; 6) print a concise run summary.

