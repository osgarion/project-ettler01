# Copilot task: Create an R Markdown report from `ettler_analysis_standalone.R`

> **Goal**: Generate a publication-ready **HTML report** by converting the existing script `ettler_analysis_standalone.R` into an R Markdown document (`reports/251106_ettler_survival_02.Rmd`). Use the YAML headers, formatting, abbreviations, and setup structure directly from `reports/ettler_project_03-c.Rmd`.

---

## Deliverables

1. `reports/251106_ettler_survival_02.Rmd` — the main R Markdown file
2. `reports/figs/` — directory for figures generated in the Rmd
3. HTML report (self-contained if possible)
4. Downloadable tables (Excel) and figures (TIFF, 150 dpi, LZW compression)

---

## Acceptance criteria (checklist)

- [ ] The Rmd **knits successfully** into a clean **HTML report**.
- [ ] The structure follows: **Basic Description**, **Abbreviations**, **Methods**, **EDA**, **Statistics** (→ **Main models** and **Adjusted models**, each with **Modeling**, **Tables**, and **Figures** subsections), **Conclusion**, **Session info**.
- [ ] **YAML header, formatting, and abbreviation list** are reused from `reports/ettler_project_03-c.Rmd`.
- [ ] **Helper functions** from `scripts/functions/FUN.R` are imported and used where possible.
- [ ] **All figures and tables** are downloadable via buttons — tables in **Excel (.xlsx)** and figures in **TIFF (150 dpi, LZW compression)**.
- [ ] Figures are created within the `.Rmd` file using the code from `ettler_analysis_standalone.R`.
- [ ] After each table, include a concise explanatory paragraph describing its interpretation.
- [ ] Each table or figure must have a **non-numbered legend** printed directly below the object.
- [ ] Each dependent variable is presented on a **separate section** under both **Main** and **Adjusted models**.
- [ ] In the **Adjusted model** subsections, each **figure section** should contain a **separate tab layout** where:
  - The **first row** displays results grouped by **dependent variable**, and
  - The **second row** displays results grouped by **covariate**.
  This tabular layout helps visualize relationships between covariates and dependent variables clearly.

---

## Document structure

### 1) Basic description of the project
Provide a **comprehensive and compact overview** of the study, including rationale, cohort, objectives, hypotheses, and analytical framework.

### 2) Abbreviations
Use the same abbreviations list and format as in `ettler_project_03-c.Rmd`.

### 3) Methods
Write in **scientific style suitable for journal publication**, following OARSI Author Guidelines (<https://www.oarsijournal.com/content/authorinfo>). Include:
- Study design and setting
- Data sources, inclusion/exclusion criteria
- Outcomes (dependent variables) and units
- Predictors and covariates (including `stage_early`)
- Data preprocessing, transformations, missing-data handling
- Statistical models: logistic, linear, and survival (Cox PH)
  - Include mathematical equations for main and adjusted model variants.
  - Define all symbols, parameters, and units.
  - Describe model diagnostics and significance thresholds.

### 4) EDA (Exploratory Data Analysis)
Use the **EDA code already implemented in `ettler_analysis_standalone.R`** to generate summaries and visualizations. Optionally, you may supplement outputs with `dlookr` if needed (e.g., for descriptive summaries or data quality assessments). Add concise interpretations after each figure or table.

### 5) Statistics
Two major subsections:

#### 5.1 Main models
For each dependent variable:
- **Modeling** — specify formula, family, and covariates used (including `stage_early`).
- **Tables** — model estimates with confidence intervals and p-values; followed by descriptive text.
- **Figures** — plots defined in `ettler_analysis_standalone.R`, downloadable in TIFF.

#### 5.2 Adjusted models
Same structure as Main models, including additional covariates.
In the **Figures** section of the Adjusted models, include **tabbed visualization layouts**:  
  - The **first row** should correspond to each dependent variable.  
  - The **second row** should correspond to each covariate.  
This layout allows side-by-side comparison between outcomes and covariate effects.

### 6) Conclusion
Summarize main findings and implications concisely.

### 7) Session info
Append a reproducibility section (`sessionInfo()`), following `ettler_project_03-c.Rmd` style.

---

## Technical notes

- **YAML and styling** must be directly reused from `ettler_project_03-c.Rmd`.
- **No example code** should be added; plotting and modeling code exists in `ettler_analysis_standalone.R`.
- Use **helper functions** from `scripts/functions/FUN.R` where applicable.
- Add **download buttons** for tables (Excel) and figures (TIFF) in the HTML output.
- Use the `here` package for all file paths.

---

## Rendering instructions

Run from R console or VS Code terminal:
```r
rmarkdown::render(here::here("reports", "251106_ettler_survival_02.Rmd"))
```

The resulting report must be reproducible, publication-ready, and aligned with the Ettler project report standards.

