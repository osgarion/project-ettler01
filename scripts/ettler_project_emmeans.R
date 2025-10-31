#!/usr/bin/env Rscript

# ------------------------------------------------------------------
# Ettler Project - Enhanced Self-contained Analysis Script
# Purpose: Perform validation, EDA, and modeling using GLM, COXPH, and LM.
# Improvement: Uses emmeans::emmeans() and contrast() to compute effects
#              between stage_early levels 0 vs 1 instead of tidy() alone.
# ------------------------------------------------------------------

## 00) Setup --------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(skimr)
  library(rio)
  library(broom)
  library(survival)
  library(survminer)
  library(emmeans)   # <-- NEW: for marginal means and contrasts
  tryCatch(library(naniar), error = function(e) message("naniar not available"))
})

set.seed(123)

# Output directory setup
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("reports", showWarnings = FALSE, recursive = TRUE)

message("Starting Ettler analysis with emmeans integration...")

## 01) Helper Functions ---------------------------------------------------

# Validate file path existence (suggest closest matches if missing)
check_and_fuzzy_path <- function(path) {
  if (file.exists(path)) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  repo_files <- list.files(".", recursive = TRUE)
  d <- utils::adist(path, repo_files)
  ord <- order(as.numeric(d))
  stop(sprintf("Path not found: %s\nClosest matches:\n- %s", path, paste(head(repo_files[ord], 5), collapse = "\n- ")), call. = FALSE)
}

# Coerce stage_early to binary factor with reference level "0"
coerce_binary_factor <- function(x, ref = "0") {
  x <- as.factor(x)
  if (ref %in% levels(x)) x <- relevel(x, ref = ref)
  return(x)
}

# Nicely format confidence intervals
fmt_ci <- function(low, high) sprintf("[%.3f, %.3f]", low, high)

# ------------------------------------------------------------------
# NEW: Effect extractors using emmeans
# ------------------------------------------------------------------

# Logistic regression (aOR for stage_early == 1 vs 0)
extract_emmeans_logistic <- function(model, term = "stage_early") {
  tryCatch({
    em <- emmeans::emmeans(model, reformulate(term))
    con <- emmeans::contrast(em, method = "revpairwise")
    est <- broom::tidy(con)
    tibble::tibble(
      estimate = exp(est$estimate),
      conf.low = exp(est$conf.low),
      conf.high = exp(est$conf.high),
      p.value = est$p.value
    )
  }, error = function(e) tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
}

# Cox proportional hazards (HR for stage_early == 1 vs 0)
extract_emmeans_cox <- function(model, term = "stage_early") {
  tryCatch({
    em <- emmeans::emmeans(model, reformulate(term))
    con <- emmeans::contrast(em, method = "revpairwise")
    est <- broom::tidy(con)
    tibble::tibble(
      estimate = exp(est$estimate),
      conf.low = exp(est$conf.low),
      conf.high = exp(est$conf.high),
      p.value = est$p.value
    )
  }, error = function(e) tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
}

# Linear regression (difference in means for stage_early == 1 vs 0)
extract_emmeans_linear <- function(model, term = "stage_early") {
  tryCatch({
    em <- emmeans::emmeans(model, reformulate(term))
    con <- emmeans::contrast(em, method = "revpairwise")
    est <- broom::tidy(con)
    tibble::tibble(
      estimate = est$estimate,
      conf.low = est$conf.low,
      conf.high = est$conf.high,
      p.value = est$p.value
    )
  }, error = function(e) tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
}

## 02) Load Data ----------------------------------------------------------
# Load dataset and ensure correct factor levels for stage_early
data_file <- "reports/markD_03.RData"
if (!exists("d04")) {
  if (!file.exists(data_file)) check_and_fuzzy_path(data_file)
  load(data_file)
}
if (!exists("d04")) stop("Dataset 'd04' not found.")

df <- d04 |> janitor::clean_names()
df$stage_early <- coerce_binary_factor(df$stage_early, ref = "0")

## 03) Logistic Regression ------------------------------------------------
message("Running logistic regression using emmeans...")
mod_log <- glm(response_achieved ~ stage_early, data = df, family = binomial())
eff_log <- extract_emmeans_logistic(mod_log)
readr::write_csv(eff_log, "output/tables/effect_logistic_emmeans.csv")

## 04) Cox Proportional Hazards -------------------------------------------
message("Running Cox proportional hazards model using emmeans...")
mod_cox <- coxph(Surv(time_to_next_treatmen, ttnt_achieved) ~ stage_early, data = df, x = TRUE, model = TRUE)
eff_cox <- extract_emmeans_cox(mod_cox)
readr::write_csv(eff_cox, "output/tables/effect_cox_emmeans.csv")

## 05) Linear Regression --------------------------------------------------
message("Running linear regression using emmeans...")
mod_lm <- lm(response_time_to ~ stage_early, data = df)
eff_lm <- extract_emmeans_linear(mod_lm)
readr::write_csv(eff_lm, "output/tables/effect_linear_emmeans.csv")

## 06) Summary Output -----------------------------------------------------
message("Combining and saving all model summaries...")
summary_emmeans <- bind_rows(
  eff_log |> mutate(model = "Logistic"),
  eff_cox |> mutate(model = "CoxPH"),
  eff_lm |> mutate(model = "Linear")
)

readr::write_csv(summary_emmeans, "output/tables/summary_emmeans_models.csv")

message("Analysis complete. Results saved under 'output/tables/'.")



### emmeans effect extractors ----

# Logistic model effect using emmeans (log-odds to OR)
extract_emmeans_logistic <- function(model, term = "stage") {
  tryCatch({
    em <- emmeans::emmeans(model, reformulate(term))
    con <- emmeans::contrast(em, method = "revpairwise")
    est <- broom::tidy(con, conf.int = TRUE)
    tibble::tibble(
      estimate = exp(est$estimate),
      conf.low = exp(est$conf.low),
      conf.high = exp(est$conf.high),
      p.value = est$p.value
    )
  }, error = function(e) tibble::tibble(
    estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_
  ))
}

# Cox PH model effect using emmeans (log-HR to HR)
extract_emmeans_cox <- function(model, term = "stage") {
  tryCatch({
    em <- emmeans::emmeans(model, reformulate(term))
    con <- emmeans::contrast(em, method = "revpairwise")
    est <- broom::tidy(con, conf.int = TRUE)
    tibble::tibble(
      estimate = exp(est$estimate),
      conf.low = exp(est$conf.low),
      conf.high = exp(est$conf.high),
      p.value = est$p.value
    )
  }, error = function(e) tibble::tibble(
    estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_
  ))
}

# Linear model effect using emmeans (difference in means)
extract_emmeans_linear <- function(model, term = "stage") {
  tryCatch({
    em <- emmeans::emmeans(model, reformulate(term))
    con <- emmeans::contrast(em, method = "revpairwise")
    est <- broom::tidy(con, conf.int = TRUE)
    tibble::tibble(
      estimate = est$estimate,
      conf.low = est$conf.low,
      conf.high = est$conf.high,
      p.value = est$p.value
    )
  }, error = function(e) tibble::tibble(
    estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_
  ))
}
