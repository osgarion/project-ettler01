#!/usr/bin/env Rscript

# Ettler project: self-contained analysis script
# - Validates inputs
# - Runs EDA and saves figures
# - Fits main and adjusted models (logistic, Cox, linear)
# - Outputs two CSV summary tables

## 00) Setup --------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(skimr)
  library(rio)  # CRITICAL: Added rio for import() function
  library(broom)
  library(survival)
  library(survminer)
  library(readr)
  library(stringr)
  library(purrr)
  library(forcats)
  # Optional packages with error handling
  tryCatch(library(naniar), error = function(e) message("naniar not available, using base functions"))
  tryCatch(library(broom.helpers), error = function(e) message("broom.helpers not available"))
  tryCatch(library(emmeans), error = function(e) message("emmeans not available"))
  # align with project styles
  suppressWarnings({
    requireNamespace("sjPlot", quietly = TRUE)
    requireNamespace("paletteer", quietly = TRUE)
  })
})

set.seed(123)

# Paths (template and helper exist only for validation/context)
template_rmd <- "reports/ettler_project_03-c.Rmd"
helper_script <- "scripts/Script_project_ettler_03.R"

# Output dirs per spec
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("reports", showWarnings = FALSE, recursive = TRUE)

message("Starting Ettler autogen analysis...")

## 01) Helper functions ---------------------------------------------------

# Fuzzy path check using base adist (no external dependency)
check_and_fuzzy_path <- function(path) {
  if (file.exists(path)) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  repo_files <- list.files(".", recursive = TRUE, all.files = FALSE, full.names = FALSE)
  # Order by increasing edit distance, then by nchar
  d <- utils::adist(path, repo_files)
  ord <- order(as.numeric(d), nchar(repo_files))
  suggestions <- unique(repo_files[ord])[1:min(5, length(repo_files))]
  stop(
    sprintf(
      "Path not found: %s\nClosest matches:\n- %s",
      path, paste0(suggestions, collapse = "\n- ")
    ), call. = FALSE
  )
}

# Coerce to binary factor with a specified reference level
coerce_binary_factor <- function(x, ref) {
  x <- as.factor(x)
  levs <- levels(x)
  if (!ref %in% levs) {
    stop(sprintf("Reference level '%s' not found in levels: %s", ref, paste(levs, collapse = ", ")))
  }
  x <- stats::relevel(x, ref = ref)
  return(x)
}

# Format effect (optional pretty string)
fmt_effect <- function(effect, ci_low, ci_high, model_type = c("logistic", "cox", "linear")) {
  model_type <- match.arg(model_type)
  unit <- switch(model_type, logistic = "aOR", cox = "HR", linear = "beta")
  sprintf("%s=%.3f [%.3f, %.3f]", unit, effect, ci_low, ci_high)
}

fmt_ci <- function(ci_low, ci_high) {
  # Vectorized CI formatter
  ci_low <- as.numeric(ci_low)
  ci_high <- as.numeric(ci_high)
  out <- ifelse(is.na(ci_low) | is.na(ci_high), NA_character_, sprintf("[%.3f, %.3f]", ci_low, ci_high))
  return(out)
}

# Tidy extractors
pick_contrast_level <- function(x) {
  xf <- as.factor(x)
  levs <- levels(xf)
  if (length(levs) <= 1) return(NULL)
  ref <- levs[1]
  cand <- levs[levs != ref]
  if (length(cand) == 0) return(NULL)
  cand_lower <- tolower(cand)
  # preference order
  if ("1" %in% cand) return("1")
  if (any(cand_lower == "yes")) return(cand[cand_lower == "yes"][1])
  # most frequent non-ref level
  tab <- sort(table(xf[xf != ref]), decreasing = TRUE)
  top <- names(tab)[1]
  if (!is.null(top) && length(top)) return(top)
  cand[1]
}

extract_logistic_effects <- function(model, term, contrast_label = NULL) {
  tt <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  tt <- dplyr::filter(tt, .data$term == !!term | stringr::str_starts(.data$term, paste0("^", term)))
  if (nrow(tt) == 0) return(tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
  if (!is.null(contrast_label)) {
    # match suffix after the base term
    suffix <- sub(paste0("^", term), "", tt$term)
    hit <- which(suffix == contrast_label)
    if (length(hit) >= 1) tt <- tt[hit[1], , drop = FALSE]
    else tt <- tt[1, , drop = FALSE]
  } else {
    tt <- tt[1, , drop = FALSE]
  }
  dplyr::transmute(tt, estimate = .data$estimate, conf.low = .data$conf.low, conf.high = .data$conf.high, p.value = .data$p.value)
}

extract_cox_effects <- function(model, term, contrast_label = NULL) {
  tt <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  tt <- dplyr::filter(tt, .data$term == !!term | stringr::str_starts(.data$term, paste0("^", term)))
  if (nrow(tt) == 0) return(tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
  if (!is.null(contrast_label)) {
    suffix <- sub(paste0("^", term), "", tt$term)
    hit <- which(suffix == contrast_label)
    if (length(hit) >= 1) tt <- tt[hit[1], , drop = FALSE]
    else tt <- tt[1, , drop = FALSE]
  } else {
    tt <- tt[1, , drop = FALSE]
  }
  dplyr::transmute(tt, estimate = .data$estimate, conf.low = .data$conf.low, conf.high = .data$conf.high, p.value = .data$p.value)
}

extract_linear_effects <- function(model, term, contrast_label = NULL) {
  tt <- broom::tidy(model, conf.int = TRUE)
  tt <- dplyr::filter(tt, .data$term == !!term | stringr::str_starts(.data$term, paste0("^", term)))
  if (nrow(tt) == 0) return(tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
  if (!is.null(contrast_label)) {
    suffix <- sub(paste0("^", term), "", tt$term)
    hit <- which(suffix == contrast_label)
    if (length(hit) >= 1) tt <- tt[hit[1], , drop = FALSE]
    else tt <- tt[1, , drop = FALSE]
  } else {
    tt <- tt[1, , drop = FALSE]
  }
  dplyr::transmute(tt, estimate = .data$estimate, conf.low = .data$conf.low, conf.high = .data$conf.high, p.value = .data$p.value)
}

# Stage-specific wrappers (fit separate models by stage)
fit_stage_specific_logistic <- function(df, formula, stage_col = "stage_early", contrast_label = NULL) {
  stopifnot(stage_col %in% names(df))
  df <- df |>
    dplyr::filter(!is.na(.data[[stage_col]]))
  st_levels <- sort(unique(df[[stage_col]]))
  res <- list()
  for (s in st_levels) {
    d_sub <- df[df[[stage_col]] == s, , drop = FALSE]
    dropped <- nrow(df) - nrow(stats::na.omit(model.frame(formula, data = d_sub)))
    if (nrow(d_sub) < 5) {
      res[[as.character(s)]] <- tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_, n = nrow(d_sub), dropped = dropped)
      next
    }
    m <- stats::glm(formula, data = d_sub, family = stats::binomial())
    term <- setdiff(attr(stats::terms(formula), "term.labels"), "(Intercept)")[1]
    eff <- tryCatch(extract_logistic_effects(m, term, contrast_label = contrast_label), error = function(e) tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
    res[[as.character(s)]] <- dplyr::bind_cols(eff, tibble::tibble(n = nrow(d_sub), dropped = dropped))
  }
  res
}

# Ensure a survival event vector is binary (0/1) numeric
ensure_binary_event <- function(v) {
  if (is.logical(v)) return(as.integer(v))
  if (is.numeric(v)) return(as.integer(v > 0))
  if (is.factor(v) || is.character(v)) {
    lv <- tolower(as.character(v))
    yes <- c("1","y","yes","true","event","occurred","dead","progressed","failure")
    no  <- c("0","n","no","false","censored","alive","none","success")
    out <- ifelse(lv %in% yes, 1L, ifelse(lv %in% no, 0L, NA_integer_))
    return(out)
  }
  suppressWarnings(as.integer(as.numeric(v) > 0))
}

fit_stage_specific_cox <- function(df, time_col, event_col, covar, stage_col = "stage_early", contrast_label = NULL) {
  stopifnot(all(c(time_col, event_col, covar, stage_col) %in% names(df)))
  df <- df |>
    dplyr::filter(!is.na(.data[[stage_col]]))
  st_levels <- sort(unique(df[[stage_col]]))
  res <- list()
  for (s in st_levels) {
    d_sub <- df[df[[stage_col]] == s, , drop = FALSE]
    mf <- model.frame(stats::as.formula(sprintf("survival::Surv(%s, %s) ~ %s", time_col, event_col, covar)), data = d_sub)
    dropped <- nrow(d_sub) - nrow(stats::na.omit(mf))
    if (nrow(mf) < 5) {
      res[[as.character(s)]] <- tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_, n = nrow(d_sub), dropped = dropped)
      next
    }
    m <- survival::coxph(stats::as.formula(sprintf("survival::Surv(%s, %s) ~ %s", time_col, event_col, covar)), data = d_sub, x = TRUE)
    eff <- tryCatch(extract_cox_effects(m, covar, contrast_label = contrast_label), error = function(e) tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
    res[[as.character(s)]] <- dplyr::bind_cols(eff, tibble::tibble(n = nrow(d_sub), dropped = dropped))
  }
  res
}

fit_stage_specific_linear <- function(df, formula, stage_col = "stage_early", contrast_label = NULL) {
  stopifnot(stage_col %in% names(df))
  df <- df |>
    dplyr::filter(!is.na(.data[[stage_col]]))
  st_levels <- sort(unique(df[[stage_col]]))
  res <- list()
  for (s in st_levels) {
    d_sub <- df[df[[stage_col]] == s, , drop = FALSE]
    dropped <- nrow(d_sub) - nrow(stats::na.omit(model.frame(formula, data = d_sub)))
    if (nrow(d_sub) < 5) {
      res[[as.character(s)]] <- tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_, n = nrow(d_sub), dropped = dropped)
      next
    }
    m <- stats::lm(formula, data = d_sub)
    term <- setdiff(attr(stats::terms(formula), "term.labels"), "(Intercept)")[1]
    eff <- tryCatch(extract_linear_effects(m, term, contrast_label = contrast_label), error = function(e) tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
    res[[as.character(s)]] <- dplyr::bind_cols(eff, tibble::tibble(n = nrow(d_sub), dropped = dropped))
  }
  res
}

## 02) Validate inputs ----------------------------------------------------
message("Validating expected input files...")
invisible(tryCatch(check_and_fuzzy_path(template_rmd), error = function(e) stop(e)))
invisible(tryCatch(check_and_fuzzy_path(helper_script), error = function(e) stop(e)))

## 03) Load functions/objects -------------------------------------------
# Source all helpers and objects from scripts/functions as requested
fn_files <- list.files("scripts/functions", pattern = "\\.R$", full.names = TRUE)
if (length(fn_files)) {
  message("Sourcing helpers from scripts/functions ...")
  invisible(lapply(fn_files, function(f) {
    tryCatch({
      sys.source(f, envir = .GlobalEnv)
      message(" - sourced ", basename(f))
    }, error = function(e) {
      message(" ! could not source ", basename(f), ": ", e$message)
    })
  }))
}

## 04) Load data ----------------------------------------------------------
# Prefer objects created by sourced files (OBJ_01.R defines d04)
if (!exists("d04")) {
  message("'d04' not found after sourcing. Trying to load reports/markD_03.RData ...")
  data_rdata <- "reports/markD_03.RData"
  if (!file.exists(data_rdata)) {
    invisible(tryCatch(check_and_fuzzy_path(data_rdata), error = function(e) stop(e)))
  }
  load(data_rdata)
}

if (!exists("d04")) {
  stop("Expected dataset 'd04' not found. Ensure scripts/functions/OBJ_01.R creates it or reports/markD_03.RData contains it.")
}

# Column hygiene and canonical names
df <- d04 |>
  janitor::clean_names()

# Ensure key variables exist or map aliases
alias_map <- list(
  time_to_next_treatmen = c("ttnt"),
  ttnt_achieved = c("ttnt_achieved"),
  treatment_duration = c("treatment_duration"),
  discontinuation_reason = c("discontinuation_reason"),
  response_duration = c("response_duration"),
  progression = c("progression"),
  response_achieved = c("response_achieved"),
  response_time_to = c("response_time_to"),
  stage_early = c("stage_early")
)

resolve_col <- function(df, target, aliases) {
  # Return the first existing alias or the target if present
  choices <- unique(c(target, aliases))
  hit <- choices[choices %in% names(df)]
  if (length(hit) == 0) stop(sprintf("Missing required column: %s (aliases: %s)", target, paste(aliases, collapse = ", ")))
  hit[1]
}

col_time_ttnt <- resolve_col(df, "time_to_next_treatmen", alias_map$time_to_next_treatmen)
col_event_ttnt <- resolve_col(df, "ttnt_achieved", alias_map$ttnt_achieved)
col_treat_duration <- resolve_col(df, "treatment_duration", alias_map$treatment_duration)
col_event_discont <- resolve_col(df, "discontinuation_reason", alias_map$discontinuation_reason)
col_response_duration <- resolve_col(df, "response_duration", alias_map$response_duration)
col_event_progression <- resolve_col(df, "progression", alias_map$progression)
col_response_achieved <- resolve_col(df, "response_achieved", alias_map$response_achieved)
col_response_time_to <- resolve_col(df, "response_time_to", alias_map$response_time_to)
col_stage <- resolve_col(df, "stage_early", alias_map$stage_early)

# Coerce stage to a sensible binary factor with 0 as reference when possible
if (is.numeric(df[[col_stage]]) || is.logical(df[[col_stage]])) {
  df[[col_stage]] <- factor(df[[col_stage]])
}
if (is.factor(df[[col_stage]]) && "0" %in% levels(df[[col_stage]])) {
  df[[col_stage]] <- coerce_binary_factor(df[[col_stage]], ref = "0")
} else if (is.factor(df[[col_stage]]) && "No" %in% levels(df[[col_stage]])) {
  df[[col_stage]] <- coerce_binary_factor(df[[col_stage]], ref = "No")
} else {
  # leave as is, but ensure factor
  df[[col_stage]] <- factor(df[[col_stage]])
}

## 05) EDA ---------------------------------------------------------------
message("Running EDA and saving figures to 'output/figures/'...")

# Snapshot and missingness
capture.output({
  cat("\nData snapshot (str):\n"); str(df)
  cat("\nSkim summary:\n"); print(skimr::skim(df))
  cat("\nMissingness summary:\n"); print(naniar::miss_var_summary(df))
}, file = "reports/eda_console_log.txt")
message(" - Wrote console summaries to reports/eda_console_log.txt")

# Distributions: numeric (styled)
num_cols <- df |> dplyr::select(where(is.numeric)) |> names()
if (length(num_cols) > 0) {
  p_num <- df |>
    dplyr::select(dplyr::all_of(num_cols)) |>
    tidyr::pivot_longer(cols = everything(), names_to = "var", values_to = "val") |>
    tidyr::drop_na(val) |>
    ggplot(aes(x = val)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    facet_wrap(~ var, scales = "free", ncol = 3) +
    labs(title = "Numeric distributions") +
    { if ("sjPlot" %in% .packages(all.available = TRUE)) sjPlot::theme_sjplot2() else theme_minimal(base_size = 12) }
  ggsave("output/figures/eda_numeric_distributions.png", p_num, width = 10, height = 7, dpi = 150)
}

# Distributions: categorical (styled like project barplots)
cat_cols <- df |> dplyr::select(where(~ is.factor(.x) || is.character(.x))) |> names()
if (length(cat_cols) > 0) {
  cat_long <- df |>
    dplyr::select(dplyr::all_of(cat_cols)) |>
    dplyr::mutate(dplyr::across(everything(), as.factor)) |>
    tidyr::pivot_longer(cols = everything(), names_to = "var", values_to = "val") |>
    tidyr::drop_na(val)

  n_levels <- length(unique(cat_long$val))
  p_cat <- cat_long |>
    ggplot(aes(x = val, fill = val)) +
    geom_bar(position = "fill") +
    facet_wrap(~ var, scales = "free", ncol = 3) +
    {
      if (n_levels <= 7 && "paletteer" %in% .packages(all.available = TRUE)) {
        paletteer::scale_fill_paletteer_d("waRhol::marilyn_orange_62")
      } else {
        scale_fill_hue()
      }
    } +
    { if ("sjPlot" %in% .packages(all.available = TRUE)) sjPlot::theme_sjplot2() else theme_minimal(base_size = 12) } +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Categorical distributions (proportions)", x = NULL, y = "Proportion", fill = "Level")
  ggsave("output/figures/eda_categorical_distributions.png", p_cat, width = 12, height = 8, dpi = 150)
}

# Survival KM previews by stage_early
km_specs <- list(
  list(time = col_time_ttnt, event = col_event_ttnt, label = "time_to_next_treatment"),
  list(time = col_treat_duration, event = col_event_discont, label = "treatment_duration"),
  list(time = col_response_duration, event = col_event_progression, label = "response_duration")
)

for (sp in km_specs) {
  time_col <- sp$time; event_col <- sp$event
  df_km <- df |>
    dplyr::select(time = dplyr::all_of(time_col), event = dplyr::all_of(event_col), stage = dplyr::all_of(col_stage)) |>
    dplyr::mutate(
      time = suppressWarnings(readr::parse_number(as.character(.data$time))),
      event = ensure_binary_event(.data$event),
      stage = as.factor(.data$stage)
    ) |>
    dplyr::filter(!is.na(time), !is.na(event), !is.na(stage))
  if (nrow(df_km) > 0) {
    f <- survival::survfit(survival::Surv(time, event) ~ stage, data = df_km)
    p <- ggsurvplot(
      fit = f,
      data = df_km,
      risk.table = TRUE,
      conf.int = TRUE,
      ggtheme = theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold")),
      legend.title = "Stage",
      title = paste0("KM: ", sp$label)
    )
    out <- file.path("output/figures", paste0("eda_km_", sp$label, ".png"))
    ggplot2::ggsave(out, p$plot, width = 8, height = 6, dpi = 150)
  }
}

## 06) Models: main effects ----------------------------------------------
message("Fitting main models and assembling summary_main_models.csv ...")

# Logistic: response_achieved ~ stage_early
df_log <- df |>
  dplyr::select(y = dplyr::all_of(col_response_achieved), stage = dplyr::all_of(col_stage)) |>
  dplyr::filter(!is.na(y), !is.na(stage)) |>
  dplyr::mutate(y = as.factor(y))
drop_log <- nrow(df_log) - nrow(stats::na.omit(model.frame(y ~ stage, data = df_log)))
mod_log <- tryCatch(stats::glm(y ~ stage, data = df_log, family = stats::binomial()), error = function(e) NULL)
log_all <- if (!is.null(mod_log)) extract_logistic_effects(mod_log, term = "stage") else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)

# Stage-specific subgroup summaries: odds (intercept-only models per stage)
log_levels <- levels(as.factor(df_log$stage))
if (length(log_levels) >= 2) {
  s0 <- dplyr::filter(df_log, as.character(stage) == log_levels[1])
  s1 <- dplyr::filter(df_log, as.character(stage) == log_levels[2])
  m0 <- tryCatch(stats::glm(y ~ 1, data = s0, family = stats::binomial()), error = function(e) NULL)
  m1 <- tryCatch(stats::glm(y ~ 1, data = s1, family = stats::binomial()), error = function(e) NULL)
  log_s0 <- if (!is.null(m0)) {
    tt <- broom::tidy(m0, conf.int = TRUE, exponentiate = TRUE)
    dplyr::transmute(dplyr::filter(tt, .data$term == "(Intercept)"), estimate = .data$estimate, conf.low = .data$conf.low, conf.high = .data$conf.high, p.value = .data$p.value)
  } else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
  log_s1 <- if (!is.null(m1)) {
    tt <- broom::tidy(m1, conf.int = TRUE, exponentiate = TRUE)
    dplyr::transmute(dplyr::filter(tt, .data$term == "(Intercept)"), estimate = .data$estimate, conf.low = .data$conf.low, conf.high = .data$conf.high, p.value = .data$p.value)
  } else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
} else {
  log_s0 <- tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
  log_s1 <- tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
}

# Cox PH: three endpoints Surv(time, event) ~ stage_early
cox_main <- list()
ph_flags <- list()
for (spec in km_specs) {
  time_col <- spec$time; event_col <- spec$event; outname <- spec$label
  d_surv <- df |>
    dplyr::select(time = dplyr::all_of(time_col), event = dplyr::all_of(event_col), stage = dplyr::all_of(col_stage)) |>
    dplyr::mutate(
      time = suppressWarnings(readr::parse_number(as.character(.data$time))),
      event = ensure_binary_event(.data$event),
      stage = as.factor(.data$stage)
    ) |>
    dplyr::filter(!is.na(time), !is.na(event), !is.na(stage))
  # Log input audit for survival endpoints
  if (!exists(".surv_audit_initialized", inherits = FALSE)) {
    assign(".surv_audit_initialized", TRUE, envir = .GlobalEnv)
    writeLines("endpoint,n_total,n_complete,n_events,time_min,time_max", con = "reports/surv_input_audit.csv")
  }
  if (nrow(d_surv) > 0) {
    n_total <- nrow(df)
    n_complete <- nrow(d_surv)
    n_events <- sum(d_surv$event == 1, na.rm = TRUE)
    tmin <- suppressWarnings(min(d_surv$time, na.rm = TRUE))
    tmax <- suppressWarnings(max(d_surv$time, na.rm = TRUE))
    cat(paste(outname, n_total, n_complete, n_events, tmin, tmax, sep=","), file = "reports/surv_input_audit.csv", append = TRUE)
    cat("\n", file = "reports/surv_input_audit.csv", append = TRUE)
  }
  if (nrow(d_surv) > 0) {
    mf <- stats::model.frame(stats::as.formula("survival::Surv(time, event) ~ stage"), data = d_surv)
    drop_surv <- nrow(d_surv) - nrow(stats::na.omit(mf))
    m <- survival::coxph(survival::Surv(time, event) ~ stage, data = d_surv, x = TRUE)
    ph <- tryCatch({
      cz <- survival::cox.zph(m); all(is.finite(cz$table[, "p"]) & cz$table[, "p"] > 0.05)
    }, error = function(e) NA)
    eff <- extract_cox_effects(m, term = "stage")
  } else {
    drop_surv <- NA_integer_
    ph <- NA
    eff <- tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
  }
  cox_main[[outname]] <- dplyr::bind_cols(tibble::tibble(outcome = outname), eff, tibble::tibble(dropped = drop_surv))
  ph_flags[[outname]] <- ph
}

# Linear: response_time_to ~ stage_early (filter positive times as in Rmd)
df_lin <- df |>
  dplyr::select(y = dplyr::all_of(col_response_time_to), stage = dplyr::all_of(col_stage)) |>
  dplyr::filter(!is.na(y), y > 0, !is.na(stage))
drop_lin <- nrow(df_lin) - nrow(stats::na.omit(model.frame(y ~ stage, data = df_lin)))
mod_lin <- tryCatch(stats::lm(y ~ stage, data = df_lin), error = function(e) NULL)
lin_all <- if (!is.null(mod_lin)) extract_linear_effects(mod_lin, term = "stage") else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)

# Stage-specific subgroup summaries: means (intercept-only models per stage)
lin_levels <- levels(as.factor(df_lin$stage))
if (length(lin_levels) >= 2) {
  s0 <- dplyr::filter(df_lin, as.character(stage) == lin_levels[1])
  s1 <- dplyr::filter(df_lin, as.character(stage) == lin_levels[2])
  m0 <- tryCatch(stats::lm(y ~ 1, data = s0), error = function(e) NULL)
  m1 <- tryCatch(stats::lm(y ~ 1, data = s1), error = function(e) NULL)
  lin_s0 <- if (!is.null(m0)) {
    tt <- broom::tidy(m0, conf.int = TRUE)
    dplyr::transmute(dplyr::slice(tt, 1L), estimate = .data$estimate, conf.low = .data$conf.low, conf.high = .data$conf.high, p.value = .data$p.value)
  } else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
  lin_s1 <- if (!is.null(m1)) {
    tt <- broom::tidy(m1, conf.int = TRUE)
    dplyr::transmute(dplyr::slice(tt, 1L), estimate = .data$estimate, conf.low = .data$conf.low, conf.high = .data$conf.high, p.value = .data$p.value)
  } else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
} else {
  lin_s0 <- tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
  lin_s1 <- tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
}

# Assemble main table
main_rows <- list(
  tibble::tibble(
    outcome = "response_achieved",
    p_all = log_all$p.value,
    effect_all = log_all$estimate,
    effect_all_ci = fmt_ci(log_all$conf.low, log_all$conf.high),
    p_stage0 = log_s0$p.value,
    effect_stage0 = log_s0$estimate,
    effect_stage0_ci = fmt_ci(log_s0$conf.low, log_s0$conf.high),
    p_stage1 = log_s1$p.value,
    effect_stage1 = log_s1$estimate,
    effect_stage1_ci = fmt_ci(log_s1$conf.low, log_s1$conf.high)
  ),
  dplyr::bind_rows(cox_main) |>
    dplyr::transmute(
      outcome = .data$outcome,
      p_all = .data$p.value,
      effect_all = .data$estimate,
      effect_all_ci = fmt_ci(.data$conf.low, .data$conf.high),
      p_stage0 = NA_real_, effect_stage0 = NA_real_, effect_stage0_ci = NA_character_,
      p_stage1 = NA_real_, effect_stage1 = NA_real_, effect_stage1_ci = NA_character_
    ),
  tibble::tibble(
    outcome = "response_time_to",
    p_all = lin_all$p.value,
    effect_all = lin_all$estimate,
    effect_all_ci = fmt_ci(lin_all$conf.low, lin_all$conf.high),
    p_stage0 = lin_s0$p.value,
    effect_stage0 = lin_s0$estimate,
    effect_stage0_ci = fmt_ci(lin_s0$conf.low, lin_s0$conf.high),
    p_stage1 = lin_s1$p.value,
    effect_stage1 = lin_s1$estimate,
    effect_stage1_ci = fmt_ci(lin_s1$conf.low, lin_s1$conf.high)
  )
)

summary_main_models <- dplyr::bind_rows(main_rows) |>
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 4)))

readr::write_csv(summary_main_models, file = "output/tables/summary_main_models.csv")
if (requireNamespace("rio", quietly = TRUE)) rio::export(summary_main_models, "output/tables/summary_main_models.xlsx")
message(" - Wrote output/tables/summary_main_models.csv")

# Log PH checks
if (length(ph_flags)) {
  ph_lines <- sprintf("%s: ph_ok=%s", names(ph_flags), unlist(ph_flags))
  writeLines(ph_lines, con = "reports/ph_checks.txt")
  message(" - Wrote reports/ph_checks.txt")
}

## 07) Models: sequential adjustments ------------------------------------
message("Fitting adjusted models and assembling summary_adjusted_models.csv ...")

covars <- c("age", "sex", "bmi", "ps_ecog", "first_syst_th", "dyslipidemia_before", "thyroid_disease_before", "monotherapy")
covars <- covars[covars %in% names(df)]
if (length(covars) == 0) {
  warning("No additional covariates found; producing empty adjusted table.")
}

adjusted_rows <- list()

# Helper to coerce types: treat character as factor
coerce_predictor <- function(v) {
  if (is.character(v)) as.factor(v) else v
}

for (cv in covars) {
  # Logistic: response_achieved ~ stage + cv
  d_mod <- df |>
    dplyr::select(y = dplyr::all_of(col_response_achieved), stage = dplyr::all_of(col_stage), x = dplyr::all_of(cv)) |>
    dplyr::mutate(y = as.factor(y), x = coerce_predictor(x)) |>
    dplyr::filter(!is.na(y), !is.na(stage), !is.na(x))
  m_all <- tryCatch(stats::glm(y ~ stage + x, data = d_mod, family = stats::binomial()), error = function(e) NULL)
  contrast_label <- if (is.factor(d_mod$x)) pick_contrast_level(d_mod$x) else NULL
  eff_all <- if (!is.null(m_all)) extract_logistic_effects(m_all, term = "x", contrast_label = contrast_label) else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
  # Stage splits
  sp_log <- fit_stage_specific_logistic(d_mod |> dplyr::rename(stage_early = stage), formula = stats::as.formula("y ~ x"), stage_col = "stage_early", contrast_label = contrast_label)
  nm_log <- names(sp_log)
  s0 <- if (length(nm_log) >= 1 && !is.null(sp_log[[nm_log[1]]])) dplyr::slice(sp_log[[nm_log[1]]], 1L) else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
  s1 <- if (length(nm_log) >= 2 && !is.null(sp_log[[nm_log[2]]])) dplyr::slice(sp_log[[nm_log[2]]], 1L) else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
  adjusted_rows[[length(adjusted_rows) + 1]] <- tibble::tibble(
    outcome = "response_achieved", covariate = cv,
    p_all = eff_all$p.value, effect_all = eff_all$estimate, effect_all_ci = fmt_ci(eff_all$conf.low, eff_all$conf.high),
    p_stage0 = s0$p.value, effect_stage0 = s0$estimate, effect_stage0_ci = fmt_ci(s0$conf.low, s0$conf.high),
    p_stage1 = s1$p.value, effect_stage1 = s1$estimate, effect_stage1_ci = fmt_ci(s1$conf.low, s1$conf.high)
  )

  # Cox: each survival endpoint ~ stage + cv; stage-specific via separate models
  for (spec in km_specs) {
    time_col <- spec$time; event_col <- spec$event; outname <- spec$label
    d_surv <- df |>
      dplyr::select(time = dplyr::all_of(time_col), event = dplyr::all_of(event_col), stage = dplyr::all_of(col_stage), x = dplyr::all_of(cv)) |>
      dplyr::mutate(
        time = suppressWarnings(readr::parse_number(as.character(.data$time))),
        event = ensure_binary_event(.data$event),
        stage = as.factor(.data$stage),
        x = coerce_predictor(x)
      ) |>
      dplyr::filter(!is.na(time), !is.na(event), !is.na(stage), !is.na(x))
    m_all <- tryCatch(survival::coxph(survival::Surv(time, event) ~ stage + x, data = d_surv, x = TRUE), error = function(e) NULL)
    contrast_label <- if (is.factor(d_surv$x)) pick_contrast_level(d_surv$x) else NULL
    eff_all <- if (!is.null(m_all)) extract_cox_effects(m_all, term = "x", contrast_label = contrast_label) else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
    sp_cox <- fit_stage_specific_cox(d_surv |> dplyr::rename(stage_early = stage), time_col = "time", event_col = "event", covar = "x", stage_col = "stage_early", contrast_label = contrast_label)
    nm_cox <- names(sp_cox)
    s0 <- if (length(nm_cox) >= 1 && !is.null(sp_cox[[nm_cox[1]]])) dplyr::slice(sp_cox[[nm_cox[1]]], 1L) else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
    s1 <- if (length(nm_cox) >= 2 && !is.null(sp_cox[[nm_cox[2]]])) dplyr::slice(sp_cox[[nm_cox[2]]], 1L) else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
    adjusted_rows[[length(adjusted_rows) + 1]] <- tibble::tibble(
      outcome = outname, covariate = cv,
      p_all = eff_all$p.value, effect_all = eff_all$estimate, effect_all_ci = fmt_ci(eff_all$conf.low, eff_all$conf.high),
      p_stage0 = s0$p.value, effect_stage0 = s0$estimate, effect_stage0_ci = fmt_ci(s0$conf.low, s0$conf.high),
      p_stage1 = s1$p.value, effect_stage1 = s1$estimate, effect_stage1_ci = fmt_ci(s1$conf.low, s1$conf.high)
    )
  }

  # Linear: response_time_to ~ stage + cv (y > 0)
  d_lin <- df |>
    dplyr::select(y = dplyr::all_of(col_response_time_to), stage = dplyr::all_of(col_stage), x = dplyr::all_of(cv)) |>
    dplyr::mutate(x = coerce_predictor(x)) |>
    dplyr::filter(!is.na(y), y > 0, !is.na(stage), !is.na(x))
  m_all <- tryCatch(stats::lm(y ~ stage + x, data = d_lin), error = function(e) NULL)
  contrast_label <- if (is.factor(d_lin$x)) pick_contrast_level(d_lin$x) else NULL
  eff_all <- if (!is.null(m_all)) extract_linear_effects(m_all, term = "x", contrast_label = contrast_label) else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
  sp_lin <- fit_stage_specific_linear(d_lin |> dplyr::rename(stage_early = stage), formula = stats::as.formula("y ~ x"), stage_col = "stage_early", contrast_label = contrast_label)
  nm_lin <- names(sp_lin)
  s0 <- if (length(nm_lin) >= 1 && !is.null(sp_lin[[nm_lin[1]]])) dplyr::slice(sp_lin[[nm_lin[1]]], 1L) else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
  s1 <- if (length(nm_lin) >= 2 && !is.null(sp_lin[[nm_lin[2]]])) dplyr::slice(sp_lin[[nm_lin[2]]], 1L) else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
  adjusted_rows[[length(adjusted_rows) + 1]] <- tibble::tibble(
    outcome = "response_time_to", covariate = cv,
    p_all = eff_all$p.value, effect_all = eff_all$estimate, effect_all_ci = fmt_ci(eff_all$conf.low, eff_all$conf.high),
    p_stage0 = s0$p.value, effect_stage0 = s0$estimate, effect_stage0_ci = fmt_ci(s0$conf.low, s0$conf.high),
    p_stage1 = s1$p.value, effect_stage1 = s1$estimate, effect_stage1_ci = fmt_ci(s1$conf.low, s1$conf.high)
  )
}

summary_adjusted_models <- dplyr::bind_rows(adjusted_rows) |>
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 4))) |>
  dplyr::relocate(outcome, covariate, p_all, effect_all, effect_all_ci, p_stage0, effect_stage0, effect_stage0_ci, p_stage1, effect_stage1, effect_stage1_ci)

readr::write_csv(summary_adjusted_models, file = "output/tables/summary_adjusted_models.csv")
if (requireNamespace("rio", quietly = TRUE)) rio::export(summary_adjusted_models, "output/tables/summary_adjusted_models.xlsx")
message(" - Wrote output/tables/summary_adjusted_models.csv")

## 08) Session info & done -----------------------------------------------
message("Analysis complete. Saving session info...")
sink("reports/session_info.txt"); utils::sessionInfo(); sink()
message("Done. Key outputs:\n - output/figures/eda_*.png\n - reports/eda_console_log.txt\n - output/tables/summary_main_models.csv\n - output/tables/summary_adjusted_models.csv")
