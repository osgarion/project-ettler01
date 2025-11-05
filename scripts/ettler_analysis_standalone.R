#!/usr/bin/env Rscript

# Header 1: Standalone Analysis Entry
# - Nepoužívá ettler_analysis_autogen*.R
# - Načte d04 stejně robustně jako v autogen skriptu
# - Fallback na reports/markD_03.RData
# - Generuje EDA + modely (preferuje v2 chování)

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(skimr)
  library(rio)
  library(broom)
  library(survival)
  library(survminer)
  library(readr)
  library(stringr)
  library(purrr)
  library(forcats)
  tryCatch(library(emmeans), error = function(e) message("emmeans not available"))
  tryCatch(library(naniar), error = function(e) message("naniar not available"))
  tryCatch(library(cowplot), error = function(e) message("cowplot not available"))
  suppressWarnings({
    requireNamespace("corrplot", quietly = TRUE)
    requireNamespace("Hmisc", quietly = TRUE)
    requireNamespace("sjPlot", quietly = TRUE)
    requireNamespace("paletteer", quietly = TRUE)
  })
})

set.seed(123)

# Header 2: Output Dirs
dir.create("output", showWarnings = FALSE, recursive = TRUE)
dir.create("reports", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures/main", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures/adjusted", showWarnings = FALSE, recursive = TRUE)
dir.create("output/figures/eda", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables/main", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables/adjusted", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables/eda", showWarnings = FALSE, recursive = TRUE)

message("Starting standalone import for d04 ...")

# Header 2: Robust Path + Sourcing Utils
resolve_proj_path <- function(path) {
  if (file.exists(path)) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  up <- file.path("..", path)
  if (file.exists(up)) return(normalizePath(up, winslash = "/", mustWork = FALSE))
  check_and_fuzzy_path(path)
}

# Utility: try a set of candidate paths and source the first that exists
try_source_first <- function(paths) {
  for (p in paths) {
    if (!file.exists(p)) next
    message(sprintf(" - Sourcing: %s (keep working dir)", p))
    ok <- tryCatch({ source(p, chdir = FALSE); TRUE },
                   error = function(e) { message(sprintf("   ! First attempt failed at %s: %s", p, conditionMessage(e))); FALSE })
    if (ok) return(TRUE)
    message("   - Retrying with chdir=TRUE (relative to script dir)...")
    ok2 <- tryCatch({ source(p, chdir = TRUE); TRUE },
                    error = function(e) { message(sprintf("   ! Retry failed at %s: %s", p, conditionMessage(e))); FALSE })
    if (ok2) return(TRUE)
  }
  FALSE
}

# Header 2: Primary Import (like autogen)
# Prefer helper script (creates d04), then OBJ_01.R variants
obj_candidates <- c(
  resolve_proj_path("scripts/Script_project_ettler_03.R"),
  resolve_proj_path("scripts/functions/OBJ_01.R"),
  resolve_proj_path("functions/OBJ_01.R"),
  resolve_proj_path("scripts/OBJ_01.R"),
  resolve_proj_path("OBJ_01.R")
)

ok_src <- try_source_first(obj_candidates)

if (!exists("d04", inherits = TRUE) || !is.data.frame(get("d04", inherits = TRUE))) {
  message("d04 not found after sourcing OBJ_01.R; trying fallback RData import ...")
  rdata_path <- resolve_proj_path("reports/markD_03.RData")
  if (!file.exists(rdata_path)) {
    stop(sprintf("Fallback file not found: %s", rdata_path), call. = FALSE)
  }
  # Try rio::import first
  d04_try <- try(rio::import(rdata_path), silent = TRUE)
  if (!inherits(d04_try, "try-error")) {
    if (is.data.frame(d04_try)) {
      d04 <- d04_try
    } else if (is.list(d04_try) && "d04" %in% names(d04_try)) {
      d04 <- d04_try[["d04"]]
    }
  }
  if (!exists("d04", inherits = FALSE)) {
    # Fallback to base load
    env <- new.env(parent = emptyenv())
    load(rdata_path, envir = env)
    if (exists("d04", envir = env, inherits = FALSE)) {
      d04 <- get("d04", envir = env)
    }
  }
}

if (!exists("d04", inherits = FALSE) || !is.data.frame(d04)) {
  stop("Expected dataset 'd04' not available after all import attempts.", call. = FALSE)
}

message(sprintf("d04 imported successfully: %d rows x %d cols", nrow(d04), ncol(d04)))

# Write a quick snapshot for downstream verification
capture.output({
  cat("\nstr(d04):\n"); str(d04)
  cat("\nSummary of numeric columns:\n"); print(summary(d04[, sapply(d04, is.numeric), drop = FALSE]))
}, file = "reports/d04_snapshot.txt")
message(" - Wrote reports/d04_snapshot.txt")

# Optional: save a lightweight copy to RDS for reproducibility of downstream steps
tryCatch({
  saveRDS(d04, file = "output/d04.rds")
  message(" - Saved output/d04.rds")
}, error = function(e) message(" - Skipped saving d04.rds: ", conditionMessage(e)))

message("Standalone import complete. You can continue with EDA and modeling steps using 'd04'.")

# =========================
# Helper functions
# =========================

check_and_fuzzy_path <- function(path) {
  if (file.exists(path)) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  repo_files <- list.files(".", recursive = TRUE, all.files = FALSE, full.names = FALSE)
  d <- utils::adist(path, repo_files)
  ord <- order(as.numeric(d), nchar(repo_files))
  suggestions <- unique(repo_files[ord])[1:min(5, length(repo_files))]
  stop(sprintf("Path not found: %s\nClosest matches:\n- %s", path, paste0(suggestions, collapse = "\n- ")), call. = FALSE)
}

resolve_col <- function(df, target, aliases = NULL) {
  nm <- names(df); low <- tolower(nm); cand <- unique(c(target, aliases)); cand <- cand[!is.na(cand)]
  for (c in cand) { hit <- which(low == tolower(c)); if (length(hit)) return(nm[hit[1]]) }
  for (c in cand) { hit <- which(grepl(tolower(c), low, fixed = TRUE)); if (length(hit)) return(nm[hit[1]]) }
  stop(sprintf("Could not resolve column for '%s'", target), call. = FALSE)
}

ensure_binary_event <- function(v) {
  if (is.logical(v)) return(as.integer(v))
  if (is.numeric(v)) return(as.integer(v > 0))
  if (is.factor(v) || is.character(v)) {
    lv <- tolower(as.character(v))
    yes <- c("1","y","yes","true","event","occurred","dead","progressed","failure")
    no  <- c("0","n","no","false","censored","alive","none","success")
    return(ifelse(lv %in% yes, 1L, ifelse(lv %in% no, 0L, NA_integer_)))
  }
  suppressWarnings(as.integer(as.numeric(v) > 0))
}

coerce_binary_factor <- function(x, ref) { x <- as.factor(x); if (ref %in% levels(x)) x <- stats::relevel(x, ref = ref); x }
coerce_predictor <- function(x) { xn <- suppressWarnings(as.numeric(as.character(x))); if (all(is.na(xn) == is.na(x))) return(xn); as.factor(x) }
fmt_ci <- function(ci_low, ci_high) { ci_low <- as.numeric(ci_low); ci_high <- as.numeric(ci_high); ifelse(is.na(ci_low)|is.na(ci_high), NA_character_, sprintf("[%.3f, %.3f]", ci_low, ci_high)) }

extract_logistic_effects <- function(model, term, contrast_label = NULL) {
  tt <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  tt <- dplyr::filter(tt, .data$term == !!term | stringr::str_starts(.data$term, paste0("^", term)))
  if (nrow(tt) == 0) return(tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
  if (!is.null(contrast_label)) { suffix <- sub(paste0("^", term), "", tt$term); hit <- which(suffix == contrast_label); tt <- tt[if (length(hit)) hit[1] else 1, , drop = FALSE] } else tt <- tt[1, , drop = FALSE]
  dplyr::transmute(tt, estimate = .data$estimate, conf.low = .data$conf.low, conf.high = .data$conf.high, p.value = .data$p.value)
}

extract_cox_effects <- function(model, term, contrast_label = NULL) {
  tt <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  tt <- dplyr::filter(tt, .data$term == !!term | stringr::str_starts(.data$term, paste0("^", term)))
  if (nrow(tt) == 0) return(tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
  if (!is.null(contrast_label)) { suffix <- sub(paste0("^", term), "", tt$term); hit <- which(suffix == contrast_label); tt <- tt[if (length(hit)) hit[1] else 1, , drop = FALSE] } else tt <- tt[1, , drop = FALSE]
  dplyr::transmute(tt, estimate = .data$estimate, conf.low = .data$conf.low, conf.high = .data$conf.high, p.value = .data$p.value)
}

extract_linear_effects <- function(model, term, contrast_label = NULL) {
  tt <- broom::tidy(model, conf.int = TRUE)
  tt <- dplyr::filter(tt, .data$term == !!term | stringr::str_starts(.data$term, paste0("^", term)))
  if (nrow(tt) == 0) return(tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
  if (!is.null(contrast_label)) { suffix <- sub(paste0("^", term), "", tt$term); hit <- which(suffix == contrast_label); tt <- tt[if (length(hit)) hit[1] else 1, , drop = FALSE] } else tt <- tt[1, , drop = FALSE]
  dplyr::transmute(tt, estimate = .data$estimate, conf.low = .data$conf.low, conf.high = .data$conf.high, p.value = .data$p.value)
}

emm_stage_effects <- function(model, data, stage_var, x_var, model_type = c("logistic", "cox", "linear"), contrast_label = NULL) {
  model_type <- match.arg(model_type); x_is_factor <- is.factor(data[[x_var]])
  if (!requireNamespace("emmeans", quietly = TRUE)) return(tibble::tibble(!!stage_var := factor(NA), estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
  if (!x_is_factor) {
    tr <- try(emmeans::emtrends(model, specs = stats::as.formula(paste("~", stage_var)), var = x_var), silent = TRUE)
    if (inherits(tr, "try-error")) return(tibble::tibble(!!stage_var := factor(NA), estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
    td <- try(broom::tidy(tr, conf.int = TRUE), silent = TRUE)
    if (inherits(td, "try-error")) { s <- as.data.frame(summary(tr, infer = c(TRUE, TRUE))); td <- tibble::tibble(!!stage_var := s[[stage_var]], estimate = s$trend, conf.low = s$lower.CL, conf.high = s$upper.CL, p.value = s$`p.value`) }
    if (!("estimate" %in% names(td))) { alt <- intersect(c("estimate","emmean","emtrend","trend","x.trend"), names(td)); if (length(alt) >= 1) td <- dplyr::rename(td, estimate = !!rlang::sym(alt[1])) }
    if (!("conf.low" %in% names(td)) && ("lower.CL" %in% names(td))) td <- dplyr::rename(td, conf.low = .data$lower.CL)
    if (!("conf.high" %in% names(td)) && ("upper.CL" %in% names(td))) td <- dplyr::rename(td, conf.high = .data$upper.CL)
    if (!(stage_var %in% names(td))) names(td)[1] <- stage_var
    if (model_type %in% c("logistic","cox")) td <- dplyr::mutate(td, estimate = exp(.data$estimate), conf.low = exp(.data$conf.low), conf.high = exp(.data$conf.high))
    return(td |> dplyr::select(!!stage_var, estimate, conf.low, conf.high, p.value))
  } else {
    em <- try(emmeans::emmeans(model, specs = stats::as.formula(paste("~", x_var, "|", stage_var))), silent = TRUE)
    if (inherits(em, "try-error")) return(tibble::tibble(!!stage_var := factor(NA), estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
    cn <- try(emmeans::contrast(em, method = "trt.vs.ctrl", ref = 1), silent = TRUE)
    if (inherits(cn, "try-error")) return(tibble::tibble(!!stage_var := factor(NA), estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_))
    td <- broom::tidy(cn, conf.int = TRUE)
    if (!is.null(contrast_label)) td <- dplyr::filter(td, stringr::str_detect(.data$contrast, paste0("^", contrast_label, "\\b|\\b", contrast_label, "\\b")))
    if (stage_var %in% names(td)) td <- td |> dplyr::group_by(.data[[stage_var]]) |> dplyr::slice_head(n = 1) |> dplyr::ungroup()
    if (model_type %in% c("logistic","cox")) td <- dplyr::mutate(td, estimate = exp(.data$estimate), conf.low = exp(.data$conf.low), conf.high = exp(.data$conf.high))
    if (!(stage_var %in% names(td))) { by_cols <- setdiff(names(td), c("contrast","estimate","std.error","statistic","df","p.value","conf.low","conf.high")); if (length(by_cols) >= 1) names(td)[names(td) == by_cols[1]] <- stage_var }
    return(td |> dplyr::select(!!stage_var, estimate, conf.low, conf.high, p.value))
  }
}

plot_stage_trends <- function(df, y, x, stage_col = "stage_early", y_positive_only = FALSE, out_dir = "output/figures") {
  stopifnot(y %in% names(df), x %in% names(df), stage_col %in% names(df))
  d <- df |>
    dplyr::select(y = .data[[y]], x = .data[[x]], stage = .data[[stage_col]]) |>
    dplyr::mutate(stage = as.factor(stage)) |>
    (\(d) if (y_positive_only) dplyr::filter(d, !is.na(y), y > 0, !is.na(x), !is.na(stage)) else dplyr::filter(d, !is.na(y), !is.na(x), !is.na(stage)))()
  if (nrow(d) < 5 || !is.numeric(d$x) || !is.numeric(d$y)) return(invisible(NULL))
  m <- stats::lm(y ~ stage * x, data = d)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y, color = stage)) +
    ggplot2::geom_point(alpha = 0.6) + ggplot2::geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
    ggplot2::theme_minimal(base_size = 12) + ggplot2::labs(x = x, y = y, color = "stage")
  out <- file.path(out_dir, paste0("eda_trend_", y, "_by_", x, ".tiff"))
  ggplot2::ggsave(out, p, width = 8, height = 6, dpi = 300, device = "tiff", compression = "lzw")
}

# =========================
# Data prep and columns
# =========================

df <- d04 |> janitor::clean_names()
col_time_ttnt <- resolve_col(df, "time_to_next_treatmen")
col_event_ttnt <- resolve_col(df, "ttnt_achieved")
col_treat_duration <- resolve_col(df, "treatment_duration")
col_event_discont <- resolve_col(df, "discontinuation_reason")
col_response_duration <- resolve_col(df, "response_duration")
col_event_progression <- resolve_col(df, "progression")
col_response_achieved <- resolve_col(df, "response_achieved")
col_response_time_to <- resolve_col(df, "response_time_to")
col_stage <- resolve_col(df, "stage_early")
if (is.numeric(df[[col_stage]]) || is.logical(df[[col_stage]])) df[[col_stage]] <- factor(df[[col_stage]])
if (is.factor(df[[col_stage]]) && "0" %in% levels(df[[col_stage]])) df[[col_stage]] <- coerce_binary_factor(df[[col_stage]], ref = "0")
df[[col_stage]] <- factor(df[[col_stage]])
if (!is.null(col_event_discont) && col_event_discont %in% names(df)) { dr_num <- suppressWarnings(readr::parse_number(as.character(df[[col_event_discont]]))); df[[col_event_discont]] <- dplyr::if_else(dr_num == 0, 0L, 1L, missing = NA_integer_) }
km_specs <- list(
  list(time = col_time_ttnt, event = col_event_ttnt, label = "time_to_next_treatment"),
  list(time = col_treat_duration, event = col_event_discont, label = "treatment_duration"),
  list(time = col_response_duration, event = col_event_progression, label = "response_duration")
)

# =========================
# 05) EDA
# =========================

message("Standalone: Running EDA ...")
capture.output({ cat("\nData snapshot (str):\n"); str(df); cat("\nSkim summary:\n"); print(skimr::skim(df)) }, file = "reports/eda_console_log.txt")
num_cols <- df |> dplyr::select(where(is.numeric)) |> names()
if (length(num_cols) > 0) {
  p_num <- df |> dplyr::select(dplyr::all_of(num_cols)) |> tidyr::pivot_longer(cols = everything(), names_to = "var", values_to = "val") |> tidyr::drop_na(val) |>
    ggplot(aes(x = val)) + geom_histogram(bins = 30, fill = "steelblue", color = "white") + facet_wrap(~ var, scales = "free", ncol = 3) + labs(title = "Numeric distributions") + theme_minimal(base_size = 12)
  ggsave("output/figures/eda_numeric_distributions.tiff", p_num, width = 10, height = 7, dpi = 300, device = "tiff", compression = "lzw")
}
cat_cols <- df |> dplyr::select(where(~ is.factor(.x) || is.character(.x))) |> names()
if (length(cat_cols) > 0) {
  cat_long <- df |> dplyr::select(dplyr::all_of(cat_cols)) |> dplyr::mutate(dplyr::across(everything(), as.factor)) |> tidyr::pivot_longer(cols = everything(), names_to = "var", values_to = "val") |> tidyr::drop_na(val)
  p_cat <- cat_long |> ggplot(aes(x = val, fill = val)) + geom_bar(position = "fill") + facet_wrap(~ var, scales = "free", ncol = 3) + theme_minimal(base_size = 12) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Categorical distributions (proportions)", x = NULL, y = "Proportion", fill = "Level")
  ggsave("output/figures/eda_categorical_distributions.tiff", p_cat, width = 12, height = 8, dpi = 300, device = "tiff", compression = "lzw")
}
for (sp in km_specs) {
  time_col <- sp$time; event_col <- sp$event; df_km <- df |> dplyr::select(time = dplyr::all_of(time_col), event = dplyr::all_of(event_col), stage = dplyr::all_of(col_stage)) |>
    dplyr::mutate(time = suppressWarnings(readr::parse_number(as.character(.data$time))), event = ensure_binary_event(.data$event), stage = as.factor(.data$stage)) |>
    (\(d) if (identical(sp$label, "treatment_duration")) dplyr::mutate(d, event = dplyr::if_else(d$event == 0, 0L, 1L)) else d)() |>
    dplyr::filter(!is.na(time), !is.na(event), !is.na(stage))
  if (nrow(df_km) > 0) {
    f <- survival::survfit(survival::Surv(time, event) ~ stage, data = df_km)
    p <- ggsurvplot(f, data = df_km, risk.table = TRUE, conf.int = TRUE, ggtheme = theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold")), legend.title = "stage_early", title = paste0("KM: ", sp$label))
    ggplot2::ggsave(file.path("output/figures", paste0("eda_km_", sp$label, ".tiff")), p$plot, width = 8, height = 6, dpi = 300, device = "tiff", compression = "lzw")
  }
}
if (length(num_cols) >= 2 && requireNamespace("corrplot", quietly = TRUE)) {
  mat <- df |> dplyr::select(dplyr::all_of(num_cols)) |> as.matrix(); if (requireNamespace("Hmisc", quietly = TRUE)) { rc <- Hmisc::rcorr(mat, type = "spearman"); r <- rc$r; p <- rc$P } else { r <- suppressWarnings(cor(mat, method = "spearman", use = "pairwise.complete.obs")); p <- matrix(NA_real_, nrow = ncol(mat), ncol = ncol(mat), dimnames = dimnames(r)) }
  rio::export(list(Spearman_R = as.data.frame(r), P_values = as.data.frame(p)), "output/tables/eda_corr_spearman_v2.xlsx")
  pal <- grDevices::colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))
  grDevices::tiff("output/figures/eda_corrplot_spearman_v2.tiff", width = 2400, height = 2000, res = 300, compression = "lzw"); corrplot::corrplot(r, method = "color", type = "lower", tl.col = "black", tl.srt = 45, p.mat = p, sig.level = 0.05, insig = "blank", col = pal(200), addCoef.col = "black", number.cex = 0.5); grDevices::dev.off()
}
fac_cols <- df |> dplyr::select(where(~ is.factor(.x) || is.character(.x))) |> names(); fac_cols <- setdiff(fac_cols, col_stage); res <- list()
for (v in fac_cols) { dsub <- df |> dplyr::select(var = .data[[v]], stage = .data[[col_stage]]) |> dplyr::mutate(var = as.factor(var), stage = as.factor(stage)) |> tidyr::drop_na(); if (nrow(dsub) == 0 || nlevels(dsub$var) < 2 || nlevels(dsub$stage) < 2) next; tbl <- table(dsub$var, dsub$stage); chi <- try(chisq.test(tbl, correct = FALSE), silent = TRUE); if (!inherits(chi, "try-error") && all(chi$expected >= 5, na.rm = TRUE)) res[[length(res) + 1]] <- tibble::tibble(variable = v, method = "chisq", p.value = chi$p.value, n_levels = nlevels(dsub$var)) else { fe <- try(fisher.test(tbl), silent = TRUE); if (!inherits(fe, "try-error")) res[[length(res) + 1]] <- tibble::tibble(variable = v, method = "fisher", p.value = fe$p.value, n_levels = nlevels(dsub$var)) } }
if (length(res)) rio::export(dplyr::bind_rows(res), "output/tables/eda_factor_vs_stage_tests_v2.xlsx")
if (requireNamespace("skimr", quietly = TRUE)) { sk <- skimr::skim(df); try(rio::export(as.data.frame(sk), "output/tables/eda/skimr_summary_v2.xlsx"), silent = TRUE) }

# Discontinuation (binary) by stage_early: bar plot (proportions)
if (!is.null(col_event_discont) && col_event_discont %in% names(df) && col_stage %in% names(df)) {
  d_disc <- df |>
    dplyr::select(event = .data[[col_event_discont]], stage = .data[[col_stage]]) |>
    dplyr::mutate(event = as.factor(event), stage = as.factor(stage)) |>
    tidyr::drop_na()
  if (nrow(d_disc) > 0) {
    p_disc <- ggplot(d_disc, aes(x = stage, fill = event)) +
      geom_bar(position = "fill", color = "white") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal(base_size = 12) +
      labs(title = "Discontinuation (0/1) by stage_early", x = "stage_early", y = "Proportion", fill = "event")
    ggplot2::ggsave("output/figures/eda/eda_bar_discontinuation_by_stage_v2.tiff", p_disc, width = 8, height = 6, dpi = 300, device = "tiff", compression = "lzw")
  }
}

# =========================
# 06) Main effects and v2 medians
# =========================

message("Standalone: Fitting main models ...")
# Logistic
df_log <- df |> dplyr::select(y = dplyr::all_of(col_response_achieved), stage = dplyr::all_of(col_stage)) |> dplyr::filter(!is.na(y), !is.na(stage)) |> dplyr::mutate(y = as.factor(y))
mod_log <- tryCatch(stats::glm(y ~ stage, data = df_log, family = stats::binomial()), error = function(e) NULL)
log_all <- if (!is.null(mod_log)) extract_logistic_effects(mod_log, term = "stage") else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
# Cox
cox_main <- list()
for (spec in km_specs) {
  time_col <- spec$time; event_col <- spec$event; outname <- spec$label
  d_surv <- df |> dplyr::select(time = dplyr::all_of(time_col), event = dplyr::all_of(event_col), stage = dplyr::all_of(col_stage)) |>
    dplyr::mutate(time = suppressWarnings(readr::parse_number(as.character(.data$time))), event = ensure_binary_event(.data$event), stage = as.factor(.data$stage)) |>
    (\(d) if (identical(outname, "treatment_duration")) dplyr::mutate(d, event = dplyr::if_else(d$event == 0, 0L, 1L)) else d)() |>
    dplyr::filter(!is.na(time), !is.na(event), !is.na(stage))
  if (nrow(d_surv) > 0) {
    m <- survival::coxph(survival::Surv(time, event) ~ stage, data = d_surv, x = TRUE)
    eff <- extract_cox_effects(m, term = "stage")
  } else {
    eff <- tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
  }
  cox_main[[outname]] <- dplyr::bind_cols(tibble::tibble(outcome = outname), eff)
}
# Linear
df_lin <- df |> dplyr::select(y = dplyr::all_of(col_response_time_to), stage = dplyr::all_of(col_stage)) |> dplyr::filter(!is.na(y), y > 0, !is.na(stage))
mod_lin <- tryCatch(stats::lm(y ~ stage, data = df_lin), error = function(e) NULL)
lin_all <- if (!is.null(mod_lin)) extract_linear_effects(mod_lin, term = "stage") else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)

main_rows <- list(
  tibble::tibble(outcome = "response_achieved", p_all = log_all$p.value, effect_all = log_all$estimate, effect_all_ci = fmt_ci(log_all$conf.low, log_all$conf.high), p_stage0 = NA_real_, effect_stage0 = NA_real_, effect_stage0_ci = NA_character_, p_stage1 = NA_real_, effect_stage1 = NA_real_, effect_stage1_ci = NA_character_),
  dplyr::bind_rows(cox_main) |> dplyr::transmute(outcome = .data$outcome, p_all = .data$p.value, effect_all = .data$estimate, effect_all_ci = fmt_ci(.data$conf.low, .data$conf.high), p_stage0 = NA_real_, effect_stage0 = NA_real_, effect_stage0_ci = NA_character_, p_stage1 = NA_real_, effect_stage1 = NA_real_, effect_stage1_ci = NA_character_),
  tibble::tibble(outcome = "response_time_to", p_all = lin_all$p.value, effect_all = lin_all$estimate, effect_all_ci = fmt_ci(lin_all$conf.low, lin_all$conf.high), p_stage0 = NA_real_, effect_stage0 = NA_real_, effect_stage0_ci = NA_character_, p_stage1 = NA_real_, effect_stage1 = NA_real_, effect_stage1_ci = NA_character_)
)
summary_main_models <- dplyr::bind_rows(main_rows)

compute_km_medians_by_stage <- function(df, time_col, event_col, stage_col, label) {
  d <- df |> dplyr::select(time = dplyr::all_of(time_col), event = dplyr::all_of(event_col), stage = dplyr::all_of(stage_col)) |>
    dplyr::mutate(time = suppressWarnings(readr::parse_number(as.character(.data$time))), event = ensure_binary_event(.data$event), stage = as.factor(.data$stage)) |>
    (\(d) if (identical(label, "treatment_duration")) dplyr::mutate(d, event = dplyr::if_else(d$event == 0, 0L, 1L)) else d)() |>
    dplyr::filter(!is.na(time), !is.na(event), !is.na(stage))
  if (nrow(d) == 0) return(tibble::tibble(outcome = label, stage = factor(NA), median = NA_real_, lcl = NA_real_, ucl = NA_real_))
  fit <- survival::survfit(survival::Surv(time, event) ~ stage, data = d)
  td <- try(survminer::surv_median(fit), silent = TRUE)
  if (!inherits(td, "try-error") && all(c("median","lower","upper") %in% names(td))) {
    tibble::tibble(outcome = label, stage = td$strata |> gsub("^stage=", "", x = _), median = td$median, lcl = td$lower, ucl = td$upper)
  } else {
    st <- as.data.frame(summary(fit)$table); rn <- rownames(st); stage_vals <- gsub("^stage=", "", rn)
    med <- if ("median" %in% names(st)) st$median else rep(NA_real_, nrow(st))
    lcl <- if ("0.95LCL" %in% names(st)) st[["0.95LCL"]] else rep(NA_real_, nrow(st))
    ucl <- if ("0.95UCL" %in% names(st)) st[["0.95UCL"]] else rep(NA_real_, nrow(st))
    tibble::tibble(outcome = label, stage = stage_vals, median = med, lcl = lcl, ucl = ucl)
  }
}
km_labels <- vapply(km_specs, function(x) x$label, character(1))
km_meds <- purrr::map_dfr(km_specs, function(sp) compute_km_medians_by_stage(df, sp$time, sp$event, col_stage, sp$label))
km_meds_wide <- km_meds |> dplyr::filter(!is.na(stage)) |> dplyr::mutate(stage = as.character(stage)) |> tidyr::pivot_wider(id_cols = outcome, names_from = stage, values_from = c(median, lcl, ucl), names_sep = "_")
add_ci_fmt <- function(low, high) ifelse(is.na(low) | is.na(high), NA_character_, sprintf("[%.2f, %.2f]", low, high))
km_meds_fmt <- km_meds_wide |> dplyr::mutate(median_stage0 = .data[["median_0"]], median_stage0_ci = add_ci_fmt(.data[["lcl_0"]], .data[["ucl_0"]]), median_stage1 = .data[["median_1"]], median_stage1_ci = add_ci_fmt(.data[["lcl_1"]], .data[["ucl_1"]])) |> dplyr::select(outcome, median_stage0, median_stage0_ci, median_stage1, median_stage1_ci)
summary_main_models_v2 <- summary_main_models |> dplyr::left_join(km_meds_fmt, by = "outcome") |> dplyr::mutate(effect_stage0 = dplyr::if_else(.data$outcome %in% km_labels, as.numeric(.data$median_stage0), .data$effect_stage0), effect_stage0_ci = ifelse(.data$outcome %in% km_labels, .data$median_stage0_ci, .data$effect_stage0_ci), effect_stage1 = dplyr::if_else(.data$outcome %in% km_labels, as.numeric(.data$median_stage1), .data$effect_stage1), effect_stage1_ci = ifelse(.data$outcome %in% km_labels, .data$median_stage1_ci, .data$effect_stage1_ci))
rio::export(summary_main_models_v2, "output/tables/main/summary_main_models_v2.xlsx")

# =========================
# 07) Adjusted models and plots
# =========================

message("Standalone: Fitting adjusted models ...")
exclude <- unique(c(col_stage, col_time_ttnt, col_event_ttnt, col_treat_duration, col_event_discont, col_response_duration, col_event_progression, col_response_achieved, col_response_time_to))
covars <- setdiff(names(df), exclude); covars <- covars[!vapply(df[covars], function(x) is.list(x) || is.data.frame(x), logical(1))]
adjusted_rows <- list()
for (cv in covars) {
  d_mod <- df |> dplyr::select(y = dplyr::all_of(col_response_achieved), stage = dplyr::all_of(col_stage), x = dplyr::all_of(cv)) |> dplyr::mutate(y = as.factor(y), x = coerce_predictor(x)) |> dplyr::filter(!is.na(y), !is.na(stage), !is.na(x))
  m_all <- tryCatch(stats::glm(y ~ stage + x, data = d_mod, family = stats::binomial()), error = function(e) NULL)
  m_int <- tryCatch(stats::glm(y ~ stage * x, data = d_mod, family = stats::binomial()), error = function(e) NULL)
  contrast_label <- if (is.factor(d_mod$x)) levels(d_mod$x)[2] else NULL
  eff_all <- if (!is.null(m_all)) extract_logistic_effects(m_all, term = "x", contrast_label = contrast_label) else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
  if (!is.null(m_int)) sp_log_emm <- emm_stage_effects(m_int, d_mod, stage_var = "stage", x_var = "x", model_type = "logistic", contrast_label = contrast_label) else sp_log_emm <- tibble::tibble(stage = factor(NA), estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
  st_levels <- levels(as.factor(d_mod$stage)); s0 <- sp_log_emm |> dplyr::filter(as.character(.data$stage) == st_levels[1]) |> dplyr::slice_head(n = 1); s1 <- sp_log_emm |> dplyr::filter(length(st_levels) >= 2, as.character(.data$stage) == st_levels[2]) |> dplyr::slice_head(n = 1)
  if (nrow(s0) == 0) s0 <- tibble::tibble(p.value = NA_real_, estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_)
  if (nrow(s1) == 0) s1 <- tibble::tibble(p.value = NA_real_, estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_)
  adjusted_rows[[length(adjusted_rows) + 1]] <- tibble::tibble(outcome = "response_achieved", covariate = cv, p_all = eff_all$p.value, effect_all = eff_all$estimate, effect_all_ci = fmt_ci(eff_all$conf.low, eff_all$conf.high), p_stage0 = s0$p.value, effect_stage0 = s0$estimate, effect_stage0_ci = fmt_ci(s0$conf.low, s0$conf.high), p_stage1 = s1$p.value, effect_stage1 = s1$estimate, effect_stage1_ci = fmt_ci(s1$conf.low, s1$conf.high))
  for (spec in km_specs) {
    time_col <- spec$time; event_col <- spec$event; outname <- spec$label
    d_surv <- df |> dplyr::select(time = dplyr::all_of(time_col), event = dplyr::all_of(event_col), stage = dplyr::all_of(col_stage), x = dplyr::all_of(cv)) |>
      dplyr::mutate(time = suppressWarnings(readr::parse_number(as.character(.data$time))), event = ensure_binary_event(.data$event), stage = as.factor(.data$stage), x = coerce_predictor(x)) |>
      (\(d) if (identical(outname, "treatment_duration")) dplyr::mutate(d, event = dplyr::if_else(d$event == 0, 0L, 1L)) else d)() |>
      dplyr::filter(!is.na(time), !is.na(event), !is.na(stage), !is.na(x))
    m_all <- tryCatch(survival::coxph(survival::Surv(time, event) ~ stage + x, data = d_surv, x = TRUE), error = function(e) NULL)
    m_int <- tryCatch(survival::coxph(survival::Surv(time, event) ~ stage * x, data = d_surv, x = TRUE), error = function(e) NULL)
    contrast_label <- if (is.factor(d_surv$x)) levels(d_surv$x)[2] else NULL
    eff_all <- if (!is.null(m_all)) extract_cox_effects(m_all, term = "x", contrast_label = contrast_label) else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
    if (!is.null(m_int)) sp_cox_emm <- emm_stage_effects(m_int, d_surv, stage_var = "stage", x_var = "x", model_type = "cox", contrast_label = contrast_label) else sp_cox_emm <- tibble::tibble(stage = factor(NA), estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
    st_levels <- levels(as.factor(d_surv$stage)); s0 <- sp_cox_emm |> dplyr::filter(as.character(.data$stage) == st_levels[1]) |> dplyr::slice_head(n = 1); s1 <- sp_cox_emm |> dplyr::filter(length(st_levels) >= 2, as.character(.data$stage) == st_levels[2]) |> dplyr::slice_head(n = 1)
    if (nrow(s0) == 0) s0 <- tibble::tibble(p.value = NA_real_, estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_)
    if (nrow(s1) == 0) s1 <- tibble::tibble(p.value = NA_real_, estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_)
    adjusted_rows[[length(adjusted_rows) + 1]] <- tibble::tibble(outcome = outname, covariate = cv, p_all = eff_all$p.value, effect_all = eff_all$estimate, effect_all_ci = fmt_ci(eff_all$conf.low, eff_all$conf.high), p_stage0 = s0$p.value, effect_stage0 = s0$estimate, effect_stage0_ci = fmt_ci(s0$conf.low, s0$conf.high), p_stage1 = s1$p.value, effect_stage1 = s1$estimate, effect_stage1_ci = fmt_ci(s1$conf.low, s1$conf.high))
  }
  d_lin <- df |> dplyr::select(y = dplyr::all_of(col_response_time_to), stage = dplyr::all_of(col_stage), x = dplyr::all_of(cv)) |> dplyr::mutate(x = coerce_predictor(x)) |> dplyr::filter(!is.na(y), y > 0, !is.na(stage), !is.na(x))
  m_all <- tryCatch(stats::lm(y ~ stage + x, data = d_lin), error = function(e) NULL)
  m_int <- tryCatch(stats::lm(y ~ stage * x, data = d_lin), error = function(e) NULL)
  contrast_label <- if (is.factor(d_lin$x)) levels(d_lin$x)[2] else NULL
  eff_all <- if (!is.null(m_all)) extract_linear_effects(m_all, term = "x", contrast_label = contrast_label) else tibble::tibble(estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
  if (!is.null(m_int)) sp_lin_emm <- emm_stage_effects(m_int, d_lin, stage_var = "stage", x_var = "x", model_type = "linear", contrast_label = contrast_label) else sp_lin_emm <- tibble::tibble(stage = factor(NA), estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_, p.value = NA_real_)
  st_levels <- levels(as.factor(d_lin$stage)); s0 <- sp_lin_emm |> dplyr::filter(as.character(.data$stage) == st_levels[1]) |> dplyr::slice_head(n = 1); s1 <- sp_lin_emm |> dplyr::filter(length(st_levels) >= 2, as.character(.data$stage) == st_levels[2]) |> dplyr::slice_head(n = 1)
  if (nrow(s0) == 0) s0 <- tibble::tibble(p.value = NA_real_, estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_)
  if (nrow(s1) == 0) s1 <- tibble::tibble(p.value = NA_real_, estimate = NA_real_, conf.low = NA_real_, conf.high = NA_real_)
  adjusted_rows[[length(adjusted_rows) + 1]] <- tibble::tibble(outcome = "response_time_to", covariate = cv, p_all = eff_all$p.value, effect_all = eff_all$estimate, effect_all_ci = fmt_ci(eff_all$conf.low, eff_all$conf.high), p_stage0 = s0$p.value, effect_stage0 = s0$estimate, effect_stage0_ci = fmt_ci(s0$conf.low, s0$conf.high), p_stage1 = s1$p.value, effect_stage1 = s1$estimate, effect_stage1_ci = fmt_ci(s1$conf.low, s1$conf.high))
  if (is.numeric(d_lin$x)) { try(plot_stage_trends(df, y = col_response_time_to, x = cv, stage_col = col_stage, y_positive_only = TRUE), silent = TRUE) }
}
summary_adjusted_models <- dplyr::bind_rows(adjusted_rows)
rio::export(summary_adjusted_models, "output/tables/adjusted/summary_adjusted_models.xlsx")

# v2 multiplots
parse_ci_safe <- function(ci) { if (is.na(ci) || is.null(ci)) return(c(NA_real_, NA_real_)); s <- gsub("[^0-9.,-]", "", as.character(ci)); parts <- strsplit(s, ",")[[1]]; parts <- trimws(parts); if (length(parts) != 2) return(c(NA_real_, NA_real_)); as.numeric(c(parts[1], parts[2])) }
make_point_plot <- function(df_plot, xlab = NULL, title = NULL) { ggplot(df_plot, aes(x = label, y = effect, ymin = low, ymax = high)) + geom_pointrange(color = "#2c3e50") + coord_flip() + theme_minimal(base_size = 12) + labs(x = xlab, y = "Effect (estimate)", title = title) }
save_main_plot <- function(row) { out <- paste0("output/figures/main/mainplot_", row$outcome, "_v2.tiff") |> gsub("[[:space:]]+", "_", x = _); if (row$outcome %in% km_labels) { sp <- km_specs[[which(km_labels == row$outcome)[1]]]; d <- df |> dplyr::select(time = dplyr::all_of(sp$time), event = dplyr::all_of(sp$event), stage = dplyr::all_of(col_stage)) |> dplyr::mutate(time = suppressWarnings(readr::parse_number(as.character(.data$time))), event = ensure_binary_event(.data$event), stage = as.factor(.data$stage)) |>
  (\(d) if (identical(sp$label, "treatment_duration")) dplyr::mutate(d, event = dplyr::if_else(d$event == 0, 0L, 1L)) else d)() |> dplyr::filter(!is.na(time), !is.na(event)); if (nrow(d) < 5) return(invisible(NULL)); f_all <- survival::survfit(survival::Surv(time, event) ~ 1, data = d); p_left <- ggsurvplot(f_all, data = d, conf.int = TRUE, risk.table = FALSE, ggtheme = theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold")), title = paste0("KM (All): ", sp$label))$plot; f_by <- survival::survfit(survival::Surv(time, event) ~ stage, data = d); p_right <- ggsurvplot(f_by, data = d, conf.int = TRUE, risk.table = FALSE, ggtheme = theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold")), legend.title = "stage_early", title = paste0("KM by stage_early: ", sp$label))$plot; grDevices::tiff(out, width = 2200, height = 1000, res = 300, compression = "lzw"); if ("cowplot" %in% .packages()) print(cowplot::plot_grid(p_left, p_right, ncol = 2)) else { grid::grid.newpage(); pushViewport(grid::viewport(layout = grid::grid.layout(1, 2))); print(p_left, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1)); print(p_right, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2)) }; grDevices::dev.off() } else { lowhigh <- parse_ci_safe(row$effect_all_ci); df_left <- tibble::tibble(label = "All", effect = as.numeric(row$effect_all), low = lowhigh[1], high = lowhigh[2]); df_right <- tibble::tibble(label = c("Stage 0","Stage 1"), effect = c(as.numeric(row$effect_stage0), as.numeric(row$effect_stage1)), low = sapply(c(row$effect_stage0_ci, row$effect_stage1_ci), function(z) parse_ci_safe(z)[1]), high = sapply(c(row$effect_stage0_ci, row$effect_stage1_ci), function(z) parse_ci_safe(z)[2])) |> dplyr::filter(!is.na(effect)); p_left <- make_point_plot(df_left, xlab = NULL, title = paste0(row$outcome, " (All)")); p_right <- make_point_plot(df_right, xlab = NULL, title = paste0(row$outcome, " (by stage_early)")); grDevices::tiff(out, width = 2000, height = 1000, res = 300, compression = "lzw"); if ("cowplot" %in% .packages()) print(cowplot::plot_grid(p_left, p_right, ncol = 2)) else { grid::grid.newpage(); pushViewport(grid::viewport(layout = grid::grid.layout(1, 2))); print(p_left, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1)); print(p_right, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2)) }; grDevices::dev.off() } }
invisible(purrr::pwalk(summary_main_models_v2, save_main_plot))
save_adjusted_plot <- function(row) { out <- paste0("output/figures/adjusted/adjplot_", row$outcome, "__", row$covariate, "_v2.tiff") |> gsub("[^A-Za-z0-9_.-]+", "_", x = _); lowhigh <- parse_ci_safe(row$effect_all_ci); df_left <- tibble::tibble(label = "All", effect = as.numeric(row$effect_all), low = lowhigh[1], high = lowhigh[2]); df_right <- tibble::tibble(label = c("Stage 0","Stage 1"), effect = c(as.numeric(row$effect_stage0), as.numeric(row$effect_stage1)), low = sapply(c(row$effect_stage0_ci, row$effect_stage1_ci), function(z) parse_ci_safe(z)[1]), high = sapply(c(row$effect_stage0_ci, row$effect_stage1_ci), function(z) parse_ci_safe(z)[2])) |> dplyr::filter(!is.na(effect)); p_left <- make_point_plot(df_left, xlab = NULL, title = paste0(row$outcome, " ~ ", row$covariate, " (All)")); p_right <- make_point_plot(df_right, xlab = NULL, title = paste0(row$outcome, " ~ ", row$covariate, " (by stage_early)")); grDevices::tiff(out, width = 2000, height = 1000, res = 300, compression = "lzw"); if ("cowplot" %in% .packages()) print(cowplot::plot_grid(p_left, p_right, ncol = 2)) else { grid::grid.newpage(); pushViewport(grid::viewport(layout = grid::grid.layout(1, 2))); print(p_left, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1)); print(p_right, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2)) }; grDevices::dev.off() }
if (exists("summary_adjusted_models")) invisible(purrr::pwalk(summary_adjusted_models, save_adjusted_plot))

message("Standalone analysis complete. Key outputs:\n - output/tables/main/summary_main_models_v2.xlsx\n - output/tables/adjusted/summary_adjusted_models.xlsx\n - output/tables/eda/*_v2.xlsx\n - output/figures/main/*.tiff\n - output/figures/adjusted/*.tiff\n - output/figures/eda/*.tiff")
