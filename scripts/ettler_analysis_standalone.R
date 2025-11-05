#!/usr/bin/env Rscript

# Ettler project: standalone analysis entry
# - Does NOT reference any ettler_analysis_autogen*.R scripts
# - Imports dataset object d04 by sourcing OBJ_01.R as primary path
# - Falls back to loading reports/markD_03.RData if sourcing does not produce d04
# - Emits simple diagnostics so the rest of analyses can build on top

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(rio)
})

set.seed(123)

# Output dirs
dir.create("output", showWarnings = FALSE, recursive = TRUE)
dir.create("reports", showWarnings = FALSE, recursive = TRUE)

message("Starting standalone import for d04 ...")

# Utility: try a set of candidate paths and source the first that exists
try_source_first <- function(paths) {
  for (p in paths) {
    if (file.exists(p)) {
      message(sprintf(" - Sourcing: %s", p))
      tryCatch({
        source(p, chdir = TRUE)
        return(TRUE)
      }, error = function(e) {
        message(sprintf("   ! Source failed at %s: %s", p, conditionMessage(e)))
        return(FALSE)
      })
    }
  }
  FALSE
}

# Primary: source OBJ_01.R (expected to create d04)
obj_candidates <- c(
  "scripts/functions/OBJ_01.R",
  "functions/OBJ_01.R",
  "scripts/OBJ_01.R",
  "OBJ_01.R"
)

ok_src <- try_source_first(obj_candidates)

if (!exists("d04", inherits = TRUE) || !is.data.frame(get("d04", inherits = TRUE))) {
  message("d04 not found after sourcing OBJ_01.R; trying fallback RData import ...")
  rdata_path <- "reports/markD_03.RData"
  if (!file.exists(rdata_path)) {
    # Try parent directory in case of chdir
    up <- file.path("..", rdata_path)
    if (file.exists(up)) rdata_path <- up
  }
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

