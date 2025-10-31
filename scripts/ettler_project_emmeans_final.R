
## 04a) Alias map for key variables ----
# This list maps preferred names to alternative names (aliases)
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
