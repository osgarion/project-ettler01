# Directories ----
dir.create("data/raw", recursive = TRUE)
dir.create("data/processed", recursive = TRUE)
dir.create("scripts/functions", recursive = TRUE)
dir.create("output/figures", recursive = TRUE)
dir.create("output/tables", recursive = TRUE)
dir.create("misc")
dir.create("reports")

# Libraries & functions ----
# required
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, purrr,conflicted)

# Functions and libraries uploading
list.files("scripts/functions/", pattern="*.*", full.names=TRUE) %>% map(~source(.))

# Back-up
back_up("scripts/functions/FUN_01.R") # the the destination subdirectory specify using 'path_dest'
back_up("scripts/functions/OBJ_01.R") # the the destination subdirectory specify using 'path_dest'
back_up("scripts/Script_project_ettler_02.R") # the the destination subdirectory specify using 'path_dest'

# Save to .RData
save(legend_03, d04, 
     res_surv_ttnt, 
     res_surv_durTRTeffect, 
     res_surv_response,
     file = "reports/markD_02.RData")
save.image("reports/markD_02.RData")
# Save the tables into data/tables.RData by listing them individually
cgwtools::resave(xx2,xx3, file = "reports/markD_02.RData") # resave a list of tables that I'll use in the .Rmd file.
# Save the tables into data/tables.RData using "patterns" 
cgwtools::resave(list=ls(pattern="tbl"), file = "reports/markD_02.RData")

# EDA ----
## Overview ----
d04 |> 
  select(-initials) |>  
  mutate(across(
    .cols = -c(bmi, age), 
    .fns = as.factor
  )) |> 
  my_skim()
## Barplot --
d04 |> 
  select(-initials, -age, -bmi, -contains("duration"), -ttnt,
         -contains("time_to")) |> 
  select(where(is.numeric)) |> 
  mutate(across(everything(),as.factor)) |> 
  pivot_longer(cols = everything()) |> 
  na.omit() |> 
  ggplot(aes(x = name, fill = as.factor(value))) +
  geom_bar(position = "fill") +  # Proportions
  labs(y = "Proportion", fill = "Outcome") +
  theme_sjplot2()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  paletteer::scale_fill_paletteer_d("waRhol::marilyn_orange_62") +
  NULL



# Statistics ----
## Correspondence analyses ----
# https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/
res_mca_02 <- d04 |> select(-initials, -age, -bmi) |> na.omit() |> 
  mutate(across(everything(),as.factor)) |> 
  MCA(graph = FALSE)
fviz_mca_var(res_mca_02, 
             select.var = list(contrib = 10),  # only top ten variables
             repel = TRUE, ggtheme = theme_minimal(),
             col.var = "contrib",
             # choice = "mca.cor",
             gradient.cols = c("#bdbdbd", "#ffeda0", "#FC4E07"))
fviz_screeplot(res_mca_02)
fviz_ellipses(res_mca_02, c("sex", "ae_grade_3_4"),
              geom = "point")
dimdesc(res_mca_02, axes = c(1,2))


# Statistics ----
## I. section ----
### Logistic model ----
res_logist_02 <- d04 |> 
  select(all_of(c(variab_indep_01, variab_dep_01))) |> 
  mutate(sex = if_else(sex == "M", 1, 0)) |> 
  pivot_longer(
    cols = all_of(variab_dep_01),
    names_to = "dep_name",
    values_to = "dep_value"
  ) |> 
  pivot_longer(cols = all_of(variab_indep_01),
               names_to = "indep_name",
               values_to = "indep_value") |> 
  group_by(dep_name,indep_name) |> 
  nest() |> 
  mutate(mod = map(data, ~glm(dep_value ~ indep_value, 
                              data = .x, family = "binomial")),
         tidier = map(mod, ~tidy(.x, conf.int = T, exp = T)))

### Table ----
res_logist_02_tab <- res_logist_02 |> 
  unnest(tidier) |> 
  filter(term == "indep_value") |> 
  select(-data, -mod, -term) |> 
  mutate(across(where(is.numeric), ~round(.x,3)))

# export(res_logist_02_tab, "output/tables/250428_partA_logistic_01.xlsx")

### kableExtra ----
res_logist_02_tab |> 
  select(-std.error, - statistic) |> 
  kable(col.names = c("Dependent", "Independent", "Odds Ratio", "p-value",
                      "5% CI", "95% CI")) |> 
  kable_styling(
    full_width = F,
    latex_options = c(
      "hold_position" # stop table floating
    ),
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  ) %>%
  collapse_rows(columns = 1, valign = "top")

## II. section ----
### Logistic regression ----
#### Without `Stage Early` ----
##### Model ----
res_logist_03 <- d04 |> 
  select(all_of(c(variab_indep_02, variab_dep_02a))) |> 
  mutate(sex = if_else(sex == "M", 1, 0)) |> 
  pivot_longer(
    cols = all_of(variab_dep_02a),
    names_to = "dep_name",
    values_to = "dep_value"
  ) |> 
  pivot_longer(cols = all_of(variab_indep_02),
               names_to = "indep_name",
               values_to = "indep_value") |> 
  group_by(dep_name,indep_name) |> 
  nest() |> 
  mutate(mod = map(data, ~glm(dep_value ~ indep_value, 
                              data = .x, family = "binomial")),
         tidier = map(mod, ~tidy(.x, conf.int = T, exp = T)))

##### Table ----
res_logist_03_tab <- res_logist_03 |> 
  unnest(tidier) |> 
  filter(term == "indep_value") |> 
  select(-data, -mod, -term) |> 
  mutate(across(where(is.numeric), ~round(.x,3)))

export(res_logist_03_tab, "output/tables/250428_partA_logistic_02.xlsx")

##### kableExtra ----
res_logist_03_tab |> 
  select(-std.error, - statistic) |> 
  kable(col.names = c("Dependent", "Independent", "Odds Ratio", "p-value",
                      "5% CI", "95% CI")) |> 
  kable_styling(
    full_width = F,
    latex_options = c(
      "hold_position" # stop table floating
    ),
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  ) %>%
  collapse_rows(columns = 1, valign = "top")


#### With `Stage Early`-----
##### Model ----
res_logist_04 <- d04 |> 
  select(stage_early, all_of(c(variab_indep_02, variab_dep_02a))) |> 
  mutate(sex = if_else(sex == "M", 1, 0)) |> 
  pivot_longer(
    cols = all_of(variab_dep_02a),
    names_to = "dep_name",
    values_to = "dep_value"
  ) |> 
  pivot_longer(cols = all_of(variab_indep_02),
               names_to = "indep_name",
               values_to = "indep_value") |> 
  group_by(dep_name,indep_name) |> 
  nest() |> 
  mutate(mod = map(data, ~glm(dep_value ~ indep_value + stage_early, 
                              data = .x, family = "binomial")),
         tidier = map(mod, ~tidy(.x, conf.int = T, exp = T)),
         mod2 = map(data, ~glm(dep_value ~ indep_value * stage_early, 
                              data = .x, family = "binomial")),
         tidier2 = map(mod2, ~tidy(.x, conf.int = T, exp = T))
         )

##### Table ----
res_logist_04_tab_a <- res_logist_04 |> 
  unnest(tidier) |> 
  filter(!str_detect(term, "Intercept")) |> 
  select(-data, -mod, -mod2, -tidier2) |> 
  mutate(across(where(is.numeric), ~round(.x,3)))

res_logist_04_tab_b <- res_logist_04 |> 
  unnest(tidier2) |> 
  filter(!str_detect(term, "Intercept")) |> 
  select(-data, -mod, -mod2, -tidier) |> 
  mutate(across(where(is.numeric), ~round(.x,3)))


# export(res_logist_04_tab_a, "output/tables/250428_partA_logistic_03_a.xlsx")
# export(res_logist_04_tab_b, "output/tables/250428_partA_logistic_03_b.xlsx")

##### kableExtra ----
res_logist_04_tab_a |> 
  select(-std.error, - statistic) |> 
  kable(col.names = c("Dependent", "Adjusted for", "Independent", "Odds Ratio", "p-value",
                      "5% CI", "95% CI")) |> 
  kable_styling(
    full_width = F,
    latex_options = c(
      "hold_position" # stop table floating
    ),
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  ) %>%
  collapse_rows(columns = 1:2, valign = "top")

res_logist_04_tab_b |> 
  select(-std.error, - statistic) |> 
  kable(col.names = c("Dependent", "Adjusted for", "Independent", "Odds Ratio", "p-value",
                      "5% CI", "95% CI")) |> 
  kable_styling(
    full_width = F,
    latex_options = c(
      "hold_position" # stop table floating
    ),
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  ) %>%
  collapse_rows(columns = 1:2, valign = "top")

### Survival analyses ----
# https://www.themillerlab.io/posts/survival_analysis/
# https://thriv.github.io/biodatasci2018/r-survival.html#cox_regression
# https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/
# https://biostatsquid.com/easy-survival-analysis-r-tutorial/

#### Time to next treatment ----
res_surv_ttnt <- d04 |> 
  select(contains("ttnt"), ps_ecog, first_syst_th, ae_grade_3_4, monotherapy, sex) |> 
  mutate(sex = if_else(sex == "M",1,0)) |> 
  pivot_longer(cols = !contains("ttnt"),
               names_to = "indep_name",
               values_to = "indep_value") |> 
  group_by(indep_name) |> 
  nest() |> 
  mutate(surv_cat = map2(data, indep_name,
                     ~complex_surv_analyses_cat(df=.x, 
                                                time_to = "ttnt", 
                                                event = "ttnt_achieved",
                                                independent = .y))) |> 
  bind_rows(
    d04 |> 
      select(contains("ttnt"), bmi, age) |> 
      pivot_longer(cols = !contains("ttnt"),
                   names_to = "indep_name",
                   values_to = "indep_value") |> 
      group_by(indep_name) |> 
      nest() |> 
      mutate(surv_cont = map2(data, indep_name,
                              ~complex_surv_analyses_cont(df=.x, 
                                                          time_to = "ttnt", 
                                                          event = "ttnt_achieved",
                                                          independent = .y)))
    ) |> 
  mutate(surv = coalesce(surv_cat,surv_cont)) |> 
  select(-surv_cat,-surv_cont)




#### Duration treatment ----
res_surv_durTRTeffect <- d04 |> 
  select(treatment_duration, discontinuation_reason, ps_ecog, first_syst_th, 
         ae_grade_3_4, monotherapy, sex) |> 
  filter(!is.na(discontinuation_reason)) |> 
  mutate(discontinuation_reason = if_else(discontinuation_reason == 0, 0, 1),
         sex = if_else(sex == "M",1,0)) |> 
  pivot_longer(cols = -c("treatment_duration", "discontinuation_reason"),
               names_to = "indep_name",
               values_to = "indep_value") |> 
  group_by(indep_name) |> 
  nest() |> 
  mutate(surv_cat = map2(data, indep_name,
                         ~complex_surv_analyses_cat(df=.x, 
                                                    time_to = "treatment_duration", 
                                                    event = "discontinuation_reason",
                                                    independent = .y))) |> 
  bind_rows(
    d04 |> 
      select(treatment_duration, discontinuation_reason, bmi, age) |> 
      filter(!is.na(discontinuation_reason)) |> 
      mutate(discontinuation_reason = if_else(discontinuation_reason == 0, 0, 1)) |> 
      pivot_longer(cols = -c("treatment_duration", "discontinuation_reason"),
                   names_to = "indep_name",
                   values_to = "indep_value") |> 
      group_by(indep_name) |> 
      nest() |> 
      mutate(surv_cont = map2(data, indep_name,
                              ~complex_surv_analyses_cont(df=.x, 
                                                          time_to = "treatment_duration", 
                                                          event = "discontinuation_reason",
                                                          independent = .y))) 
  ) |> 
  mutate(surv = coalesce(surv_cat,surv_cont)) |> 
  select(-surv_cat,-surv_cont)


#### Disease Progression & Response Time ----
res_surv_response <- d04 |> 
  select(response_duration, progression, ps_ecog, first_syst_th, 
         ae_grade_3_4, monotherapy, sex) |> 
  mutate(sex = if_else(sex == "M",1,0)) |> 
  pivot_longer(cols = -c("response_duration", "progression"),
               names_to = "indep_name",
               values_to = "indep_value") |> 
  group_by(indep_name) |> 
  nest() |> 
  mutate(surv_cat = map2(data, indep_name,
                     ~complex_surv_analyses_cat(df=.x, 
                                                time_to = "response_duration", 
                                                event = "progression",
                                                independent = .y))) |> 
  bind_rows(
    d04 |> 
      select(response_duration, progression, bmi, age) |> 
      pivot_longer(cols = -c("response_duration", "progression"),
                   names_to = "indep_name",
                   values_to = "indep_value") |> 
      group_by(indep_name) |> 
      nest() |> 
      mutate(surv_cont = map2(data, indep_name,
                             ~complex_surv_analyses_cont(df=.x, 
                                                        time_to = "response_duration", 
                                                        event = "progression",
                                                        independent = .y))) 
    ) |> 
  mutate(surv = coalesce(surv_cat,surv_cont)) |> 
  select(-surv_cat,-surv_cont)

### Linear modelling ----
#### Time to effect ----
res_timeTOeffect <- d04 |> 
  select(response_time_to, all_of(variab_indep_02)) |> 
  mutate(sex = if_else(sex == "M",1,0)) |> 
  pivot_longer(cols = -response_time_to,
               names_to = "indep_name",
               values_to = "indep_value") |> 
  group_by(indep_name) |> 
  nest() |> 
  mutate(mod = map(data, ~lm(data = .x, response_time_to ~ indep_value)),
         tidier = map(mod, ~tidy(.x, conf.int = T)),
         qqplot = map2(mod, indep_name, ~ggqqplot(na.omit(resid(.x)) |> as_tibble(),
                                                  x = "value", title = .y)
                       )) 


res_timeTOeffect_tab <- res_timeTOeffect|> 
  unnest(tidier) |> 
  filter(term == "indep_value") |> 
  select(-(data:term), -qqplot)


res_timeTOeffect_tab |> 
  select(-std.error, - statistic) |> 
  kable(col.names = c("Independent", "Estimate", "p-value",
                      "5% CI", "95% CI"),
        digits = 3) |> 
  kable_styling(
    full_width = F,
    latex_options = c(
      "hold_position" # stop table floating
    ),
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  ) %>%
  collapse_rows(columns = 1, valign = "top")
