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
back_up("scripts/Script_project_ettler_03.R") # the the destination subdirectory specify using 'path_dest'


# Save to .RData
save(legend_03, d04, 
     res_surv_ttnt_02, 
     res_surv_durTRTeffect_02, 
     res_surv_response_02,
     file = "reports/markD_03.RData")
save.image("reports/markD_03.RData")
# Save the tables into data/tables.RData by listing them individually
cgwtools::resave(xx2,xx3, file = "reports/markD_03.RData") # resave a list of tables that I'll use in the .Rmd file.
# Save the tables into data/tables.RData using "patterns" 
cgwtools::resave(list=ls(pattern="tbl"), file = "reports/markD_03.RData")

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

### Additional analyses ----
#### Stopped Treatment Due to AE ----
# all NaNs were removed
res_logist_05 <- d04 |> 
  select(all_of(variab_indep_01), discontinued_due_to_ae) |> 
  mutate(sex = if_else(sex == "M", 1, 0)) |> 
  pivot_longer(cols = all_of(variab_indep_01),
               names_to = "indep_name",
               values_to = "indep_value") |> 
  filter(!is.na(discontinued_due_to_ae)) |>
  group_by(indep_name) |> 
  nest() |> 
  mutate(mod = map(data, ~glm(discontinued_due_to_ae ~ indep_value, 
                              data = .x, family = "binomial")),
         tidier = map(mod, ~tidy(.x, conf.int = T, exp = T)))

res_logist_05_tab <- res_logist_05 |> 
  unnest(tidier) |> 
  filter(term == "indep_value") |> 
  select(-data, -mod, -term) |> 
  mutate(across(where(is.numeric), ~round(.x,3)))

res_logist_05_tab |> 
  select(-std.error, - statistic) |> 
  kable(col.names = c("Independent", "Odds Ratio", "p-value",
                      "5% CI", "95% CI")) |> 
  kable_styling(
    full_width = F,
    latex_options = c(
      "hold_position" # stop table floating
    ),
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  ) %>%
  collapse_rows(columns = 1, valign = "top")

#### BMI a hyperTAG Grade 3/4 ----
# Figure
d04 |> 
  ggplot(aes(x = factor(ae_hyperTAG_grade_3_4), 
             y = bmi, 
             color = factor(ae_hyperTAG_grade_3_4))) +
  geom_beeswarm(cex = 3.5, size = 3.5) +
  scale_color_brewer(
    palette = "Set1", 
    name = "Grade 3–4 Hypertriglyceridemia"
  ) +
  stat_summary(fun.data = ggpubr::median_mad, 
               geom = "errorbar", 
               color = "black", 
               width = 0.2, 
               size = 1) +
  stat_summary(fun = median, 
               geom = "crossbar", 
               color = "black", 
               size = 0.8, 
               width = 0.3) +
  labs(
    y = "Body Mass Index (BMI)",
    title = "Grade 3–4 Hypertriglyceridemia",
    caption = "The error bars represent Median Absolute Deviations (MAD) intervals with the median."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  ) + stat_compare_means(paired = F)

# Categorization
res_fisher_01 <- d04 |> 
  select(ae_hyperTAG_grade_3_4, bmi) |> 
  mutate(
    bmi = if_else(bmi < 25, 0, 1)
  ) |> 
  table() |> 
  fisher.test() |> 
  tidy() |> 
  mutate(Treshold = "BMI = 25") |> 
  relocate( method, Treshold) |> 
  bind_rows(
    d04 |> 
      select(ae_hyperTAG_grade_3_4, bmi) |> 
      mutate(
        bmi = if_else(bmi < 30, 0, 1)
      ) |> 
      table() |> 
      fisher.test() |> 
      tidy() |> 
      mutate(Treshold = "BMI = 30") |> 
      relocate(Treshold)
  )

res_fisher_01 |> 
  select(-alternative) |> 
  kable(col.names = c("Method", "Threshold", "Odds Ratio", "p-value",
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

# Fisher’s exact test was used to assess the association between high-grade hypertriglyceridemia and body mass index (BMI) thresholds. For BMI ≥25, the odds ratio (OR) was 2.74 (95% CI: 0.67–12.71, p = 0.134). For BMI ≥30, the association was statistically significant with an OR of 7.56 (95% CI: 1.19–86.18, p = 0.019).


## II. section ----
### Fisher exact test
res_fisher_02 <- d04 |> 
  select(response_achieved, stage_early) |> 
  table() |> 
  fisher.test() |> 
  tidy() |> 
  relocate( method) 

res_fisher_02 |> 
  select(-alternative) |> 
  kable(col.names = c("Method",  "Odds Ratio", "p-value",
                      "5% CI", "95% CI"),
        digits = 3) |> 
  kable_styling(
    full_width = F,
    latex_options = c(
      "hold_position" # stop table floating
    ),
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  ) 

### Logistic regression ----
#### Only with `Stage Early`
# for this purpose, the logicstic regression is not ideal
glm_spec <- logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")

glm_fit <- glm_spec %>%
  fit(factor(response_achieved) ~ factor(stage_early), data = d04)

glm_fit |> tbl_regression(exp = TRUE,
                          show_single_row = everything())

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

# export(res_logist_03_tab, "output/tables/250428_partA_logistic_02.xlsx")

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


#### Only `Stage early` ----
## TTNT
res_cox_ttnt <- coxph(Surv(ttnt , ttnt_achieved ) ~ stage_early, 
                      data = data_surv,
                      x = TRUE)
res_cox_ttnt |> 
  tbl_regression(exp = TRUE,
                 show_single_row = everything())

prefig_cox_res_cox_ttnt <- adjustedsurv(data=  data_surv, 
                                        variable="stage_early", ev_time="ttnt",
                                        event="ttnt_achieved", method="direct",
                                        outcome_model=res_cox_ttnt, conf_int=TRUE)

plot(prefig_cox_res_cox_ttnt, conf_int=TRUE, risk_table=TRUE,
     title = "Cox Proportional Hazards",
     subtitle = "Stratified By Stage Early",
     risk_table_stratify=TRUE, method="direct",
     risk_table_digits=0, x_n_breaks=10, median_surv_lines=TRUE,
     risk_table_theme=ggplot2::theme_classic(),
     gg_theme=ggplot2::theme_minimal(),
     xlab="Months", custom_colors=c('#E7B800', '#2e9fdf', '#FC4E07', '#9900CC')) 

## Duration  

res_cox_dur <- coxph(Surv(treatment_duration, discontinuation_reason) ~ stage_early, 
                     data = data_surv,
                     x = TRUE)
res_cox_dur |> 
  tbl_regression(exp = TRUE,
                 show_single_row = everything())

prefig_cox_res_cox_dur <- adjustedsurv(data=  data_surv, 
                                       variable="stage_early", ev_time="treatment_duration",
                                       event="discontinuation_reason", method="direct",
                                       outcome_model=res_cox_dur, conf_int=TRUE)

plot(prefig_cox_res_cox_dur, conf_int=TRUE, risk_table=TRUE,
     title = "Cox Proportional Hazards",
     subtitle = "Stratified By Stage Early",
     risk_table_stratify=TRUE, method="direct",
     risk_table_digits=0, x_n_breaks=10, median_surv_lines=TRUE,
     risk_table_theme=ggplot2::theme_classic(),
     gg_theme=ggplot2::theme_minimal(),
     xlab="Months", custom_colors=c('#E7B800', '#2e9fdf', '#FC4E07', '#9900CC')) 

## Disease Progression & Response Time 
res_cox_resp <- coxph(Surv(response_duration, progression) ~ stage_early, 
                      data = data_surv,
                      x = TRUE)
res_cox_resp |> 
  tbl_regression(exp = TRUE,
                 show_single_row = everything())

prefig_cox_res_cox_resp <- adjustedsurv(data=  data_surv, 
                                        variable="stage_early", ev_time="response_duration",
                                        event="progression", method="direct",
                                        outcome_model=res_cox_resp, conf_int=TRUE)

plot(prefig_cox_res_cox_resp, conf_int=TRUE, risk_table=TRUE,
     title = "Cox Proportional Hazards",
     subtitle = "Stratified By Stage Early",
     risk_table_stratify=TRUE, method="direct",
     risk_table_digits=0, x_n_breaks=10, median_surv_lines=TRUE,
     risk_table_theme=ggplot2::theme_classic(),
     gg_theme=ggplot2::theme_minimal(),
     xlab="Months", custom_colors=c('#E7B800', '#2e9fdf', '#FC4E07', '#9900CC'))



### Linear modelling ----
#### Time to effect ----
# without zore values
res_timeTOeffect_02 <- d04 |> 
  select(response_time_to, stage_early, all_of(variab_indep_02)) |> 
  filter(response_time_to > 0) |> 
  mutate(sex = if_else(sex == "M",1,0)) |> 
  pivot_longer(cols = -c("response_time_to", "stage_early"),
               names_to = "indep_name",
               values_to = "indep_value") |> 
  group_by(indep_name) |> 
  nest() |> 
  mutate(mod = map(data, ~lm(data = .x, response_time_to ~ indep_value + stage_early)),
         tidier = map(mod, ~tidy(.x, conf.int = T)),
         qqplot = map2(mod, indep_name, ~ggqqplot(na.omit(resid(.x)) |> as_tibble(),
                                                  x = "value", title = .y)
                       )) 


res_timeTOeffect_02_tab <- res_timeTOeffect_02|> 
  unnest(tidier) |> 
  filter(!str_detect(term, "(Intercept)")) |> 
  select(-(data:mod), -qqplot)


res_timeTOeffect_02_tab |> 
  select(-std.error, - statistic) |> 
  kable(col.names = c("Independent", "Term", "Estimate", "p-value",
                      "5% CI", "95% CI"),
        digits = 3) |> 
  kable_styling(
    full_width = F,
    latex_options = c(
      "hold_position" # stop table floating
    ),
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  ) %>%
  collapse_rows(columns = 1, valign = "top") |> 
  footnote(general_title = "Note.", 
           footnote_as_chunk = TRUE,
           threeparttable = TRUE,
           general = "Data without zero values.")


### Additional Analyses

### Additionally analyses----
#### Time to next treatment ----
res_surv_ttnt_02 <- d04 |> 
  select(contains("ttnt"), stage_early, ps_ecog, first_syst_th, ae_grade_3_4, monotherapy, sex) |> 
  mutate(sex = if_else(sex == "M",1,0)) |> 
  pivot_longer(cols = !(contains("ttnt") | stage_early),
               names_to = "indep_name",
               values_to = "indep_value") |> 
  mutate(var_indep_group = str_c(indep_name," ", indep_value, " - stage early ", stage_early)) |> 
  group_by(indep_name) |> 
  nest() |> 
  mutate(surv_cat = map2(data, indep_name,
                         ~complex_surv_analyses_cat_02(df=.x, 
                                                    time_to = "ttnt", 
                                                    event = "ttnt_achieved",
                                                    independent = .y))) |> 
  bind_rows(
    d04 |> 
      select(contains("ttnt"), stage_early, bmi, age) |> 
      pivot_longer(cols =!(contains("ttnt") | stage_early),
                   names_to = "indep_name",
                   values_to = "indep_value") |> 
      group_by(indep_name) |> 
      nest() |> 
      mutate(surv_cont = map2(data, indep_name,
                              ~complex_surv_analyses_cont_02(df=.x, 
                                                          time_to = "ttnt", 
                                                          event = "ttnt_achieved",
                                                          independent = .y)))
  ) |> 
  mutate(surv = coalesce(surv_cat,surv_cont)) |> 
  select(-surv_cat,-surv_cont)

# Figure
data_lm_01 <-  d04 |> 
  filter(response_time_to > 0) |> 
  select(response_time_to, bmi)

pred_data <- data_lm_01 %>%
  bind_cols(predict(lm_fit_bmi , new_data = data_lm_01,type = "conf_int")) |> 
  bind_cols(predict(lm_fit_bmi , new_data = data_lm_01))


model_stats <- tidy(lm_fit_bmi)
beta <- model_stats |>  filter(term == "bmi") |>  pull(estimate)
p_val <- model_stats %>% filter(term == "bmi") |>  pull(p.value)

lm_base <- extract_fit_engine(lm_fit_bmi)  # base R lm objekt
omega <- omega_squared(lm_base) |>  as.data.frame() |> 
  filter(Parameter == "bmi") |>  pull(Omega2)

annot_text <- sprintf("β = %.3f\np = %.3f\nω² = %.3f", beta, p_val, omega)
x_pos <- min(pred_data$bmi, na.rm = TRUE) *1.1
y_pos <- max(pred_data$response_time_to, na.rm = TRUE)


ggplot(pred_data, aes(x = bmi, y = response_time_to)) +
  geom_point(alpha = 0.7) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper), alpha = 0.2) +
  geom_line(aes(y = .pred), color = "blue", linewidth = 1) +
  labs(
    x = "BMI", y = "Time to response [months]",
    title = "Linear model fit - Time to response ~ BMI"
  ) +
  geom_text(
    aes(x = x_pos, y = y_pos, label = annot_text),
    hjust = 1, vjust = 1, size = 4.5, lineheight = 1.2
  ) +
  # theme_minimal(base_size = 14) +
  theme_sjplot2() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  )



#### Duration treatment ----
res_surv_durTRTeffect_02 <- d04 |> 
  select(treatment_duration, discontinuation_reason, stage_early, ps_ecog, first_syst_th, 
         ae_grade_3_4, monotherapy, sex) |> 
  filter(!is.na(discontinuation_reason)) |> 
  mutate(discontinuation_reason = if_else(discontinuation_reason == 0, 0, 1),
         sex = if_else(sex == "M",1,0)) |> 
  pivot_longer(cols = -(c("treatment_duration", "discontinuation_reason") | stage_early),
               names_to = "indep_name",
               values_to = "indep_value") |> 
  mutate(var_indep_group = str_c(indep_name," ", indep_value, " - stage early ", stage_early)) |> 
  group_by(indep_name) |> 
  nest() |> 
  mutate(surv_cat = map2(data, indep_name,
                         ~complex_surv_analyses_cat_02(df=.x, 
                                                    time_to = "treatment_duration", 
                                                    event = "discontinuation_reason",
                                                    independent = .y))) |> 
  bind_rows(
    d04 |> 
      select(treatment_duration, discontinuation_reason, stage_early, bmi, age) |> 
      filter(!is.na(discontinuation_reason)) |> 
      mutate(discontinuation_reason = if_else(discontinuation_reason == 0, 0, 1)) |> 
      pivot_longer(cols = -(c("treatment_duration", "discontinuation_reason") | stage_early),
                   names_to = "indep_name",
                   values_to = "indep_value") |> 
      group_by(indep_name) |> 
      nest() |> 
      mutate(surv_cont = map2(data, indep_name,
                              ~complex_surv_analyses_cont_02(df=.x, 
                                                          time_to = "treatment_duration", 
                                                          event = "discontinuation_reason",
                                                          independent = .y))) 
  ) |> 
  mutate(surv = coalesce(surv_cat,surv_cont)) |> 
  select(-surv_cat,-surv_cont)


#### Disease Progression & Response Time ----
res_surv_response_02 <- d04 |> 
  select(response_duration, progression, stage_early, ps_ecog, first_syst_th, 
         ae_grade_3_4, monotherapy, sex) |> 
  mutate(sex = if_else(sex == "M",1,0)) |> 
  pivot_longer(cols = -(c("response_duration", "progression") | stage_early),
               names_to = "indep_name",
               values_to = "indep_value") |> 
  mutate(var_indep_group = str_c(indep_name," ", indep_value, " - stage early ", stage_early)) |> 
  group_by(indep_name) |> 
  nest() |> 
  mutate(surv_cat = map2(data, indep_name,
                         ~complex_surv_analyses_cat_02(df=.x, 
                                                    time_to = "response_duration", 
                                                    event = "progression",
                                                    independent = .y))) |> 
  bind_rows(
    d04 |> 
      select(response_duration, progression, stage_early, bmi, age) |> 
      pivot_longer(cols = -(c("response_duration", "progression") | stage_early),
                   names_to = "indep_name",
                   values_to = "indep_value") |> 
      group_by(indep_name) |> 
      nest() |> 
      mutate(surv_cont = map2(data, indep_name,
                              ~complex_surv_analyses_cont_02(df=.x, 
                                                          time_to = "response_duration", 
                                                          event = "progression",
                                                          independent = .y))) 
  ) |> 
  mutate(surv = coalesce(surv_cat,surv_cont)) |> 
  select(-surv_cat,-surv_cont)
