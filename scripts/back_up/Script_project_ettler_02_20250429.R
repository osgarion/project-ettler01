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

export(res_logist_02_tab, "output/tables/250428_partA_logistic_01.xlsx")

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
#### Model ----
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

#### Table ----
res_logist_03_tab <- res_logist_03 |> 
  unnest(tidier) |> 
  filter(term == "indep_value") |> 
  select(-data, -mod, -term) |> 
  mutate(across(where(is.numeric), ~round(.x,3)))

export(res_logist_03_tab, "output/tables/250428_partA_logistic_02.xlsx")

#### kableExtra ----
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

### Survival analyses ----
# https://www.themillerlab.io/posts/survival_analysis/
# https://thriv.github.io/biodatasci2018/r-survival.html#cox_regression
# https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/
# https://biostatsquid.com/easy-survival-analysis-r-tutorial/

#### Time to next treatment ----
# data
surv_data <- d04 |> 
  select(ttnt, ttnt_achieved, first_syst_th) |> 
  mutate(first_syst_th = factor(first_syst_th))

surv_obj <- Surv(surv_data$ttnt,surv_data$first_syst_th)

##### Kaplan-Meier ----
survfit_obj <- survfit(surv_obj ~ 1, data = surv_data)

# Kaplan-Meier using ggsurvplot
fig_km_01 <- ggsurvplot(fit = survfit_obj, 
           data = surv_data,
           # Format Title
           title = "Overall Survival",
           subtitle = "Unstratified Cohort",
           font.title = c(22, "bold", "black"),
           ggtheme = theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
             theme(plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic")), # center and bold title and subtitle
           # Format Axes
           xlab="Days", # changes xlabel,
           ylab = "Survival Probability",
           font.x=c(18,"bold"), # changes x axis labels
           font.y=c(18,"bold"), # changes y axis labels
           font.xtickslab=c(14,"plain"), # changes the tick label on x axis
           font.ytickslab=c(14,"plain"),
           # Format Curve Lines
           palette = "black",
           # Censor Details
           censor.shape="|",
           censor.size = 5,
           # Confidence Intervals
           conf.int = TRUE, # To Remove conf intervals use "FALSE"
           conf.int.fill = "purple", # fill color to be used for confidence interval
           surv.median.line = "hv", # allowed values include one of c("none", "hv", "h", "v"). v: vertical, h:horizontal
           # Format Legend
           legend.title = "All Patients",
           legend.labs = "All Patients", # Change the Strata Legend
           # Risk Table
           risk.table = TRUE, # Adds Risk Table
           risk.table.height = 0.25, # Adjusts the height of the risk table (default is 0.25)
           risk.table.fontsize = 4.5
)

survfit_obj_pred <- survfit(surv_obj ~ first_syst_th, data = surv_data)


fig_km_02 <- ggsurvplot(fit = survfit_obj_pred, 
           data = surv_data,
           # Format Title
           title = "Overall Survival",
           subtitle = "Stratified By First Systemic Therapy",
           font.title = c(22, "bold", "black"),
           ggtheme = theme_classic() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))+ # theme_classic will give a white background with no lines on the plot
             theme(plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic")), 
           # Format Axes
           xlab="Time to next treatment [Days]", # changes xlabel,
           ylab = "Survival Probability",
           font.x=c(18,"bold"), # changes x axis labels
           font.y=c(18,"bold"), # changes y axis labels
           font.xtickslab=c(14,"plain"), # changes the tick label on x axis
           font.ytickslab=c(14,"plain"),
           break.time.by=10,
           # Format Curve Lines
           palette = c('#E7B800', '#2e9fdf'),
           # Censor Details
           censor = TRUE, # logical value. If TRUE, censors will be drawn,
           censor.shape="|",
           censor.size = 5,
           # Confidence Intervals
           conf.int = TRUE, # To Remove conf intervals use "FALSE"
           # conf.int.fill = "purple", # fill color to be used for confidence interval
           surv.median.line = "hv", # allowed values include one of c("none", "hv", "h", "v"). v: vertical, h:horizontal
           # Format Legend
           legend = "right", # If you'd prefer more space for your plot, consider removing the legend
           legend.title = "All Patients",
           legend.labs = list('0' = 'No', '1' = 'Yes'), # Change the Strata Legend
           # p-value
           pval = TRUE,
           # Risk Table
           risk.table = TRUE, # Adds Risk Table
           risk.table.col = 'strata',
           risk.table.height = 0.25 # Adjusts the height of the risk table (default is 0.25)
)

# ggsurvplot(res_surv_02, data = d04,
#            size = 1,
#            palette = c('#E7B800', '#2e9fdf'),
#            censor.shape = '|', censor.size = 4,
#            conf.int = TRUE,
#            pval = TRUE,
#            risk.table = TRUE,
#            risk.table.col = 'strata',
#            legend.labs = list('0' = 'Not achieved', '1' = 'Achieved'),
#            risk.table.height = 0.25,
#            ggtheme = theme_bw())

# ggsurvplot(fit = survfit_obj_pred, 
#            data = surv_data,
#            title = "Overall Survival",
#            subtitle = "Stratified By Histology",
#            font.title = c(22, "bold", "black"),
#            ggtheme = theme_grey() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))+ # theme_grey will give a grey background with  lines on the plot
#              theme(plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic")),
#            # Censor Details
#            censor = TRUE, #ogical value. If TRUE, censors will be drawn
#            censor.shape="|",
#            censor.size = 5,
#            # Confidence Intervals
#            conf.int = TRUE, # To Remove conf intervals use "FALSE"
#            surv.median.line = "hv", # allowed values include one of c("none", "hv", "h", "v"). v: vertical, h:horizontal
#            # Format Axes
#            xlab="Days", # changes xlabel,
#            ylab = "Survival Probability",
#            font.x=c(18,"bold"), # changes x axis labels
#            font.y=c(18,"bold"), # changes y axis labels
#            font.xtickslab=c(14,"plain"), # changes the tick label on x axis
#            font.ytickslab=c(14,"plain"),
#            # Format Legend
#            legend.title = "All Patients",
#            legend.labs = c("No","Yes"), # Change the Strata Legend
#            legend = c(0.8,0.8), #c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top right" position
#            # Plot Dimensions
#            surv.plot.height = 0.95, # Default is 0.75
#            # Risk Table
#            risk.table = FALSE, # Adds Risk Table
#            risk.table.height = 0.35, # Adjusts the height of the risk table (default is 0.25)
#            risk.table.fontsize = 3,
#            # p-value details
#            pval = TRUE,
#            pval.size = 5,
#            pval.coord = c(1,1)
# )

# s1 <- survfit(Surv(ttnt, ttnt_achieved) ~ 1, data = surv_data)
# 
# km_curve <- ggsurvfit(s1, linewidth = 1) +
#   labs(x = 'Months', y = 'Overall survival') +
#   add_confidence_interval() +
#   add_risktable() +
#   scale_ggsurvfit() + 
#   biostatsquid_theme +
#   coord_cartesian(xlim = c(0, 8))
# 
# km_curve +
#   geom_vline(xintercept = 1, linetype = 'dashed', colour = 'red', size = 1) +
#   geom_hline(yintercept = summary(s1, times = 12)$surv, linetype = 'dashed', colour = 'red', size = 1) + 
#   coord_cartesian(xlim = c(0, 8))

# Log-Rank Test
survdif_obj <- survdiff(surv_obj ~ first_syst_th, data = surv_data)
survdif_obj 

##### CoxPH ----
res_cox_01 <- coxph( Surv(ttnt,ttnt_achieved) ~ first_syst_th, 
                     data = surv_data,
                     x = TRUE)
summary(coxph_obj_pred)
res_cox_01 |> 
  tbl_regression(exp = TRUE,
                 show_single_row = everything()) 




# calculate and plot curves
prefig_cox_01 <- adjustedsurv(data=surv_data, variable="first_syst_th", ev_time="ttnt",
                    event="ttnt_achieved", method="direct",
                    outcome_model=res_cox_01, conf_int=TRUE)

fig_cox_01 <- plot(prefig_cox_01, conf_int=TRUE, risk_table=TRUE, 
                   risk_table_stratify=TRUE, ethod="direct",
     risk_table_digits=0, x_n_breaks=10, median_surv_lines=TRUE,
     risk_table_theme=ggplot2::theme_classic(),
     gg_theme=ggplot2::theme_minimal(),
     xlab="Time in Days", custom_colors=c('#E7B800', '#2e9fdf')) 

# ggadjustedcurves(res_cox_01, data=surv_data,
#                  variable = "first_syst_th",
#                  size = 1,
#                  palette = c('#E7B800', '#2e9fdf'),
#                  ggtheme = theme_bw()) 

# coxph_obj_pred |> plot()

# Plot the Schoenfeld residuals over time for each covariate
survival::cox.zph(res_cox_01)
survminer::ggcoxzph(survival::cox.zph(res_cox_01), point.size = 0.1)


# Forest plot
theme_set(theme_grey()) # required for ggforest
ggforest(res_cox_01, 
         data = d04)








