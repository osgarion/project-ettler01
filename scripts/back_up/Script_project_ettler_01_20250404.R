# Header 1 ----
## Header 2 ----
### Header 3 ----

# References ----
# model diagnosis
## https://easystats.github.io/see/articles/performance.html
## https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
# statistical data and model report
## report(model) # report(model)
## https://github.com/easystats/report
# Multivariate analyses
## https://cran.r-project.org/web/packages/MVN/vignettes/MVN.pdf
# Regression model summary
## https://strengejacke.github.io/sjPlot/articles/tab_model_estimates.html
## https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
# Regression model plots
## https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html
# Markdown session info
## https://stackoverflow.com/questions/33156946/how-can-i-format-sessioninfo-in-rmarkdown

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
back_up("scripts/Script_project_ettler_01.R") # the the destination subdirectory specify using 'path_dest'


# Save to .RData
save(d01,res_logist_tab_01, legend,
     file = "reports/markD_01.RData")
save.image("reports/markD_01.RData")
# Save the tables into data/tables.RData by listing them individually
cgwtools::resave(xx2,xx3, file = "reports/markD_01.RData") # resave a list of tables that I'll use in the .Rmd file.
# Save the tables into data/tables.RData using "patterns" 
cgwtools::resave(list=ls(pattern="tbl"), file = "reports/markD_01.RData")

# copy-to-clipboard botton
# ```{r xaringanExtra-clipboard, echo=FALSE}
# xaringanExtra::use_clipboard()
# ```

# EDA ----
## Proportion tables ----
# Barplot
d02 |> 
  select(-initials, -age, -bmi) |> 
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


# Baloonplot
# tiff("output/figures/250403_eda_baloonplot_01.tiff", width = 8, height = 8, 
#      units = "in", res = 300, bg = "white")
d02_binary |> 
  pivot_longer(cols = c("ae_grade_3_4","ae_hyperTAG_grade_3_4","ae_liver",
                        "discontinued_due_to_ae", "ae_liver", "ae_hemato"),
               names_to = "ae_name",
               values_to = "ae_value") |> 
  na.omit() |> 
  group_by(ae_name, ae_value) |> 
  summarise(across(sex:response_achieved, sum), .groups = "drop") |>
  mutate(
    ae_name = as.factor(ae_name),
    ae_value = case_when(
      ae_value == 0 ~ "no",
      ae_value == 1 ~ "yes"
    ),
    ea = paste(ae_name, ae_value, sep = "_")
  ) |>
  select(-ae_name, -ae_value) |> 
  column_to_rownames("ea") |> 
  ggballoonplot(fill = "value") +
  scale_fill_viridis_c(option = "C")
# dev.off()


## Correlation ----
# Tetrachoric analyses
res_tetra <- psych::tetrachoric(
  d02_binary
  )


corrplot::corrplot(res_tetra$rho, method = 'square', tl.col = 'black',
                   order = 'AOE', type = 'lower', addCoef.col = 'black',
                   col=colorRampPalette(c("blue","white","red"))(200),
                   diag = FALSE)
# Pearson
d02_binary |> 
  correlate()  |> 
  shave() |> 
  rplot(print_cor=TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Clustering ----

fig_heatmap_bin_01 <- d02_binary |> 
  pivot_longer(cols = c("ae_grade_3_4","ae_hyperTAG_grade_3_4","ae_liver",
                        "discontinued_due_to_ae", "ae_liver", "ae_hemato"),
               names_to = "ae_name",
               values_to = "ae_value") |> 
  na.omit() |> 
  relocate(ae_value) |> 
  group_by(ae_name) |> 
  nest() |> 
  mutate(fig = map2(data, ae_name, ~heatmap_bin(df = .x, name_ae = .y)))

walk(test$fig, print)

### Save ----
# Directory to save the SVG files
output_dir <- "F:/Analysis/github/project-ettler01/output/figures/250404_heatmaps"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

fig_heatmap_bin_01 %>%
  rowwise() %>%
  mutate(
    file_name = paste(ae_name),
    # file_path = file.path(output_dir, paste0(file_name, ".svg"))
    file_path = file.path(output_dir, paste0(file_name, ".tiff"))
  ) %>%
  ungroup() %>% # Remove rowwise grouping
  select(fig, file_path) %>% # Keep only necessary columns
  pwalk(function(fig, file_path) {
    ## using ggsave - works onyl with "ggplot" of "gg" classes
    # ggsave(file_path, plot = fig, device = "svg", width = 8, height = 6)
    # ggsave(file_path, plot = fig, device = "tiff", width = 8, height = 6, dpi = 300)
    tiff(file_path, width = 8, height = 8, units = "in", res = 300)
    ComplexHeatmap::draw(fig)
    dev.off()
  })

## Visualization Ideas
ComplexUpset::upset(d02_binary, intersect = d02_binary |> names())


# Statistics ----
## Correspondence analyses ----
### Visae ----
# https://cran.r-project.org/web/packages/visae/vignettes/ca_biplots.html
# DOI: 10.1186/s12874-021-01368-w
pacman::p_load(visae)

d02 |> run_ca(group = ctcl_type,
              id = initials,
              ae_grade = ae_grade_3_4)

## Multiple correspondence analyses ----
### factoextra ----
# https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/
res_mca <- d02 |> select(-initials, -age, -bmi) |> na.omit() |> 
  mutate(across(everything(),as.factor)) |> 
  MCA(graph = FALSE)
fviz_mca_var(res_mca, repel = TRUE, ggtheme = theme_minimal(),
             col.var = "contrib",
             # choice = "mca.cor",
             gradient.cols = c("#bdbdbd", "#ffeda0", "#FC4E07"))
fviz_screeplot(res_mca)
fviz_ellipses(res_mca, c("sex", "ae_grade_3_4"),
              geom = "point")
dimdesc(res_mca, axes = c(1,2))

## Univariate & Multivariable logistic regression ----
tic()
Sys.time()
no_cores <- availableCores() - 1
cluster <- new_cluster(no_cores)
cluster_library(cluster, "MASS") |> cluster_library("data.table") |> 
  cluster_library("tidyverse") |> cluster_library("gtsummary")
cluster_copy(cluster, 'tab_logist_anal') |> 
  cluster_copy('tab_logist_anal_uva')

res_logist_tab_01 <- d02 |>
  mutate(sex = case_when(
    sex == "F" ~ 0,
    TRUE ~ 1
  )) |> 
  pivot_longer(cols = c("ae_grade_3_4","ae_hyperTAG_grade_3_4","ae_liver",
                        "discontinued_due_to_ae", "ae_liver", "ae_hemato"),
               names_to = "ae_name",
               values_to = "ae_value") |> 
  na.omit() |> 
  relocate(ae_value) |> 
  group_by(ae_name) |> 
  nest() |> 
  multidplyr::partition(cluster = cluster) |> 
  mutate(mod_01 = map2(data, ae_name, 
                    ~tab_logist_anal(data = .x, 
                                     dependent_variable = .y,
                                     independent_variable = c("bmi", "sex", "age"))),
         mod_02 = map2(data, ae_name, 
                       ~tab_logist_anal(data = .x, 
                                        dependent_variable = .y,
                                        independent_variable = c("bmi", "sex"))),
         mod_03 = map2(data, ae_name, 
                       ~tab_logist_anal(data = .x, 
                                        dependent_variable = .y,
                                        independent_variable = c("bmi", "age"))),
         mod_04 = map2(data, ae_name, 
                       ~tab_logist_anal_uva(data = .x, 
                                        dependent_variable = .y,
                                        independent_variable = c("stage_early","ps_ecog",
                                                                 "first_syst_th", "response_achieved",
                                                                 "monotherapy", "dyslipidemia_before"))),
         mod_05 = map2(data, ae_name, 
                       ~tab_logist_anal(data = .x, 
                                        dependent_variable = .y,
                                        independent_variable = c("bmi", "sex", "age",
                                                                 "stage_early"))),
         mod_06 = map2(data, ae_name, 
                       ~tab_logist_anal(data = .x, 
                                        dependent_variable = .y,
                                        independent_variable = c("bmi", "sex", "age",
                                                                 "ps_ecog"))),
         mod_07 = map2(data, ae_name, 
                       ~tab_logist_anal(data = .x, 
                                        dependent_variable = .y,
                                        independent_variable = c("bmi", "sex", "age",
                                                                 "first_syst_th"))),
         mod_08 = map2(data, ae_name, 
                       ~tab_logist_anal(data = .x, 
                                        dependent_variable = .y,
                                        independent_variable = c("bmi", "sex", "age",
                                                                 "response_achieved"))),
         mod_09 = map2(data, ae_name, 
                       ~tab_logist_anal(data = .x, 
                                        dependent_variable = .y,
                                        independent_variable = c("bmi", "sex", "age",
                                                                 "monotherapy"))),
         mod_10 = map2(data, ae_name, 
                       ~tab_logist_anal(data = .x, 
                                        dependent_variable = .y,
                                        independent_variable = c("bmi", "sex", "age",
                                                                 "dyslipidemia_before")))
         ) |> 
  collect()
walk(cluster, ~ .x$kill())
toc()
beep(4)

### Export tables ----
# Write to Excel file with each list element as a sheet
export(res_logist_tab_01$mod_01 |> 
         map(as_tibble) |>
         set_names(res_logist_tab_01$ae_name),
       "output/tables/250404_logistic_bmi_sex_age.xlsx")
export(res_logist_tab_01$mod_02 |> 
         map(as_tibble) |>
         set_names(res_logist_tab_01$ae_name),
       "output/tables/250404_logistic_bmi_sex.xlsx")
export(res_logist_tab_01$mod_03 |> 
         map(as_tibble) |>
         set_names(res_logist_tab_01$ae_name),
       "output/tables/250404_logistic_bmi_age.xlsx")
export(res_logist_tab_01$mod_04 |> 
         map(as_tibble) |>
         set_names(res_logist_tab_01$ae_name),
       "output/tables/250404_logistic_univariate.xlsx")
export(res_logist_tab_01$mod_05 |> 
         map(as_tibble) |>
         set_names(res_logist_tab_01$ae_name),
       "output/tables/250404_logistic_bmi_sex_age_stageEarly.xlsx")
export(res_logist_tab_01$mod_06 |> 
         map(as_tibble) |>
         set_names(res_logist_tab_01$ae_name),
       "output/tables/250404_logistic_bmi_sex_age_ecog.xlsx")
export(res_logist_tab_01$mod_07 |> 
         map(as_tibble) |>
         set_names(res_logist_tab_01$ae_name),
       "output/tables/250404_logistic_bmi_sex_age_firstSyst.xlsx")
export(res_logist_tab_01$mod_08 |> 
         map(as_tibble) |>
         set_names(res_logist_tab_01$ae_name),
       "output/tables/250404_logistic_bmi_sex_age_responseAchieved.xlsx")
export(res_logist_tab_01$mod_09 |> 
         map(as_tibble) |>
         set_names(res_logist_tab_01$ae_name),
       "output/tables/250404_logistic_bmi_sex_age_monotherapy.xlsx")
export(res_logist_tab_01$mod_10 |> 
         map(as_tibble) |>
         set_names(res_logist_tab_01$ae_name),
       "output/tables/250404_logistic_bmi_sex_age_dyslipidemiaBefore.xlsx")

