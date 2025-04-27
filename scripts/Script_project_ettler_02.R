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
fviz_mca_var(res_mca_02, repel = TRUE, ggtheme = theme_minimal(),
             col.var = "contrib",
             # choice = "mca.cor",
             gradient.cols = c("#bdbdbd", "#ffeda0", "#FC4E07"))
fviz_screeplot(res_mca_02)
fviz_ellipses(res_mca_02, c("sex", "ae_grade_3_4"),
              geom = "point")
dimdesc(res_mca_02, axes = c(1,2))


# Statistics ----
## Logistic regression ----
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


test <- res_logist_02 |> 
  unnest(tidier) |> 
  filter(term == "indep_value") |> 
  select(-data, -mod, -term)


library(sjPlot)

res_logist_02$mod[[23]] |> tab_model()
res_logist_02$mod[[23]] |> tab_model()

