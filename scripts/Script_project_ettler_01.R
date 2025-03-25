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

# Theme for ggplot
theme_grey() + theme(plot.title = element_text(hjust = 0.5, size = 20,face="bold"),
                                 plot.subtitle = element_text(size=15,face="italic"),
                                 axis.title = element_text(size=12,face = "bold")) %>% 
  theme_set()

## Conflicted functions ----
conflicted_functions <- conflicted::conflict_scout() 

# set up preferences
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::mutate)
conflicted::conflicts_prefer(dplyr::rename)
conflicted::conflicts_prefer(dplyr::summarize)

# Options ----
furrr_options(seed = TRUE,
              scheduling = Inf)
options(knitr.kable.NA = '') # empty space in cells with NAs in kable
## without scientific format
options(scipen=999)

# Okabe & Ito palette - colorblind palette
pal_okabe_ito <- colorblind_pal()(8)[2:8] # ggthemes


#Data manipulation ----
# data
df_batch1 <- 
#  %>% labelled::remove_attributes(c("label")) # "label" describes type of attributes -attr(*,"label") = ...
  
# import multiple sheets from .xlsx
  import_list("*.xlsx") %>% list2env(envir = .GlobalEnv)






# Markdown ----
options(knitr.kable.NA = '',     # empty space in cells with NAs in kable
        scipen = 999)            # non-academic format of numbers
# Save to .RData
save(xx1,
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
## Duplicates ----
eval_dup_01 <- d01 %>% get_dupes(!starts_with("p_")) %>% capture_messages()

## Missing values ----
vis_miss()
gg_miss_var()
gg_miss_var( ,facet=)
gg_miss_case()
gg_miss_case( ,facet = )
(miss_tab01 <- miss_var_summary())
(miss_tab02 <- miss_case_summary())

gg_miss_upset(d21) # give an overall pattern of missingness
gg_miss_fct(x = df, fct = group)

## Multivariate analysis ----
res_mvn <- mvn(data = d01, mvnTest = "mardia")
# create univariate Q-Q plots
mvn_qq <- mvn(data = d01, mvnTest = "mardia", univariatePlot = "qqplot")
# create univariate histograms
mvn_hist <- mvn(data = d01, mvnTest = "mardia", univariatePlot = "histogram")


## TRAFO package ----
fmla <- y ~ x
lin_mod <- df %>%  lm(fmla,.)
assumptions(lin_mod)
lin_Mod_trafo <- trafo_lm(lin_mod)
diagnostics(lin_Mod_trafo)


# Statistics ----



