# Libraries & functions ----
## libraries ----
pacman::p_load(update = T,  
               equatiomatic, beepr, tictoc, 
               tidyverse, purrr, furrr, easystats, rio, janitor, ggthemes, car,
               gtsummary, skimr, sjPlot, flextable, ggpubr, rstatix, tidymodels,
               psych, paletteer, ComplexHeatmap, future, multidplyr, corrr, 
               factoextra, lmerTest, ggforce, lazyWeave, FactoMineR, sjPlot,
               kableExtra, survival, survminer, ggsurvfit
               
)
## function specification ----
conflicted::conflicts_prefer(
  janitor::remove_empty,
  dplyr::filter,
  dplyr::mutate,
  dplyr::rename,
  dplyr::summarize,
  dplyr::summarise,
  dplyr::select,
  purrr::map,
  tidyr::extract,
  stats::chisq.test,
  base::intersect,
  base::setdiff,
  dplyr::last,
  dplyr::first, 
  dplyr::between,
  corrr::correlate,
  purrr::set_names
)

## Conflicted functions ----
ls_conflicted_01 <- conflicted::conflict_scout()

# Options ----
## Furrr
furrr_options(seed = TRUE,
              scheduling = Inf)
options(knitr.kable.NA = '') # empty space in cells with NAs in kable

## Markdown ----
options(knitr.kable.NA = '',     # empty space in cells with NAs in kable
        scipen = 999)            # non-academic format of numbers

## Possibly ----


## Number of cores ----
ncores <- availableCores() -1

## Okabe & Ito palette - colorblind palette ----
pal_okabe_ito <- colorblind_pal()(8)[2:8] # ggthemes
median_cl_boot <- function(x, conf = 0.95) {
  lconf <- (1 - conf)/2
  uconf <- 1 - lconf
  require(boot)
  bmedian <- function(x, ind) median(x[ind])
  bt <- boot(x, bmedian, 50000)
  bb <- boot.ci(bt, type = "perc")
  data.frame(y = median(x), ymin = quantile(bt$t, lconf), ymax = quantile(bt$t, 
                                                                          uconf))
}

# Figures ----
## Theme ----
# Theme for ggplot
theme_grey() + theme(plot.title = element_text(hjust = 0.5, size = 20,face="bold"),
                     plot.subtitle = element_text(size=15,face="italic"),
                     axis.title = element_text(size=12,face = "bold")) %>% 
  theme_set()

## Heatmap binary ----
heatmap_bin <- function(df, name_ae) {
  if (!require("ComplexHeatmap")) install.packages("ComplexHeatmap")
  df_filtered <- df |>
    na.omit()
  
  fa_col <- c("0" = 4, "1" = 3)
  grouping <- df_filtered |>
    select(ae_value) |>
    pull()
  df_heatmap <- df_filtered |>
    select(-ae_value) |>
    as.matrix()
  
  dend1 <- cluster_within_group(t(df_heatmap), grouping)
  
  fig1 <- Heatmap(t(df_heatmap),
                  cluster_columns = dend1, column_split = 2,
                  top_annotation = HeatmapAnnotation(
                    foo = grouping, col = list(foo = fa_col)
                  )
  )
  
  fig2 <- draw(fig1,
               column_title = name_ae,
               column_title_gp = grid::gpar(fontsize = 16)
  )
  
  return(fig2)
}

## biostatsquid theme ----
biostatsquid_theme <- theme(plot.title = element_text(size = rel(2)),
                            panel.grid.major.y = element_line(colour = 'gray'),
                            panel.grid.minor.y = element_line(colour = 'gray'),
                            panel.grid.major.x = element_blank(),
                            panel.grid.minor.x = element_blank(),
                            plot.background = element_rect(fill = NULL, colour = 'white'),
                            panel.background = element_rect(fill = 'white'),
                            # Axis stuff
                            axis.line = element_line(colour = 'black', linewidth = 1),
                            axis.text = element_text(colour = "black", face = 'bold'),
                            axis.text.x = element_text(size = rel(1)),
                            axis.text.y = element_text(size = rel(1)),
                            axis.title = element_text(size = rel(1.2)),
                            axis.ticks = element_line(colour = 'black', linewidth = 1.2),
                            # Legend stuff
                            legend.position = "bottom",
                            legend.margin = margin(6, 6, 6, 6),
                            legend.title = element_text(face = 'bold'),
                            legend.background = element_blank(),
                            legend.box.background = element_rect(colour = "black"))

# Tables ----
## skimr ----
my_skim <- skimr::skim_with(numeric = sfl(median = ~ median(., na.rm = TRUE),
                                          mad = ~ mad(., na.rm = TRUE)), 
                            append = T)

## Logistic regression ----
tab_logist_anal <- function(data, dependent_variable, independent_variable) {
  # definition of variables
  dependent_var <- dependent_variable
  independent_var <- independent_variable
  
  # data processing
  data_noNA <- data |> 
    drop_na(any_of(c(dependent_var, independent_var)))
  
  # univariate logistic regression - individual variables
  univ_tab <- data_noNA  |>
    dplyr::select(all_of(independent_var), ae_value ) |>  ## select variables of interest
    
    tbl_uvregression(                         ## produce univariate table
      method = glm,                           ## define regression want to run (generalised linear model)
      y = ae_value,                            ## define outcome variable
      method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
      exponentiate = TRUE                     ## exponentiate to produce odds ratios (rather than log odds)
    )
  
  # multivariable logistic regression - individual variables
  mv_reg <- independent_var  |>   ## begin with vector of explanatory column names
    str_c(collapse = "+")  |>      ## combine all names of the variables of interest separated by a plus
    (\(x) str_c("ae_value ~ ", x))() |>    ## combine the names of variables of interest with outcome in formula style
    glm(family = "binomial",      ## define type of glm as logistic,
        data = data_noNA)      
  
  mv_tab <- tbl_regression(mv_reg, exponentiate = TRUE)
  
  ## combine with univariate results 
  final_table <- tbl_merge(
    tbls = list(univ_tab, mv_tab),                          # combine
    tab_spanner = c("**Univariate**", "**Multivariable**")) |>  # set header names
    modify_caption(glue::glue("**{dependent_var}**"))
  
  
  return(final_table)
  
}

tab_logist_anal_uva <- function(data, dependent_variable, independent_variable) {
  # definition of variables
  dependent_var <- dependent_variable
  independent_var <- independent_variable
  # data processing
  data_noNA <- data |>
    drop_na(any_of(c(dependent_var, independent_var)))
  # univariate logistic regression - individual variables
  univ_tab <- data_noNA  |>
    dplyr::select(all_of(independent_var), ae_value ) |>  ## select variables of interest
    tbl_uvregression(                         ## produce univariate table
      method = glm,                           ## define regression want to run (generalised linear model)
      y = ae_value,                            ## define outcome variable
      method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
      exponentiate = TRUE                     ## exponentiate to produce odds ratios (rather than log odds)
    ) |>
    modify_caption(glue::glue("**{dependent_var}**"))
  return(univ_tab)
}

# Others ----