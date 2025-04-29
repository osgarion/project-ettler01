# Libraries & functions ----
## libraries ----
pacman::p_load(update = T,  
               equatiomatic, beepr, tictoc, 
               tidyverse, purrr, furrr, easystats, rio, janitor, ggthemes, car,
               gtsummary, skimr, sjPlot, flextable, ggpubr, rstatix, tidymodels,
               psych, paletteer, ComplexHeatmap, future, multidplyr, corrr, 
               factoextra, lmerTest, ggforce, lazyWeave, FactoMineR, sjPlot,
               kableExtra, survival, survminer, ggsurvfit, ggfortify, 
               adjustedCurves
               
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
## Survival Analyses ----
complex_surv_analyses <- function(df, time_to, event, independent) {
  
  # Required packages ----
  ## Function
  check_and_load_packages <- function(packages) {
    # Check for each package
    for (pkg in packages) {
      # Is the package attached? (loaded into search path)
      if (!(pkg %in% loadedNamespaces())) {
        # If not, load it
        library(pkg, character.only = TRUE)
      }
    }
  }
  ## Checking and loading
  check_and_load_packages(c("ggplot2", "survival", "adjustedCurves", "purrr",
                            "survival", "survminer"))
  
  # Data ----
  surv_data <- df |> 
    select(!!time_to, !!event, !!independent) |> 
    mutate(!!sym(independent) := factor(.data[[independent]])) |> 
    set_names(c("var_time","var_status","var_indep"))
  
  # Survival Analses ----
  ## Kaplan-Meier ----
  surv_obj <- Surv(surv_data$var_time,surv_data$var_status)
  survfit_obj <- survfit(surv_obj ~ 1, data = surv_data)
  ### Overall analyses ----
  fig_km_overall <- ggsurvplot(fit = survfit_obj, 
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
                               risk.table.fontsize = 4.5)
  
  ### Stratified analyses ----
  survfit_obj_pred <- survfit(surv_obj ~ var_indep, data = surv_data)
  
  fig_km_stratified <- ggsurvplot(fit = survfit_obj_pred, 
                                  data = surv_data,
                                  # Format Title
                                  title = "Overall Survival",
                                  subtitle = str_c("Stratified By ", independent),
                                  font.title = c(22, "bold", "black"),
                                  ggtheme = theme_classic() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))+ # theme_classic will give a white background with no lines on the plot
                                    theme(plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic")), 
                                  # Format Axes
                                  xlab="Days", # changes xlabel,
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
                                  legend = "none", # If you'd prefer more space for your plot, consider removing the legend
                                  legend.title = "All Patients",
                                  legend.labs = list('0' = 'No', '1' = 'Yes'), # Change the Strata Legend
                                  # p-value
                                  pval = TRUE,
                                  # Risk Table
                                  risk.table = TRUE, # Adds Risk Table
                                  risk.table.col = 'strata',
                                  risk.table.height = 0.25 # Adjusts the height of the risk table (default is 0.25)
  )
  
  ##Log-Rank Test ----
  log_rank <- survdiff(surv_obj ~ var_indep, data = surv_data)
  
  ## CoxPH ----
  res_cox_01 <- coxph( Surv(var_time,var_status) ~ var_indep, 
                       data = surv_data,
                       x = TRUE)
  res_cox_01_tab <- res_cox_01 |> 
    tbl_regression(exp = TRUE,
                   show_single_row = everything(),
                   label = setNames(list(independent), "var_indep")
    ) 
  
  ### Plot curves ----
  prefig_cox_01 <- adjustedsurv(data=surv_data, variable="var_indep", ev_time="var_time",
                                event="var_status", method="direct",
                                outcome_model=res_cox_01, conf_int=TRUE)
  
  fig_cox_01 <- plot(prefig_cox_01, conf_int=TRUE, risk_table=TRUE, 
                     risk_table_stratify=TRUE, method="direct",
                     risk_table_digits=0, x_n_breaks=10, median_surv_lines=TRUE,
                     risk_table_theme=ggplot2::theme_classic(),
                     gg_theme=ggplot2::theme_minimal(),
                     xlab="Time in Days", custom_colors=c('#E7B800', '#2e9fdf')) 
  
  ### Assumptions ----
  # Plot the Schoenfeld residuals over time for each covariate 
  assumption_check <- survival::cox.zph(res_cox_01)
  assumption_plot <- survminer::ggcoxzph(survival::cox.zph(res_cox_01), point.size = 0.1)
  
  # Forest plot
  theme_set(theme_grey()) # required for ggforest
  fig_forest <- ggforest(res_cox_01, 
                         data = surv_data)
  
  return(list(`KM overall` = fig_km_overall, 
              `KM stratified` = fig_km_stratified,
              `Log-Rank test` = log_rank,
              `CoxPH full outcome` = res_cox_01,
              `CoxPH print table` = res_cox_01_tab,
              `CoxPH plot` = fig_cox_01,
              `CoxPH assumption` = assumption_check,
              `CoxPH assumption plot` = assumption_plot,
              `CoxPH forest plot` = fig_forest))
  
}
