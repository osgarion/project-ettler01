## Survival Analyses ----
complex_surv_analyses_cat <- function(df, time_to, event, independent) {
  
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
    set_names(c("var_time","var_status","var_indep")) |> 
    na.omit() |> 
    mutate(var_indep = factor(var_indep))
  
  # Survival Analses ----
  ## Kaplan-Meier ----
  surv_obj <- Surv(surv_data$var_time,surv_data$var_status)
  survfit_obj <- survfit(Surv(var_time,var_status) ~ 1, data = surv_data)
  ### Overall analyses ----
  fig_km_overall <- ggsurvplot(fit = survfit_obj, 
                               data = surv_data,
                               # Format Title
                               title = "Kaplan-Meier Overall Survival",
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
  survfit_obj_pred <- survfit(Surv(var_time,var_status) ~ var_indep, data = surv_data)
  
  fig_km_stratified <- ggsurvplot(fit = survfit_obj_pred, 
                                  data = surv_data,
                                  # Format Title
                                  title = "Kaplan-Meier Overall Survival",
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
  log_rank <- survdiff(Surv(var_time,var_status) ~ var_indep, data = surv_data)
  
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
                     title = "Cox Proportional Hazards",
                     subtitle = str_c("Stratified By ", independent),
                     risk_table_stratify=TRUE, method="direct",
                     risk_table_digits=0, x_n_breaks=10, median_surv_lines=TRUE,
                     risk_table_theme=ggplot2::theme_classic(),
                     gg_theme=ggplot2::theme_minimal(),
                     xlab="Time in Days", custom_colors=c('#E7B800', '#2e9fdf')) 
  
  ### Assumptions ----
  # Plot the Schoenfeld residuals over time for each covariate 
  assumption_check <- survival::cox.zph(res_cox_01)
  assumption_plot <- survminer::ggcoxzph(survival::cox.zph(res_cox_01), point.size = 0.1)
  
  ### Forest plot ----
  theme_set(theme_grey()) # required for ggforest
  fig_forest <- ggforest(res_cox_01, 
                         main = independent,
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


