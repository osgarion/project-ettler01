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






