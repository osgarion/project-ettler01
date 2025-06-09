# Parameter selection ----
param_sel <- c("initials", "age", "sex", "bmi", "ae_grade_3_4",
               "ae_hyperTAG_grade_3_4","discontinued_due_to_ae", "ae_liver",
               "ae_hemato", "ps_ecog", "stage_early", "first_syst_th",
               "thyroid_disease_before", "monotherapy", "response_achieved",
               "dyslipidemia_before")

param_sel_2 <- c("initials", "age", "sex", "bmi", "ps_ecog", "stage_early",
                 "first_syst_th", "response_achieved", "thyroid_disease_before",
                 "dyslipidemia_before", "monotherapy", "ttnt", "ttnt_achieved",
                 "response_time_to", "response_duration",	"progression",
                 "treatment_duration", "discontinuation_reason", "ae_grade_3_4",
                 "ae_liver", "ae_hemato", "discontinued_due_to_ae",
                 "ae_hyperTAG_grade_3_4", "ae_hyperTAG_any"
                 
)

variab_dep_01 <- c("discontinued_due_to_ae", "ae_hyperTAG_any", 
                    "ae_hyperTAG_grade_3_4", "ae_liver",	"ae_hemato")
variab_indep_01 <- c("age", "sex", "bmi", "ps_ecog", "stage_early", "first_syst_th",
                     "response_achieved", "dyslipidemia_before", 
                     "thyroid_disease_before", "monotherapy"  
)

variab_dep_02a <- c("response_achieved" 
)
variab_dep_02b <- c("treatment_duration", "discontinuation_reason",
                    "ttnt", "ttnt_achieved"
)
variab_dep_02c <- c("response_time_to",                 # only non-zero patients
                    "response_duration", "progression")
variab_indep_02 <- c("age", "sex", "bmi", "ps_ecog", "first_syst_th", "ae_grade_3_4",
                     "monotherapy")


# Data uploading ----
## Original version ----
d01 <- import("data/processed/Bexaroten 20250330_processed.xlsx")
legend <- import("data/processed/Bexaroten 20250330_processed.xlsx", which = "legend")
rename_columns <- set_names(legend$parameter, legend$columname)
d01 <- d01 |> rename(any_of(rename_columns))

d02 <- d01 |> select(any_of(param_sel))
d02_binary <- d02 |>
  select(-initials, -age, -bmi, -ps_ecog) |>
  mutate(sex = case_when(
    sex == "F" ~ 0,
    TRUE ~ 1
  )) 

## Updated version ----
d03 <- import("data/processed/Bexaroten 20250419_processed.xlsx")
legend_03 <- import("data/processed/Bexaroten 20250419_processed.xlsx", which = "legend")
rename_columns_03 <- set_names(legend_03$parameter, legend_03$columname)
d03 <- d03 |> rename(any_of(rename_columns_03)) |> 
  mutate(ps_ecog = if_else(ps_ecog == 0, 0, 1))

d04 <- d03 |> select(any_of(param_sel_2)) |> 
  mutate(discontinued_due_to_ae = str_remove_all(discontinued_due_to_ae, "x") |> 
           as.numeric())

## Survival analyses ----
data_surv <- d04 |> 
  filter(!is.na(discontinuation_reason)) |> 
  select(contains("ttnt"),
         treatment_duration, discontinuation_reason,
         response_duration, progression, bmi,
         stage_early) |> 
  mutate(stage_early = factor(stage_early),
         discontinuation_reason = if_else(discontinuation_reason == 0, 0, 1)) 
