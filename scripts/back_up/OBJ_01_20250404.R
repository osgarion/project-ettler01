# Parameter selection ----
param_sel <- c("initials", "age", "sex", "bmi", "ae_grade_3_4",
               "ae_hyperTAG_grade_3_4","discontinued_due_to_ae", "ae_liver",
               "ae_hemato", "ps_ecog", "stage_early", "first_syst_th",
               "thyroid_disease_before", "monotherapy", "response_achieved",
               "dyslipidemia_before")

# Data uploading ----
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
