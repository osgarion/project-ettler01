# Data uploading ----
d01 <- import("data/processed/Bexaroten 20250208_processed.xlsx")
legend <- import("data/processed/Bexaroten 20250208_processed.xlsx", which = "legend") |> 
  mutate(columname2 = names(d01))
rename_columns <- set_names(legend$columname2, legend$columname)
d01 <- d01 |> rename(any_of(rename_columns))
