rename_if_exists <- function(df, lookup_df) {
  for (old_col in names(df)) {
    if (old_col %in% lookup_df$abbreviation) {
      new_col <- lookup_df$en[lookup_df$abbreviation == old_col]
      df <- df %>% rename(!!new_col := !!old_col)
    }
  }
  return(df)
  print("This approach is better:")
  print("mtcars %>%
  rename_with(~ str_replace_all(., setNames(name_tbl$new_names, name_tbl$old_names)), everything())")
}
