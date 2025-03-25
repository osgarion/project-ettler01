replace_if_exist <- function(data , lookup_df, 
                         column_to_replace, matching_column, replacing_column) {
  
  # Check if the specified columns exist in the data frames
  if (!(column_to_replace %in% colnames(data ) && matching_column %in% colnames(lookup_df) && replacing_column %in% colnames(lookup_df))) {
    stop("Specified columns do not exist in the data frames.")
  }
  
  result <- data %>%
    mutate_at(vars(column_to_replace), as.factor) %>% 
    left_join(lookup_df %>% 
                select(all_of(c(matching_column, replacing_column))) %>%
                mutate(across(all_of(replacing_column), factor)), 
              by = setNames(matching_column, column_to_replace)) %>%
    mutate(!!column_to_replace := if_else(!is.na(!!rlang::sym(replacing_column)), !!rlang::sym(replacing_column), as.character(!!column_to_replace)))  %>%  
    select(-{{replacing_column}})
  
  return(result)
}

# Example usage
# replace_from_other_df(data = d10, lookup_df = legend, 
#               column_to_replace = "collection", 
#               matching_column = "collection", replacing_column = "name")



# Alternative 
## change 'abbreviation' are 'full_name' in idx table and 'variable' is in input table data
# replace_if_exist <- function(data, idx, variable, abbreviation, full_name ) {
#   for (i in 1:nrow(idx)) {
#     pattern <- paste0("\\b", idx[[abbreviation]][i], "\\b")
#     replacement <- idx[[full_name]][i]
#     data$variable <- gsub(pattern, replacement, data[[variable]])
#     
#     # Also replace partial matches
#     partial_pattern <- idx[[abbreviation]][i]
#     data$variable <- gsub(partial_pattern, replacement, data[[variable]])
#   }
#   return(data)
# }


################# Alternative 2 ############################################################################
replace_if_exist <- function(df1, df2, feature_col, short_col, full_col) {
  # Check that df1 has the specified feature_col and df2 has the specified short_col and full_col
  if(!feature_col %in% colnames(df1)) {
    stop(paste("df1 must have a column named", feature_col))
  }
  if(!all(c(short_col, full_col) %in% colnames(df2))) {
    stop(paste("df2 must have columns named", short_col, "and", full_col))
  }
  
  # Create a named vector for mapping short names to full names
  name_mapping <- setNames(df2[[full_col]], df2[[short_col]])
  
  # Replace feature names in df1 using the mapping
  df1[[feature_col]] <- sapply(df1[[feature_col]], function(name) {
    if (name %in% names(name_mapping)) {
      return(name_mapping[name])
    } else {
      return(name)
    }
  })
  
  return(df1)
}

# example
# replace_if_exist(d06, desc_name_01, "feature_name", "abbreviation", "en")
################################################################################################################