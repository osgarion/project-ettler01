back_up <- function(path_source, path_dest = "scripts/back_up") {
  
  # Load required library
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(lubridate, tools)
  
  # Check if the destination subdirectory exists
  if (!dir.exists(path_dest)) {
    dir.create(path_dest, recursive = TRUE)
    cat("Created directory:", path_dest, "\n")
  } 
  
  # Extract the base name and extension
  base_name <- file_path_sans_ext(basename(path_source)) # Base name without extension
  extension <- file_ext(path_source)                    # Extract the extension
  
  # Construct the new file name with the date appended before the extension
  new_file_name <- paste0(base_name, "_", format(Sys.Date(), "%Y%m%d"), ".", extension)
  
  # Create the new destination path
  destination_file <- file.path(path_dest, new_file_name)
  
  # Copy the file
  invisible(file.copy(path_source, destination_file, overwrite = TRUE))
  
  cat("File copied to:", destination_file, "\n")
  
  # List all .R files in the directory
  files <- list.files(path_dest, pattern = "\\.R$", full.names = TRUE)
  
  # Sort files alphabetically
  files <- sort(files)
  
  # Loop through the files to compare and remove duplicates
  i <- 1
  while (i < length(files)) {
    # Read the current file and the next file
    current_file <- files[i]
    next_file <- files[i + 1]
    
    # Compare the contents of the two files
    if (identical(readLines(current_file, warn = FALSE), readLines(next_file, warn = FALSE))) {
      # If files are identical, remove the current file
      file.remove(next_file)
      
      # Remove the file from the list
      files <- files[-(i + 1)]
    } else {
      # Only move to the next file if the current one wasn't removed
      i <- i + 1
    }
  }
  
  cat("Comparison complete.\n")
  
}