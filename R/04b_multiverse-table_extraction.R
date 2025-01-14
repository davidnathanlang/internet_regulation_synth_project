library(dplyr)
library(readr)
library(here)
library(glue)
library(tidyverse)
library(tidylog)
library(patchwork)
library(augsynth)
library(furrr)
pacman::p_load(geofacet)
pacman::p_load(unglue)

# List files and parse data
mm <- list.files(here::here('multiverse_mod'))
pattern <- "{keyword}_{start_date}_{end_date}_{covariate_tag}_scm{scm}_fixedeff{fixedeffects}_leads{leads}_lags{lags}_treatment_{treatment}.rds"

parsed_data <- unglue_data(mm, pattern) %>%
  mutate(
    path = list.files(here::here('multiverse_mod'), full.names = TRUE),
    rn = row_number()
  )

# Loop through each row in parsed_data
for (i in seq_len(nrow(parsed_data))) {
  # Extract the current row as a single-row data frame
  single_example <- parsed_data[i, ]
  
  # Define the output file path
  output_path <- here::here(glue("multiverse_tables/{single_example$rn}.csv"))
  
  # Skip processing if the output file already exists
  if (file.exists(output_path)) {
    message(glue("Skipping row {i}: Output file already exists."))
    next
  }
  
  # Try-catch block to handle potential errors in processing
  tryCatch({
    # Read the model
    ms_mod <- read_rds(single_example$path)
    ms_summary <- summary(ms_mod)
    
    # Post-treatment analysis
    post_treatment <- ms_summary$att %>%
      as.data.frame() %>%
      filter(Time >= 0) %>%
      group_by(Level) %>%
      summarise(
        POST_MSPE = mean(Estimate^2),
        ATT = mean(Estimate),
        CATT = sum(Estimate),
        .groups = "drop"
      )
    
    # Pre-treatment analysis
    pre_treatment <- ms_summary$att %>%
      as.data.frame() %>%
      filter(Time < 0) %>%
      group_by(Level) %>%
      summarise(
        PRE_MSPE = mean(Estimate^2),
        MAB = mean(abs(Estimate)),
        MEAN_ERROR = mean(Estimate),
        .groups = "drop"
      )
    
    # Combine results
    joined_results <- single_example %>%
      full_join(pre_treatment, by = character()) %>%
      full_join(post_treatment, by = "Level")
    
    # Write results to a CSV file
    write_csv(joined_results, output_path)
    
    message(glue("Processed row {i}: {single_example$path}"))
  }, error = function(e) {
    message(glue("Error processing row {i}: {e$message}"))
  })
}
