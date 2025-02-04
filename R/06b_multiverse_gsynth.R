# Output is dependent on previous script, only runs for 960 iteractions, rather than 1152 (per param_grid)

library(dplyr)
library(readr)
library(here)
library(glue)
library(tidyverse)
library(tidylog)
library(patchwork)
library(augsynth)
library(furrr)
library(gsynth)
pacman::p_load(geofacet)
pacman::p_load(unglue)

# Define the input and output directories
input_dir <- here::here("gsynth_output")
output_dir <- here::here("gsynth_tables")

# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# List files and parse data using the updated pattern
files <- list.files(input_dir, full.names = FALSE)
pattern <- "{keyword}_{start_date}_{end_date}_treatment_{treatment}_verification_{verification_method}_force_{force}_estimator_{estimator}.rds"

# Parse the file names based on the pattern
parsed_data <- unglue_data(files, pattern) %>%
  mutate(
    path = here::here(str_c(input_dir, files, sep = '/')),
    rn = row_number()
  )

# Set up parallel processing
plan(multisession, workers = 4)  # Adjust workers to match your system's capabilities

# Define the processing function
process_row <- function(keyword, start_date, end_date, treatment, verification_method, force, estimator, path, rn) {
  tryCatch({
    # Create a single example tibble
    single_example <- tibble(
      keyword = keyword,
      start_date = start_date,
      end_date = end_date,
      treatment = treatment,
      verification_method = verification_method,
      force = force,
      estimator = estimator,
      path = path,
      rn = rn
    )
    
    # Read the model file
    gsynth_model <- read_rds(path)
    gsynth_summary <- summary(gsynth_model)
    
    # Calculate average ATT
    avg_att <- cumuEff(gsynth_model, cumu = TRUE) %>% 
      as_tibble() %>% 
      mutate(Level = 'Average', Time = row_number() - 1, ATT = catt / row_number())
    
    # Get ATT for treated states
    treated_states <- colnames(gsynth_model$Y.tr)
    state_att <- lapply(treated_states, function(state) {
      cumuEff(gsynth_model, cumu = TRUE, id = state) %>% 
        as_tibble() %>% 
        mutate(Level = state, Time = row_number() - 1, ATT = catt / row_number())
    }) %>% bind_rows()
    
    # Combine average and state-specific ATT
    joined_results <- bind_rows(avg_att, state_att) %>% 
      mutate(MSPE = gsynth_model$MSPE) %>%
      left_join(single_example, by = character())
    
    # Define output file path
    output_path <- single_example %>% select(-path) %>% str_c(collapse = "_")
    
    # Write results to CSV
    write_csv(joined_results, here::here(output_dir, str_c(output_path, '.csv')))
    
    message(glue("Processed row {rn}: {path}"))
  }, error = function(e) {
    message(glue("Error processing row {rn}: {e$message}"))
  })
}

# Process rows in parallel using future_pmap
future_pmap(
  parsed_data,
  process_row,
  .progress = TRUE  # Show progress bar
)

# Clean up parallel session
plan(sequential)
