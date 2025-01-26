# Load required libraries
library(gsynth)
library(dplyr)
library(readr)
library(glue)
library(here)
library(tidyr)
library(furrr)

# Define parameters
keywords <- c("pornhub", "xvideos", "vpn", "porn")
time_ranges <- c("2019-01-01 2024-10-31", "2022-01-01 2024-10-31")
treatment_options <- c("post_treat", "post_treat_passage", "post_treat_enforcement_date")
verification_options <- c(
  "government_id", "digitized_id", "transaction_data", "database_id",
  "any_reasonable_method", "any_id_requirement"
)
force_options <- c("none", "unit", "time", "two-way")
estimator_options <- c("ife", "mc")

output_dir <- here::here("gsynth_output")
if (!dir.exists(output_dir)) dir.create(output_dir)

# Function to generate file path for caching
get_output_path <- function(output_dir, keyword, time_range, treatment, verification_method, force, estimator) {
  here::here(glue("{output_dir}/{keyword}_{gsub(' ', '_', time_range)}_treatment_{treatment}_verification_{verification_method}_force_{force}_estimator_{estimator}.rds"))
}

# Function to run gsynth for a single combination of parameters
run_gsynth <- function(keyword, time_range, treatment, verification_method, force, estimator, output_dir) {
  print(glue("Running gsynth for {keyword} + {time_range} + {treatment} + {verification_method} + {force} + {estimator}"))
  
  # Generate output path
  output_path <- get_output_path(output_dir, keyword, time_range, treatment, verification_method, force, estimator)
  
  # Check if model already exists
  if (file.exists(output_path)) {
    message(glue("Model already exists for {keyword}, {time_range}, {treatment}, {verification_method}, {force}, {estimator}. Skipping..."))
    return(readRDS(output_path)) # Load cached model
  }
  
  # Load and filter data
  keyword_df <- tryCatch(
    {
      read_csv(here::here(glue("data/{keyword}.csv"))) %>%
        mutate(
          post_treat_passage = if_else(date >= passage_date, 1L, 0L),
          post_treat_enforcement_date = if_else(date >= enforcement_date, 1L, 0L)
        ) %>%
        filter(
          between(as.Date(date), as.Date(str_split(time_range, " ")[[1]][1]), as.Date(str_split(time_range, " ")[[1]][2]))
        ) %>%
        filter(
          (get(treatment) == 0 | (get(verification_method) & get(treatment))) == TRUE
        ) %>%
        distinct(state, date, .keep_all = TRUE)
    },
    error = function(e) {
      message(glue("Error reading data for keyword {keyword}: {e$message}"))
      return(NULL)
    }
  )
  
  if (is.null(keyword_df) || nrow(keyword_df) == 0) {
    message(glue("No data available for {keyword} in {time_range}. Skipping..."))
    return(NULL)
  }
  
  # Fit gsynth model
  model <- tryCatch(
    gsynth(
      formula = as.formula(glue("hits ~ {treatment}")),
      data = keyword_df,
      index = c("state", "date"),
      CV = TRUE,
      r = c(0, 5),
      force = force,
      estimator = estimator,
      inference = "parametric"
    ),
    error = function(e) {
      message(glue("Error running gsynth for {keyword}, {time_range}, {treatment}, {verification_method}, {force}, {estimator}: {e$message}"))
      return(NULL)
    }
  )
  
  # Save model if successful
  if (!is.null(model)) {
    saveRDS(model, output_path)
    message(glue("Saved gsynth model for {keyword}, {time_range}, {treatment}, {verification_method}, {force}, {estimator} to {output_path}"))
  }
  
  return(model)
}

# Generate parameter grid
param_grid <- expand.grid(
  keyword = keywords,
  time_range = time_ranges,
  treatment = treatment_options,
  verification_method = verification_options,
  force = force_options,
  estimator = estimator_options,
  stringsAsFactors = FALSE
)

# Set up parallel processing with furrr
plan(multisession, workers = parallel::detectCores() - 1)

# Apply grid search in parallel with furrr
results <- future_pmap(
  .l = list(
    param_grid$keyword,
    param_grid$time_range,
    param_grid$treatment,
    param_grid$verification_method,
    param_grid$force,
    param_grid$estimator
  ),
  .f = ~ run_gsynth(
    keyword = ..1,
    time_range = ..2,
    treatment = ..3,
    verification_method = ..4,
    force = ..5,
    estimator = ..6,
    output_dir = output_dir
  ),
  .progress = TRUE
)

# Reset plan to sequential after parallel execution
plan(sequential)

message("Gsynth models saved to: ", output_dir)

# Example of running a specific configuration for debugging
result <- run_gsynth(
  keyword = "pornhub",
  time_range = "2019-01-01 2024-10-31",
  treatment = "post_treat",
  verification_method = "government_id",
  force = "two-way",
  estimator = "ife",
  output_dir = output_dir
)
