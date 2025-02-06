# Errors are contained in the gsynth_errors folder
# 193 models won't run in the original 06_multiverse_gsynth.R code
# this causes downstream effects on the multiverse plots and the tables
# e.g. error_porn_2019-01-01_2024-10-31_treatment_post_treat_passage_verification_government_id_force_time_estimator_mc.txt
# Error in obs.missing[, id.tr] <- tr.pre + tr.post: number of items to replace is not a multiple of replacement length

# Load required libraries
library(gsynth)
library(dplyr)
library(readr)
library(glue)
library(here)
library(tidyr)
library(parallel)
library(pbmcapply)

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
error_dir <- here::here("gsynth_errors")
if (!dir.exists(output_dir)) dir.create(output_dir)
if (!dir.exists(error_dir)) dir.create(error_dir)

# Function to generate file path for caching
get_output_path <- function(output_dir, keyword, time_range, treatment, verification_method, force, estimator) {
  here::here(glue("{output_dir}/{keyword}_{gsub(' ', '_', time_range)}_treatment_{treatment}_verification_{verification_method}_force_{force}_estimator_{estimator}.rds"))
}

# Function to run gsynth for a single combination of parameters
run_gsynth <- function(params) {
  result <- list(
    success = FALSE,
    model = NULL,
    error = NULL,
    params = params
  )
  
  tryCatch({
    keyword <- params$keyword
    time_range <- params$time_range
    treatment <- params$treatment
    verification_method <- params$verification_method
    force <- params$force
    estimator <- params$estimator
    
    print(glue("Running gsynth for {keyword} + {time_range} + {treatment} + {verification_method} + {force} + {estimator}"))
    
    # Generate output path
    output_path <- get_output_path(output_dir, keyword, time_range, treatment, verification_method, force, estimator)
    
    # Check if model already exists
    if (file.exists(output_path)) {
      message(glue("Model already exists for {keyword}, {time_range}, {treatment}, {verification_method}, {force}, {estimator}. Loading..."))
      result$model <- readRDS(output_path)
      result$success <- TRUE
      return(result)
    }
    
    # Load and filter data
    keyword_df <- read_csv(here::here(glue("data/{keyword}.csv"))) %>%
      mutate(
        post_treat_passage = if_else(date >= passage_date, 1L, 0L, 0L),
        post_treat_enforcement_date = if_else(date >= enforcement_date, 1L, 0L,0L)
      ) %>%
      filter(
        (time_span==time_range)
      ) %>%
      filter(
        (treated_state == 0 | get(verification_method) )
      ) %>%
      distinct(state, date, .keep_all = TRUE)
    
    if (nrow(keyword_df) == 0) {
      result$error <- glue("No data available for {keyword} in {time_range}")
      return(result)
    }
    
    # Fit gsynth model
    model <- gsynth(
      formula = as.formula(glue("hits ~ {treatment}")),
      data = keyword_df,
      index = c("state", "date"),
      CV = TRUE,
      r = c(0, 5),
      force = force,
      estimator = estimator,
      inference = "parametric"
    )
    
    # Save model if successful
    saveRDS(model, output_path)
    message(glue("Saved gsynth model for {keyword}, {time_range}, {treatment}, {verification_method}, {force}, {estimator} to {output_path}"))
    
    result$model <- model
    result$success <- TRUE
    
  }, error = function(e) {
    result$error <- as.character(e)
    # Save error details to file
    error_file <- glue("{error_dir}/error_{keyword}_{gsub(' ', '_', time_range)}_treatment_{treatment}_verification_{verification_method}_force_{force}_estimator_{estimator}.txt")
    write(as.character(e), error_file)
  })
  
  return(result)
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

# Convert parameter grid to list of parameter sets
param_list <- split(param_grid, seq(nrow(param_grid)))

# Set up parallel processing using pbmcapply
num_cores <- parallel::detectCores() - 1
results <- pbmclapply(
  param_list,
  run_gsynth,
  mc.cores = num_cores,
  mc.style = "ETA"
)

# Process results
successful_runs <- sum(sapply(results, function(x) x$success))
failed_runs <- sum(sapply(results, function(x) !x$success))

# Generate summary report
summary_report <- data.frame(
  total_runs = length(results),
  successful_runs = successful_runs,
  failed_runs = failed_runs
)

# Save summary report
write_csv(summary_report, here::here(output_dir, "summary_report.csv"))

message(glue("
Processing complete:
Total runs: {summary_report$total_runs}
Successful: {summary_report$successful_runs}
Failed: {summary_report$failed_runs}
Models saved to: {output_dir}
Errors saved to: {error_dir}
"))

# Example of running a specific configuration for debugging
debug_params <- list(
  keyword = "pornhub",
  time_range = "2019-01-01 2024-10-31",
  treatment = "post_treat_passage",
  verification_method = "any_id_requirement",
  force = "none",
  estimator = "ife"
)

debug_result <- run_gsynth(debug_params)

df<-read_csv(here::here("data/pornhub.csv"))


df %>% distinct(state,any_id_requirement)
