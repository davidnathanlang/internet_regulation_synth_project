# Load required libraries
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

# Define keywords, time ranges, covariates, and model parameters
keywords <- c("pornhub", "xvideos", "vpn", "porn")
time_ranges <- c("2019-01-01 2024-10-31", "2022-01-01 2024-10-31", "2023-01-01 2024-10-31")
covariates <- c(
  "housesholds_with_internet_access", "high_school_or_ged", "bachelors_plus",
  "male", "household_income", "white", "black", "amerindian", "asian",
  "native_hawaiian", "two_or_more_races"
)
scm_options <- c(TRUE, FALSE)  # Include SCM or not
fixedeffects_options <- c(TRUE, FALSE)  # Include fixed effects or not
n_leads_options <- c(104, 52, 26, NULL)  # Number of leads
n_lags_options <- c(1, 3, 4, 13, NULL)  # Number of lags
treatment_options <- c("post_treat", "post_treat_passage", "post_treat_enforcement_date")

# Create output directory if it doesn't exist
output_dir <- here::here("multiverse_mod")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Function to generate file path for caching
get_output_path <- function(output_dir, keyword, time_range, include_covariates, scm, fixedeffects, n_leads, n_lags, treatment) {
  covariate_tag <- ifelse(include_covariates, "with_covariates", "without_covariates")
  here::here(glue("{output_dir}/{keyword}_{gsub(' ', '_', time_range)}_{covariate_tag}_scm{scm}_fixedeff{fixedeffects}_leads{n_leads}_lags{n_lags}_treatment_{treatment}.rds"))
}

# Function to run `multisynth` for a single combination of parameters
run_multisynth <- function(keyword, time_range, include_covariates, scm, fixedeffects, n_leads, n_lags, treatment, output_dir) {
  print(str_c(keyword, time_range, include_covariates, scm, fixedeffects, n_leads, n_lags, treatment, sep = "+"))
  
  # Check if model already exists
  output_path <- get_output_path(output_dir, keyword, time_range, include_covariates, scm, fixedeffects, n_leads, n_lags, treatment)
  if (file.exists(output_path)) {
    message(glue("Model already exists for '{keyword}', time range '{time_range}', covariates '{include_covariates}', scm '{scm}', fixedeffects '{fixedeffects}', n_leads '{n_leads}', n_lags '{n_lags}', treatment '{treatment}'. Skipping..."))
    return(readRDS(output_path))  # Load cached model
  }
  
  # Load and filter data
  keyword_df <- tryCatch(
    read_csv(here::here(glue("data/{keyword}.csv"))) %>%
      mutate(
        post_treat_passage = if_else(date >= passage_date, 1L, 0L, 0L),
        post_treat_enforcement_date = if_else(date >= enforcement_date, 1L, 0L, 0L)
      ) %>%
      filter(time_span == time_range) %>%
      distinct(state, date, .keep_all = TRUE),  # Ensure unique rows
    error = function(e) {
      message(glue("Error reading data for keyword {keyword}: {e$message}"))
      return(NULL)
    }
  )
  
  if (is.null(keyword_df) || nrow(keyword_df) == 0) {
    message(glue("No data available for keyword '{keyword}' and time range '{time_range}'"))
    return(NULL)
  }
  
  # Create auxiliary variable matrix if covariates are included
  aux_vars <- if (include_covariates) {
    keyword_df %>%
      select(all_of(covariates)) %>%
      as.matrix()
  } else {
    NULL
  }
  
  # Fit `multisynth` model
  model <- tryCatch(
    multisynth(
      as.formula(glue("hits ~ {treatment}")),
      unit = state,
      time = date,
      data = keyword_df,
      aux.var = aux_vars,
      scm = scm,  # SCM option
      fixedeff = fixedeffects,  # Fixed effects option
      n.leads = n_leads,  # Number of leads
      n.lags = n_lags  # Number of lags
    ),
    error = function(e) {
      message(glue("Error running multisynth for keyword '{keyword}', scm '{scm}', fixedeffects '{fixedeffects}', n_leads '{n_leads}', n_lags '{n_lags}', treatment '{treatment}': {e$message}"))
      return(NULL)
    }
  )
  
  if (!is.null(model)) {
    # Save the model to cache
    saveRDS(model, output_path)
    message(glue("Saved model for keyword '{keyword}', time range '{time_range}', covariates '{include_covariates}', scm '{scm}', fixedeffects '{fixedeffects}', n_leads '{n_leads}', n_lags '{n_lags}', treatment '{treatment}' to {output_path}"))
  }
  
  return(model)
}

# Prepare a grid of all combinations of parameters
param_grid <- expand.grid(
  keyword = keywords,
  time_range = time_ranges,
  include_covariates = c(TRUE, FALSE),
  scm = scm_options,
  fixedeffects = fixedeffects_options,
  n_leads = n_leads_options,
  n_lags = n_lags_options,
  treatment = treatment_options,
  stringsAsFactors = FALSE
)

# Set up parallel processing with furrr
plan(multisession, workers = parallel::detectCores() - 1)

# Apply grid search in parallel with caching
results <- param_grid %>%
  mutate(
    model = future_pmap(
      list(keyword, time_range, include_covariates, scm, fixedeffects, n_leads, n_lags, treatment),
      ~ run_multisynth(..1, ..2, ..3, ..4, ..5, ..6, ..7, ..8, output_dir),  # Pass parameters explicitly
      .progress = TRUE
    )
  )

# Shut down parallel workers
plan(sequential)

message("Multiverse models saved to: ", output_dir)

# param_grid
# plot(`porn_2022-01-01_2024-10-31_without_covariates_scmTRUE_fixedeffFALSE_leads26_lags13_treatment_post_treat_enforcement_date`)
# sm<-summary(`porn_2022-01-01_2024-10-31_without_covariates_scmTRUE_fixedeffFALSE_leads26_lags13_treatment_post_treat_enforcement_date`)
# summary(`porn_2022-01-01_2024-10-31_without_covariates_scmTRUE_fixedeffFALSE_leads26_lags13_treatment_post_treat_enforcement_date`,inf_type = 'jackknife')
# 
# 
# ?summary()
# ?multisynth()
# oof<-sm$att %>% filter(Time>=0,Level=='Average') 
# mean(oof$Estimate)
