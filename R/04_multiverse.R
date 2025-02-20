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
gc()

# Define keywords, time ranges, covariates, and model parameters
keywords <- c("pornhub", "xvideos", "vpn", "porn")
time_ranges <- c("2019-01-01 2024-10-31","2022-01-01 2024-10-31")
covariates <- c(
  "housesholds_with_internet_access", "high_school_or_ged", "bachelors_plus",
  "male", "household_income", "white", "black", "amerindian", "asian",
  "native_hawaiian", "two_or_more_races"
)

scm_options <- c(T)  # Include SCM or not

fixedeffects_options <- c(T, F)  # Include fixed effects or not

n_leads_options <- c(104, 52, 26,# weekly
                     24,12,6, # monthly
                     NULL)  # Number of leads

n_lags_options <- c(13,4, # weekly
                    3,1, #monthly
                    NULL)  # Number of lags

treatment_options <- c("post_treat", "post_treat_passage", "post_treat_enforcement_date")

verification_options<-c('government_id','digitized_id','transaction_data','database_id','any_id_requirement')

treatment<-'post_treat'

verification_method<-'government_id'

# Create output directory if it doesn't exist

output_dir <- here::here("multiverse_mod")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Function to generate file path for caching
get_output_path <- function(output_dir, keyword, time_range, include_covariates, scm, fixedeffects, n_leads, n_lags, treatment,verification_method) {
  covariate_tag <- ifelse(include_covariates, "with_covariates", "without_covariates")
  here::here(glue("{output_dir}/{keyword}_{gsub(' ', '_', time_range)}_{covariate_tag}_scm{scm}_fixedeff{fixedeffects}_leads{n_leads}_lags{n_lags}_treatment_{treatment}_verification_method_{verification_method}.rds"))
}

# Function to run `multisynth` for a single combination of parameters
run_multisynth <- function(keyword, time_range, include_covariates, scm, fixedeffects, n_leads, n_lags, treatment,verification_method, output_dir) {
  print(str_c(keyword, time_range, include_covariates, scm, fixedeffects, n_leads, n_lags, treatment,verification_method, sep = "+"))
  
  # Check if model already exists
  output_path <- get_output_path(output_dir, keyword, time_range, include_covariates, scm, fixedeffects, n_leads, n_lags, treatment,verification_method)
  if (file.exists(output_path)) {
    message(glue("Model already exists for '{keyword}', time range '{time_range}', covariates '{include_covariates}', scm '{scm}', fixedeffects '{fixedeffects}', n_leads '{n_leads}', n_lags '{n_lags}', treatment '{treatment}',verification_method '{verification_method}'. Skipping..."))
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
      filter(treated_state==0 | get(verification_method)==1) %>%
      distinct(state, date, .keep_all = TRUE),  # Ensure unique rows
    error = function(e) {
      message(glue("Error reading data for keyword {keyword}: {e$message}"))
      return(NULL)
    }
  )

  if (is.null(keyword_df) || nrow(keyword_df) == 0) {
    print(output_path)
    message(
      glue("No data available for keyword '{keyword}' and time range '{time_range}'
           \n
           ")
      
            )
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
      message(glue("Error running multisynth for keyword '{keyword}', scm '{scm}', fixedeffects '{fixedeffects}', n_leads '{n_leads}', n_lags '{n_lags}', treatment '{treatment}', verification_method '{verification_method}'.: {e$message}"))
      return(NULL)
    }
  )
  
  if (!is.null(model)) {
    # Save the model to cache
    saveRDS(model, output_path)
    message(glue("Saved model for keyword '{keyword}', time range '{time_range}', covariates '{include_covariates}', scm '{scm}', fixedeffects '{fixedeffects}', n_leads '{n_leads}', n_lags '{n_lags}', treatment '{treatment}', verification_method '{verification_method}'. to {output_path}"))
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
  verification_method = verification_options,
  stringsAsFactors = FALSE
)  %>% filter(
  
(
  (time_range=='2022-01-01 2024-10-31' &
   (n_leads %in% c(104,52,26)|is.null(n_leads)) &
  n_lags %in% c(4,13))|is.null(n_lags) 
) |
  (
  (time_range=='2019-01-01 2024-10-31' &
     (n_leads %in% c(24,12,6)|is.null(n_leads)) &
     n_lags %in% c(1,3))|is.null(n_lags)
  )
  
  )


# Set up parallel processing with furrr
plan(multisession, workers = parallel::detectCores() - 1)

# Apply grid search in parallel with caching
safe_run_multisynth <- safely(run_multisynth)
pwalk(
  list(
    param_grid$keyword,
    param_grid$time_range,
    param_grid$include_covariates,
    param_grid$scm,
    param_grid$fixedeffects,
    param_grid$n_leads,
    param_grid$n_lags,
    param_grid$treatment,
    param_grid$verification_method
  ),
  ~ safe_run_multisynth(..1, ..2, ..3, ..4, ..5, ..6, ..7, ..8, ..9,output_dir),  # Pass parameters explicitly
  .progress = TRUE 
) # This may hang with progress @ 100%, confirm 3457 files in the folder before terminating...

example_param_grid <- param_grid %>% filter(row_number()==3L) 
example_param_grid <- param_grid %>% filter(verification_method=='any_reasonable_method',keyword=='pornhub',treatment=='post_treat_enforcement_date',n_leads==104L)
example_param_grid
results<-
pwalk(
  list(
    example_param_grid$keyword,
    example_param_grid$time_range,
    example_param_grid$include_covariates,
    example_param_grid$scm,
    example_param_grid$fixedeffects,
    example_param_grid$n_leads,
    example_param_grid$n_lags,
    example_param_grid$treatment,
    example_param_grid$verification_method
  ),
  ~ safe_run_multisynth(..1, ..2, ..3, ..4, ..5, ..6, ..7, ..8, ..9,output_dir),  # Pass parameters explicitly
  .progress = TRUE
)

# Shut down parallel workers

plan(sequential)

message("Multiverse models saved to: ", output_dir)
list.files(here::here('multiverse_mod'))
