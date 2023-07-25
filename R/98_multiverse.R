library(tidyverse)
library(lubridate)
library(augsynth)
library(furrr)

# set multiverse parameters -----------------------------------------------

data_params <- list(

  treated_state = list(
    UT = "US-UT",
    TX = "US-TX",
    VA = "US-VA",
    LA = 'US-LA',
    MS = "US-MS",
    AR = "US-AR",
    MT = "US-MT"
  ),

  states_to_include = list(
    full = sort(unique(analysis_data$state))
  ),

  pretreat_start = list(
    `2019` = "2019-01-01",
    `2022` = "2022-01-01" # 1 month
  ),

  post_stop = list(
    one_month   = "2023-10-01", # Going off the last one to be implemented, 2023-09-01 + 1
    three_month = "2023-12-01"  # 2023-09-01 + 3
  ),

  # Modelling Parameters
  covariates = list(
    none = "NULL",

    covariates = c(
      "black",
      "asian",
      "two_or_more_races",
      "white",
      "male",
      "amerindian",
      "native_hawaiian",
      "high_school_or_ged",
      "bachelors_plus",
      "housesholds_with_internet_access",
      "household_income"
    )

  ),

  outcome = list(
    vpn = "vpn",
    VPN = "VPN",
    pornhub = "pornhub",
    xvideos = "xvideos",
    porn = "porn"
  )
)

data_spec <-
  crossing(
    treated_state = data_params$treated_state,
    states_to_include = data_params$states_to_include,
    pretreat_start = data_params$pretreat_start,
    post_stop = data_params$post_stop,
    covariates = data_params$covariates,
    outcome = data_params$outcome)

model_spec <- bind_rows(

  expand.grid(
    method = "multisynth",
    fixeff = c(T,F)
  ),

  expand.grid(
    method = "gsynth",
    estimator = c("ife","mc")
  )

)

multiverse_spec <-
  crossing(model_spec, data_spec) %>%
  mutate(model_num = as.character(row_number())) %>%
  as.data.frame()

# define function for multiverse of models ------------------------------------

multiverse_run <- function(model_num,
                           method,
                           as_fixedeff,
                           gs_estimator,
                           treated_state,
                           states_to_include,
                           pretreat_start,
                           post_stop,
                           covariates,
                           outcome,
                           data,
                           output = "output-multiverse",
                           verbose = F) {

  if(verbose) {
    print(paste("model_num:", model_num))
    print(paste("method:", method))
    print(paste("method_opts:", as_fixedeff))
    print(paste("states_to_include, count:", length(states_to_include)))
    print(paste("pretreat_start:", pretreat_start))
    print(paste("post_stop:", post_stop))
    print(paste("covariates:", covariates))
    print(paste("outcome:", outcome))
    print("--------------------------")
  }

  # Treatment window and data collapse to week
  data <- data %>% filter(
    date >= !!pretreat_start,
    date <= !!post_stop,
    state %in% !!states_to_include
  )

  data$outcome = data[[outcome]]

  if(method == "multisynth") {
    # Covariate inclusion
    formula = paste0(paste0(outcome, collapse = "+"), " ~ treat")
    if(covariates[[1]] != "NULL") {
      formula = paste(formula, paste0(covariates, collapse = " + "), sep = "|")
    }

    # Permute the multisynth
    names(states_to_include) <- states_to_include
    wrap_multisynth <- function(permuted_state) {
      data <- data %>%
        mutate(treat = state == !!permuted_state & relative_time >= 0)

      multisynth(as.formula(formula),
                 # Fixed params
                 unit = state, time = relative_time, data = data,
                 t_int = 0,
                 # variable params
                 fixedeff = as_fixedeff,
                 scm = T)
    }

    multisynth_out <- states_to_include %>%
      map(quietly(wrap_multisynth))

    # multisynth_out structured as:
    # state, type, pre_mspe, post_mspe, mspe_ratio, mspe_rank,
    # fischers_exact_pvalue, z_score, average_difference, average_rank,
    # last_period_diff, last_period_rank, weight

    weights_ <- lapply(multisynth_out, `[[`, 1) %>%
      lapply(., function(x) x$weights) %>%
      lapply(., data.frame) %>%
      lapply(., rownames_to_column, "state") %>%
      purrr::reduce(., dplyr::left_join, by = 'state') %>%
      `colnames<-`(c("state", paste(unlist(data_params$states_to_include), "weight", sep = "-"))) %>%
      select(state, contains(treated_state)) # only care about true treated state weights

    as_sum <- lapply(multisynth_out, `[[`, 1) %>%
      map(summary)

    get_multisynth_metrics <- function(as_sum) {

      pre_mspe = as_sum$att %>%
        filter(Time < 0, Level != "Average") %>%
        summarize(pre_mspe = mean(Estimate^2, na.rm = T))

      post_mspe = as_sum$att %>%
        filter(Time >= 0, Level != "Average") %>%
        summarize(post_mspe = mean(Estimate^2, na.rm = T))

      average_diff = as_sum$att %>%
        filter(Time >= 0, Level != "Average") %>%
        summarize(average_diff = mean(Estimate, na.rm = T))

      last_period_diff = as_sum$att %>%
        filter(Time == max(Time, na.rm = T), Level != "Average") %>%
        select(last_period_diff = Estimate)

      cbind(pre_mspe, post_mspe, average_diff, last_period_diff)
    }

    as_results <- as_sum %>%
      lapply(., get_multisynth_metrics) %>%
      bind_rows()

    as_results$state = names(as_sum)

    as_results <- as_results %>%
      mutate(mspe_ratio = post_mspe/pre_mspe,
             mspe_rank = rank(-mspe_ratio),
             average_rank = rank(average_diff),
             last_period_rank = rank(last_period_diff),
             type = ifelse(state == !!treated_state, "Treated", "Donor"),
             fishers_exact_pvalue = mspe_rank / n()) %>%
      left_join(weights_, by = "state") %>%
      mutate(model_num = model_num)

    # write the full multisynth output to disk
    saveRDS(multisynth_out, paste0(output, "/model_", model_num, ".rds"))
    # function returns selected metrics in memory
    return(as_results)
  }

  if(method == 'gsynth'){
    # Covariate inclusion
    formula = paste0(paste0(outcome, collapse = "+"), " ~ treat")
    if(covariates[[1]] != "NULL") {
      formula = paste(formula, paste0(covariates, collapse = " + "), sep = "+")
    }

    # Permute the multisynth
    names(states_to_include) <- states_to_include
    wrap_gsynth <- function(permuted_state) {
      data <- data %>%
        mutate(treat = state == !!permuted_state & relative_time >= 0)

      gsynth(as.formula(formula),
             # Fixed params
             index = c("state", "relative_time"),
             data = data,
             t_int = 0,
             # variable params
             estimator = gs_estimator,
             scm = T)
    }

    gsynth_out <- states_to_include %>%
      map(quietly(wrap_gsynth))

    weights_ <- gsynth_out %>%
      lapply(., function(x) tibble::rownames_to_column(data.frame(x$wgt.implied), 'state')) %>%
      select(contains(treated_state))

    gs_sum <- lapply(gsynth_out, `[[`, 1)

    get_multisynth_metrics <- function(gs_sum) {

      eff = gs_sum$eff %>% data.frame()

      pre_mspe = eff %>%
        tibble::rownames_to_column('Time') %>%
        filter(Time < 0) %>%
        summarize(pre_mspe = mean(Estimate^2, na.rm = T))

      post_mspe = as_sum$att %>%
        filter(Time >= 0) %>%
        summarize(post_mspe = mean(Estimate^2, na.rm = T))

      average_diff = as_sum$att %>%
        filter(Time >= 0) %>%
        summarize(average_diff = mean(Estimate, na.rm = T))

      last_period_diff = as_sum$att %>%
        filter(Time == max(Time, na.rm = T)) %>%
        select(last_period_diff = Estimate)

      cbind(pre_mspe, post_mspe, average_diff, last_period_diff)
    }

    gs_results <- gs_sum %>%
      lapply(., get_gsynth_metrics) %>%
      bind_rows()

    gs_results$state = names(gs_sum)

    gs_results <- gs_results %>%
      mutate(mspe_ratio = post_mspe/pre_mspe,
             mspe_rank = rank(-mspe_ratio),
             average_rank = rank(average_diff),
             last_period_rank = rank(last_period_diff),
             type = ifelse(state == !!treated_state, "Treated", "Donor"),
             fishers_exact_pvalue = mspe_rank / n()) %>%
      left_join(weights_, by = "state") %>%
      mutate(model_num = model_num)

    # write the full multisynth output to disk
    saveRDS(multisynth_out, paste0(output, "/model_", model_num, ".rds"))
    # function returns selected metrics in memory
    return(as_results)
  }

}


# Run Multiverse Models ---------------------------------------------------

# use furrr package to leverage multiple cores, may need to
plan(multisession, workers = availableCores())
set.seed(608134)

multiverse_output <- multiverse_spec %>%
  future_pmap(., possibly(multiverse_run, otherwise = "error"), data = analysis_data,
              .options = furrr_options(seed = TRUE),
              .progress = T)

names(multiverse_output) <- multiverse_spec$model_num

# Note on output: weight column captures each state's weight in the OH (true)
# synthetic control model. All other columns hold the specific states output from
# the permutation test

saveRDS(multiverse_output, "output-multiverse/multiverse_output.RDS")

saveRDS(multiverse_spec, "output-multiverse/multiverse_spec.RDS")
