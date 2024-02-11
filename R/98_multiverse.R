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

multiverse_run <- function(method,
                       as_fixedeff,
                       pretreat_start,
                       post_stop,
                       covariates,
                       outcome,
                       data = full_df,
                       estimator){
  
  in_function_data <- data %>%   
    filter(term == outcome) %>% # Vary this
    filter(date >= pretreat_start) %>%  # Vary this
    filter(date <= post_stop) # Vary this
  
  no_cov_formula = as.formula(hits ~ post_treat)
  
  cov_formula = as.formula(hits ~ post_treat +high_school_or_ged+bachelors_plus+male+household_income+white+black+amerindian+asian+native_hawaiian+two_or_more_races)
  
  in_function_data = in_function_data %>% select(post_treat, hits, state, date, high_school_or_ged, bachelors_plus, male, household_income, white, black, amerindian, asian, native_hawaiian, two_or_more_races)
  
  if(method == "multisynth"){
    
    mulisynth_return_object = list()
    
    if(covariates == T){
      
      multisynth_out = multisynth(form = cov_formula, # Vary this
                                  unit = state, 
                                  time = date, 
                                  data = in_function_data,
                                  fixedeff = as_fixedeff, # Vary this
                                  scm = T)
    }
    
    if(covariates == F){
      multisynth_out = multisynth(form = no_cov_formula, # Vary this
                                  unit = state, 
                                  time = date, 
                                  data = in_function_data,
                                  fixedeff = as_fixedeff, # Vary this
                                  scm = T)
    }
    
    mulisynth_return_object$multisynth_out = multisynth_out
    
    multisynth_summary_wild = summary(multisynth_out, inf_type = 'bootstrap') # wild bootstrap
    
    mulisynth_return_object$multisynth_summary_wild = multisynth_summary_wild
    
    multisynth_summary_jk = summary(multisynth_out, inf_type = 'jackknife') # jackknife boostrap
    
    mulisynth_return_object$multisynth_summary_jk = multisynth_summary_jk
    
    # "y0hat"Pilot estimates of control outcomes
    multisynth_summary_jk$y0hat
    
    convert_yhat = function(y0hat){
      
      treatment_times = names(multisynth_out$y0hat)
      
      df = multisynth_out$y0hat %>% 
        lapply(., t) %>% 
        lapply(., data.frame) %>% 
        lapply(., tibble::rownames_to_column, var = "time") %>% 
        lapply(., function(x) mutate(x, time = as.numeric(time)))
      
      for(i in 1:length(df)){
        df[[i]]$treatment_time = as.numeric(treatment_times[[i]])
        
        df[[i]]$treated = ifelse(df[[i]]$time >= df[[i]]$treatment_time, 1, 0)
        
      }
      
      df = lapply(df, function(x) select(x, time, treatment_time, treated, everything()))
      
      return(df)
      
    }
    
    converted_yhat = convert_yhat(multisynth_out$y0hat)
    
    mulisynth_return_object$ranking_results = lapply(seq_along(converted_yhat),function(x){
      
      pre_mspe = converted_yhat[[x]] %>% 
        filter(treated <= 0) %>% 
        select(contains("X")) %>% 
        apply(., 2, function(x) mean(x^2))
      
      post_mspe = converted_yhat[[x]] %>% 
        filter(treated > 0) %>% 
        select(contains("X")) %>% 
        apply(., 2, function(x) mean(x^2))
      
      average_diff = converted_yhat[[x]] %>% 
        select(contains("X")) %>% 
        apply(., 2, mean)
      
      last_period_diff = converted_yhat[[x]] %>% 
        # filter(treated > 0) %>% 
        select(contains("X")) %>% 
        apply(., 2, max)
      
      return(
        cbind(pre_mspe, post_mspe, average_diff, last_period_diff)
      )
      
    }) %>% 
      lapply(., data.frame) %>% 
      bind_rows() %>% 
      rownames_to_column(.,'state') %>% 
      mutate(state = rep(multisynth_out$data$units, 6)) %>% 
      group_by(state) %>% 
      summarize(pre_mspe = mean(pre_mspe), 
                post_mspe = mean(post_mspe),
                average_diff = mean(average_diff),
                last_period_diff = mean(last_period_diff))
    
    return(mulisynth_return_object)
    
  }
  
  if(method == 'gsynth'){
    permuting_gsynth = function(enforcement_date_, state_){
      
      if(est == "ife"){
        data %>% 
          mutate(post_treat = 0) %>% 
          mutate(post_treat = ifelse(test = state_ == state & date > enforcement_date_, yes = 1, no = 0)) %>% 
          gsynth(formula = hits ~ post_treat, data = ., index = c("state", "date"), # Vary this
                 parallel = F, se = T, inference = "nonparametric", estimator = est) -> model_obj # Vary this
      }
      
      if(est == "mc"){
        data %>% 
          mutate(post_treat = 0) %>% 
          mutate(post_treat = ifelse(test = state_ == state & date > enforcement_date_, yes = 1, no = 0)) %>% 
          gsynth(formula = hits ~ post_treat, data = ., index = c("state", "date"), # Vary this
                 parallel = F, se = T, inference = "nonparametric", estimator = est) -> model_obj # Vary this
      }
      
      out = tibble::rownames_to_column(data.frame(out$eff), var = "date")
      
      colnames(out)[2] = "eff"
      
      out$enforcement_date = enforcement_date_
      
      pre_mspe <- out %>% 
        mutate(date = as.Date(as.numeric(date), origin="1970-1-1")) %>% 
        mutate(Time = ifelse(enforcement_date <= date, 1, 0)) %>% 
        filter(Time <= 0) %>% 
        summarize(pre_mspe = mean(eff^2))
      
      post_mspe <- out %>% 
        mutate(date = as.Date(as.numeric(date), origin="1970-1-1")) %>% 
        mutate(Time = ifelse(enforcement_date <= date, 1, 0)) %>% 
        filter(Time > 0) %>% 
        summarize(post_mspe = mean(eff^2))
      
      average_diff = out %>%
        mutate(date = as.Date(as.numeric(date), origin="1970-1-1")) %>% 
        mutate(Time = ifelse(enforcement_date > date, 1, 0)) %>%
        filter(Time >= 0) %>%
        summarize(average_diff = mean(eff))
      
      last_period_diff = out %>%
        mutate(date = as.Date(as.numeric(date), origin="1970-1-1")) %>% 
        filter(date == max(date)) %>%
        select(last_period_diff = eff)
      
      out_cbind = cbind(state_, enforcement_date_, pre_mspe, post_mspe, average_diff, last_period_diff)
      
      print(out_cbind)
      
      return(list(
        gsynth_obj = model_obj, 
        gsynth_result = out_cbind))
      
    }
    
    gsynth_conds = expand.grid(
      ed = unique(data$enforcement_date), 
      s = unique(data$state), 
      stringsAsFactors = F
    ) %>% 
      drop_na()
    
    permuted_gsynth_out = mapply(FUN = permuting_gsynth, gsynth_conds$ed, gsynth_conds$s, SIMPLIFY = F)
    
    
    return(list(
      
      permuted_gsynth_out %>% 
        lapply(., `[[`, 1),
      
      permuted_gsynth_out %>% 
        lapply(., `[[`, 2) %>% 
        bind_rows() %>% 
        group_by(state_) %>% 
        summarize(pre_mspe = mean(pre_mspe), 
                  post_mspe = mean(post_mspe),
                  average_diff = mean(average_diff),
                  last_period_diff = mean(last_period_diff))))
    
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
