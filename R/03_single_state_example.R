# Load necessary libraries
library(dplyr)
library(readr)
library(here)
library(glue)
library(gsynth)
library(tidyverse)
library(tidylog)
library(augsynth)
library(pacman)
library(janitor)
library(xtable)
pacman::p_load(patchwork, gghighlight, ggrepel,kableExtra)

if (!file.exists("tables")) {
  # Create the "tables" folder if it doesn't exist
  dir.create("tables")
  cat("The 'tables' folder has been created.\n")
} else {
  cat("The 'tables' folder already exists.\n")
}

if (!file.exists("model_weights")) {
  # Create the "tables" folder if it doesn't exist
  dir.create("model_weights")
  cat("The 'model_weights' folder has been created.\n")
} else {
  cat("The 'model_weights' folder already exists.\n")
}

# Function to get models and effects for a given keyword
#get_mod_and_effects <- function(keyword) {
# Load and preprocess data

library(tidyverse)
library(here)
library(augsynth)
library(gsynth)
library(knitr)
library(kableExtra)
library(xtable)
library(ggplot2)
library(gghighlight)
library(patchwork)

# Load and combine datasets
keyword_df_long <- bind_rows(
  read_csv(here("data/pornhub.csv")),
  read_csv(here("data/xvideos.csv")),
  read_csv(here("data/porn.csv")),
  read_csv(here("data/vpn.csv"))
)

# Count occurrences by time span
keyword_df_long %>% count(time_span)

# Process data
keyword_df <- keyword_df_long %>%
  pivot_wider(names_from = term, values_from = hits) %>%
  filter(time_span == "2022-01-01 2023-10-01") %>%
  distinct() %>%
  filter(date < make_date(2023, 4, 1))

# Count states in post-treatment period
keyword_df %>% filter(post_treat == 1) %>% count(state)

# Define models for different datasets
mod_pornhub <- augsynth(pornhub ~ post_treat, unit = state, time = date, data = keyword_df, progfunc = "None", scm = TRUE, fixedeff = FALSE)
mod_xvideos <- augsynth(xvideos ~ post_treat, unit = state, time = date, data = keyword_df, progfunc = "None", scm = TRUE, fixedeff = FALSE)
mod_vpn <- augsynth(vpn ~ post_treat, unit = state, time = date, data = keyword_df, progfunc = "None", scm = TRUE, fixedeff = FALSE)
mod_porn <- augsynth(porn ~ post_treat, unit = state, time = date, data = keyword_df, progfunc = "None", scm = TRUE, fixedeff = FALSE)

# Summarize models
m1 <- summary(mod_pornhub, inf_type = 'jackknife')
m2 <- summary(mod_xvideos, inf_type = 'jackknife')
m3 <- summary(mod_vpn, inf_type = 'jackknife')
m4 <- summary(mod_porn, inf_type = 'jackknife')

# Extract conformal inference p-values
m1_conf <- summary(mod_pornhub, inf_type = 'conformal')$average_att$p_val
m2_conf <- summary(mod_xvideos, inf_type = 'conformal')$average_att$p_val
m3_conf <- summary(mod_vpn, inf_type = 'conformal')$average_att$p_val
m4_conf <- summary(mod_porn, inf_type = 'conformal')$average_att$p_val

# GSynth models
gsynth_mod1 <- gsynth(pornhub ~ post_treat, data = keyword_df, index = c("state", "int_date"), se = TRUE, seed = 12345, force = 'none', inference = 'parametric')
gsynth_mod2 <- gsynth(xvideos ~ post_treat, data = keyword_df, index = c("state", "int_date"), se = TRUE, seed = 12345, force = 'none', inference = 'parametric')
gsynth_mod3 <- gsynth(vpn ~ post_treat, data = keyword_df, index = c("state", "int_date"), se = TRUE, seed = 12345, force = 'none', inference = 'parametric')
gsynth_mod4 <- gsynth(porn ~ post_treat, data = keyword_df, index = c("state", "int_date"), se = TRUE, seed = 12345, force = 'none', inference = 'parametric')

# Extract GSynth ATT estimates
extract_gsynth_att <- function(gsynth_model) {
  if (is.null(gsynth_model$est.avg)) {
    att_avg <- gsynth_model$att.avg
    att_period <- gsynth_model$att
    message("Uncertainty estimates not available in this gsynth object.")
  } else {
    att_avg <- gsynth_model$est.avg
    att_period <- gsynth_model$est.att
  }
  
  se <- gsynth_model$se %||% NA
  ci_lower <- gsynth_model$ci[, 1] %||% NA
  ci_upper <- gsynth_model$ci[, 2] %||% NA
  p_value <- gsynth_model$p.value %||% NA
  
  data.frame(Estimate = att_avg, SE = se, CI_Lower = ci_lower, CI_Upper = ci_upper, P_Value = p_value)
}

# Combine results
results <- bind_rows(
  bind_cols(m1$average_att %>% mutate(search_term = 'pornhub'), m1_conf, extract_gsynth_att(gsynth_mod1)),
  bind_cols(m2$average_att %>% mutate(search_term = 'xvideos'), m2_conf, extract_gsynth_att(gsynth_mod2)),
  bind_cols(m3$average_att %>% mutate(search_term = 'vpn'), m3_conf, extract_gsynth_att(gsynth_mod3)),
  bind_cols(m4$average_att %>% mutate(search_term = 'porn'), m4_conf, extract_gsynth_att(gsynth_mod4))
) %>% select(
  search_term, 
  SCM_Estimate = Estimate, SCM_SE = Std.Error, SCM_P_Value = ...4, 
  Gsynth_Estimate = Estimate.Estimate, GSYNTH_SE = Estimate.S.E., 
  GSYNTH_CI_LOW = Estimate.CI.lower, GSYNTH_CI_UPPER = Estimate.CI.upper, 
  GSYNTH_p_value = Estimate.p.value
) %>% as_tibble()

# Generate LaTeX table
latex_table <- xtable(results, caption = "Search terms with their estimates and standard errors.")
write_lines(latex_table, here("tables/single_synth_att.tex"))

# Plot results
(plot(mod_pornhub) + ggtitle("pornhub") + 
    plot(mod_xvideos) + ggtitle("xvideos")) / 
  (plot(mod_vpn) + ggtitle("vpn") + 
     plot(mod_porn) + ggtitle("porn")) + 
  labs(title = "Google Trends") + 
  plot_annotation(title = 'Change in Google Trends Relative to Synthetic Control')

ggsave(here("figures/single_state_example.png"), width = 7, height = 7)

# Compute combined weights
combined_weights <- enframe(mod_pornhub$weights, value = 'pornhub', name = 'state') %>%
  left_join(enframe(mod_xvideos$weights, value = 'xvideos', name = 'state'), by = 'state') %>%
  left_join(enframe(mod_vpn$weights, value = 'vpn', name = 'state'), by = 'state') %>%
  left_join(enframe(mod_porn$weights, value = 'porn', name = 'state'), by = 'state') %>%
  mutate(across(where(is.numeric), round, digits = 3))

# GSynth weights
gsynth_weights <- gsynth_mod1$wgt.implied %>% enframe(value = 'pornhub') %>%
  left_join(gsynth_mod2$wgt.implied %>% enframe(value = 'xvideos')) %>%
  left_join(gsynth_mod3$wgt.implied %>% enframe(value = 'vpn')) %>%
  left_join(gsynth_mod4$wgt.implied %>% enframe(value = 'porn')) %>%
  rename(state = name)

# Merge SCM and GSynth weights
both_weights <- combined_weights %>% 
  left_join(gsynth_weights, by = 'state') %>% 
  mutate_if(is.numeric, ~ round(., digits = 3))

names(both_weights) <- c("state", "pornhub_scm", "xvideos_scm", "vpn_scm", "porn_scm",
                         "pornhub_gsynth", "xvideos_gsynth", "vpn_gsynth", "porn_gsynth")

both_weights <- both_weights %>% 
  select(state, contains("pornhub"), contains("xvideos"), contains("vpn"), contains("porn"))

# Generate LaTeX table for weights
latex_table <- kbl(both_weights,
                   caption = "Combined Weights for Models", format = "latex", booktabs = TRUE) %>% 
  kable_styling(latex_options = c("hold_position"))

write_lines(latex_table, here("model_weights/single_synth_example.tex"))

latex_table


# Function to perform permutation test for a given state
run_permutation_test <- function(search_term) {
  
  permutation_test <- function(s) {
    # Create a permuted panel with modified post-treatment indicator
    permute_panel <- keyword_df %>%
      mutate(permute_post = if_else(state == s & date >= make_date(2023, 1, 1), 1L, 0L))
    
    # Run Augsynth model dynamically using the search term
    formula <- as.formula(paste0(search_term, " ~ permute_post"))
    permute_est <- augsynth(
      form = formula,
      unit = state,
      time = date,
      data = permute_panel,
      progfunc = "None",
      fixedeff = FALSE,
      scm = TRUE
    )
    
    # Extract and return ATT estimates with state identifier
    cbind(summary(permute_est)$att, state = s, search_term = search_term)
  }
  
  # Run permutation tests for all donor states
  donor_states_list <- unique(keyword_df$state) 
  permute_aug_synth <- map_dfr(donor_states_list, permutation_test)
  
  # Compute pre-treatment MSPE
  pre_mspe <- permute_aug_synth %>%
    group_by(state) %>%
    filter(make_date(2023, 1, 1) > Time) %>%
    summarise(premspe = sqrt(mean(Estimate^2)))
  
  # Compute post-treatment MSPE and estimate differences
  post_mspe <- permute_aug_synth %>%
    group_by(state) %>%
    filter(make_date(2023, 1, 1) < Time) %>%
    summarise(
      postmspe = sqrt(mean(Estimate^2)),
      average_diff = mean(Estimate)
    ) %>%
    arrange(desc(average_diff)) %>%
    mutate(average_rank = row_number())
  
  # Compute inference table with MSPE ratio and ranking
  inference_tbl <- post_mspe %>%
    left_join(pre_mspe, by = "state") %>%
    mutate(
      mspe_ratio = (postmspe / premspe)^2,
      mspe_rank = row_number()
    ) %>%
    arrange(desc(mspe_ratio))
  
  # Plot results
  p1<-inference_tbl %>%
    ggplot(aes(y = fct_reorder(state, mspe_ratio), x = mspe_ratio)) +
    geom_col() +
    gghighlight(state == "LA", label_key = state) +
    labs(title = paste("MSPE Ratio by State for", search_term), 
         y = "State", 
         x = "MSPE Ratio") +
    theme_minimal()
  
  p1
  ggsave(here::here(str_glue("figures/MSPE_RATIO_{search_term}.png")))
  
  return(list(inf_tbl=inference_tbl,plt=p1))
}

# Example: Run the function for "pornhub"
inference_pornhub <- run_permutation_test("pornhub")
inference_porn <- run_permutation_test("porn")
inference_xvideos <- run_permutation_test("xvideos")
inference_vpn <- run_permutation_test("vpn")



p2<-
  (inference_pornhub$plt +inference_xvideos$plt)/
(inference_vpn$plt + inference_porn$plt)


 ggsave(here::here("figures/MSPE_Panel.png"), plot = p2, height = 20, width = 10)
 

              