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
pacman::p_load(patchwork, gghighlight, ggrepel,kableExtra)

# Function to get models and effects for a given keyword
get_mod_and_effects <- function(keyword) {
  # Load and preprocess data
  keyword_df_long <- bind_rows(
    read_csv(here(str_glue("data/pornhub.csv"))),
    read_csv(here(str_glue("data/xvideos.csv"))),
    read_csv(here(str_glue("data/porn.csv"))),
    read_csv(here(str_glue("data/vpn.csv")))
  ) 
  keyword_df <-
    pivot_wider(names_from = term, values_from = hits) %>%
    filter(time_span == "2022-01-01 2024-10-31") %>%
    distinct() %>%
    filter(date < make_date(2023, 3, 1))
  
  # Define models for different datasets
  mod_pornhub <- augsynth(
    pornhub ~ post_treat,
    unit = state,
    time = date,
    data = keyword_df,
    progfunc = "None",
    scm = FALSE,
    fixedeff = FALSE
  )
  
  mod_xvideos <- augsynth(
    xvideos ~ post_treat,
    unit = state,
    time = date,
    data = keyword_df,
    progfunc = "None",
    scm = FALSE,
    fixedeff = FALSE
  )
  
  mod_vpn <- augsynth(
    vpn ~ post_treat,
    unit = state,
    time = date,
    data = keyword_df,
    progfunc = "None",
    scm = FALSE,
    fixedeff = FALSE
  )
  
  mod_porn <- augsynth(
    porn ~ post_treat,
    unit = state,
    time = date,
    data = keyword_df,
    progfunc = "None",
    scm = FALSE,
    fixedeff = FALSE
  )
  
  # Plot results
  (plot(mod_pornhub) + plot(mod_xvideos)) / (plot(mod_vpn) + plot(mod_porn))
}
combined_weights <- enframe(mod_pornhub$weights, value = 'pornhub', name = 'state') %>%
  left_join(enframe(mod_xvideos$weights, value = 'xvideos', name = 'state'), by = 'state') %>%
  left_join(enframe(mod_vpn$weights, value = 'vpn', name = 'state'), by = 'state') %>%
  left_join(enframe(mod_porn$weights, value = 'porn', name = 'state'), by = 'state') %>% mutate(across(where(is.numeric),round,digits=3))
combined_weights

combined_weights %>% select(-state) %>% colSums()
latex_table <- combined_weights %>% filter(if_any(where(is.numeric),~.>.001)) %>%
  kbl(caption = "Combined Weights for Models", format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

keyword_df_long %>% filter(time=='2022-01-01 2024-10-31') %>% count(term,state) ->oof
keyword_df_long %>% ungroup() %>% filter(time_span=='2022-01-01 2024-10-31') %>% #filter(term=='pornhub') %>%
  filter(date<make_date(2023,04,01))  %>%
  ggplot(aes(date,y=hits,group = state))+
  geom_line() +
  gghighlight(state=="LA",calculate_per_facet = TRUE)+
  facet_wrap(~term) +
  geom_vline(xintercept = make_date(2023,01,01))

keyword_df_long %>% filter(hits==0) %>% count(term)
keyword_df
permutation_test <- function(s) {
  # Takes in a state abbreviation  and runs an augsynth estimate with that state
  
  permute.panel <- keyword_df%>% mutate(permute_post = if_else(state == s & date >= make_date(2023,01,01), 1L, 0L))
  permute.est <- augsynth(pornhub ~ permute_post,
                          unit = state, time = date,
                          data = permute.panel, progfun = "None", fixedeff = FALSE
  )
  cbind(summary(permute.est)$att, state = s)
}
donor_states_list <- unique(keyword_df$state)
permute_aug_synth <- donor_states_list %>% map_dfr(permutation_test)

permute_aug_synth
pre_mspe <- permute_aug_synth %>% 
  group_by(state) %>% 
  filter(make_date(2023,01,01)>Time) %>% 
  summarise(premspe=sqrt(mean(Estimate^2)))

post_mspe <- permute_aug_synth %>% 
  group_by(state) %>% 
  filter(make_date(2023,01,01)<Time) %>% 
  summarise(postmspe=sqrt(mean(Estimate^2)), 
            average_diff=mean(Estimate)) %>%
  arrange(desc(average_diff)) %>% 
  mutate(average_rank=row_number())



inference_tbl <- post_mspe %>%
  left_join(pre_mspe, by = "state") %>%
  #left_join(last_period, by = "state") %>%
  mutate(mspe_ratio = (postmspe / premspe)^2) %>%
  arrange(desc(mspe_ratio)) %>%
  mutate(mspe_rank=row_number())

inference_tbl
inference_tbl %>% filter(state=="OH")

# Generate models and plots
m1 <- get_mod_and_effects("pornhub")
m2 <- get_mod_and_effects("vpn")
m3 <- get_mod_and_effects("xvideos")
m4 <- get_mod_and_effects("porn")

# Combined plots for ATT
plot(m1$mod, main = "Pornhub Search Volume (ATT)")
plot(m2$mod, main = "VPN Search Volume (ATT)")
plot(m3$mod, main = "Xvideos Search Volume (ATT)")
plot(m4$mod, main = "Porn Search Volume (ATT)")

# Visualizing cumulative effects
m1$cum_effects
m2$cum_effects
m3$cum_effects
m4$cum_effects

# Additional data visualization
full_df %>%
  filter(keyword == "xvideos", time == "2022-01-01 2024-06-09") %>%
  ggplot(aes(x = date, y = hits, color = state)) +
  geom_vline(aes(xintercept = enforcement_date)) +
  facet_wrap(~state) +
  geom_line() +
  theme(legend.position = 'none')
