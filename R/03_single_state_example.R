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
pacman::p_load(patchwork, gghighlight, ggrepel,kableExtra)

# Function to get models and effects for a given keyword
#get_mod_and_effects <- function(keyword) {
  # Load and preprocess data
  keyword_df_long <- bind_rows(
    read_csv(here(str_glue("data/pornhub.csv"))),
    read_csv(here(str_glue("data/xvideos.csv"))),
    read_csv(here(str_glue("data/porn.csv"))),
    read_csv(here(str_glue("data/vpn.csv")))
  ) 
  keyword_df_long %>% count(time_span)
  keyword_df <-
    keyword_df_long %>%
    pivot_wider(names_from = term, values_from = hits) %>%
    filter(time_span == "2022-01-01 2023-10-01") %>%
    distinct() %>%
    filter(date < make_date(2023, 4, 1))
  keyword_df %>% filter(post_treat==1) %>% count(state)

  # Define models for different datasets
  mod_pornhub <- augsynth(
    pornhub ~ post_treat,
    unit = state,
    time = date,
    data = keyword_df,
    progfunc = "None",
    scm = TRUE,
    fixedeff = FALSE
  )
  
  
  mod_xvideos <- augsynth(
    xvideos ~ post_treat,
    unit = state,
    time = date,
    data = keyword_df,
    progfunc = "None",
    scm = TRUE,
    fixedeff = FALSE
  )
  
  mod_vpn <- augsynth(
    vpn ~ post_treat,
    unit = state,
    time = date,
    data = keyword_df,
    progfunc = "None",
    scm = TRUE,
    fixedeff = FALSE
  )
  
  mod_porn <- augsynth(
    porn ~ post_treat,
    unit = state,
    time = date,
    data = keyword_df,
    progfunc = "None",
    scm = TRUE,
    fixedeff = FALSE
  )

  m1<-summary(mod_pornhub,inf_type = 'jackknife') 
  m2<-summary(mod_xvideos,inf_type = 'jackknife')
  m3<-summary(mod_vpn,inf_type = 'jackknife')
  m4<-summary(mod_porn,inf_type = 'jackknife')
  
  results<-
  bind_rows(
  m1$average_att %>% mutate(search_term='pornhub'),
  m2$average_att %>% mutate(search_term='xvideos'),
  m3$average_att %>% mutate(search_term='vpn'),
  m4$average_att %>% mutate(search_term='porn'),
) %>% select(search_term,Estimate,Std.Error)
  library(xtable)
  latex_table <- xtable(results, caption = "Search terms with their estimates and standard errors.")
  latex_table %>% write_lines(here::here('tables/single_synth_att.tex'))
  
  
  # Plot results
  (plot(mod_pornhub)+ggtitle("pornhub") + plot(mod_xvideos)+ggtitle("xvideos")) / (plot(mod_vpn)+ggtitle("vpn") + plot(mod_porn)+ggtitle("porn")) +
    labs(title="Google Trends") +
    plot_annotation(title='Change in Google Trends Relative to Synthetic Control')
  ggsave(here::here("figures/single_state_example.png"),width = 7,height = 7)
} 
keyword_df %>% count(state) ->temp
combined_weights <- enframe(mod_pornhub$weights, value = 'pornhub', name = 'state') %>%
  left_join(enframe(mod_xvideos$weights, value = 'xvideos', name = 'state'), by = 'state') %>%
  left_join(enframe(mod_vpn$weights, value = 'vpn', name = 'state'), by = 'state') %>%
  left_join(enframe(mod_porn$weights, value = 'porn', name = 'state'), by = 'state') %>% mutate(across(where(is.numeric),round,digits=3))
combined_weights

combined_weights %>% select(-state) %>% colSums()
combined_weights %>% group_by(state) %>% summarise(lateral_weights=sum(c_across(pornhub:porn))) %>% arrange(desc(lateral_weights)) %>%
  mutate(cumulative_weights=cumsum(lateral_weights)/sum(lateral_weights))
latex_table <- combined_weights %>% filter(if_any(where(is.numeric),~.>.001)) %>%
  kbl(caption = "Combined Weights for Models", format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

latex_table %>% write_lines(here::here("model_weights/single_synth_example.tex"))

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
                          data = permute.panel, progfun = "None", fixedeff = FALSE,scm=TRUE
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
