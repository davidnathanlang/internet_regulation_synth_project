library(tidyverse)
library(janitor)

df1<-read_csv(here::here("combined_multiverse/augsynth_multiverse.csv")) %>% mutate(model='augsynth') %>% clean_names() %>% filter(level=='Average')

df2<-read_csv(here::here("combined_multiverse/gsynth_results.csv")) %>% mutate(model='gsynth') %>% clean_names() %>% filter(level=='Average')

df1$``
combined_df<-bind_rows(df1,df2)
combined_df <- combined_df %>%
  mutate(time_group = case_when(
    lags %in% c(1, 4)  ~ "One Month",
    lags %in% c(3, 13) ~ "Three Months",
    time %in% c(1, 4)  ~ "One Month",
    time %in% c(3, 13) ~ "Three Months"
    #TRUE ~ "Other"  # Keeps other values for reference
  ))
combined_df %>% count(time_group)

augsynth_points<-combined_df %>%filter(model=='augsynth') %>% ungroup() %>% group_by(treatment,keyword,time_group) %>% filter(min(pre_mspe)==pre_mspe)
gsynth_points<-combined_df %>%filter(model=='gsynth') %>% group_by(treatment,keyword,time_group) %>% filter(min(mspe)==mspe)

shape_mapping <- c("One Month" = 16, "Three Months" = 17, "Other" = 18)  # 16 = filled circle, 17 = triangle, 18 = diamond

augsynth_points %>% ungroup() %>% count(time_group)
# Plot
combined_df %>%
  ggplot(aes(x = treatment, y = att)) +
  geom_violin(aes(fill = model), alpha = 0.5) +  # Use fill for visual contrast
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # Augsynth Points (with shape)
  geom_point(aes(shape = time_group), size = 3, color = "black", data = augsynth_points) + 
  #geom_text(aes(label = time_group), vjust = -1, size = 3, data = augsynth_points, color = "black") + # Labels
  
  # Gsynth Points (with shape)
  geom_point(aes(shape = time_group), size = 3, color = "black", data = gsynth_points) +
  #geom_text(aes(label = time_group), vjust = -1, size = 3, data = gsynth_points, color = "black") + # Labels
  
  facet_grid(model ~ keyword) +  # Align models in columns, keywords in rows
  scale_x_discrete(labels = c("post_treat_passage" = "Law First Passed",
                              "post_treat" = "Law First Effective",
                              "post_treat_enforcement_date" = "Law First Enforced")) +
  scale_shape_manual(values = shape_mapping, name = "Time Group") +  # Add legend
  scale_fill_manual(values = c("augsynth" = "steelblue", "gsynth" = "firebrick")) +
  xlab("Law Implementation Stage") +
  ylab("Estimated ATT") +
  ggtitle("Effect of Internet Regulation on Search Behavior",
          subtitle = "Comparing Augsynth and GSynth Models with Time Group Differentiation") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

ggsave(here::here('figures/combined_multiverse.png'),width = 8,height = 8)
