library(tidyverse)
library(janitor)

df1 <- read_csv(here::here("combined_multiverse/augsynth_multiverse.csv")) %>% mutate(model='augsynth') %>% clean_names() %>% filter(level=='Average')

df2 <- read_csv(here::here("combined_multiverse/gsynth_results.csv")) %>% mutate(model='gsynth') %>% clean_names() %>% filter(level=='Average') %>%
  filter(time %in% c(1,3) & start_date=='2019-01-01'|
         time %in% c(4,13) & start_date=='2022-01-01'
         )
  

combined_df<-bind_rows(df1,df2)

combined_df <- combined_df %>%
  mutate(time_group = case_when(
    lags %in% c(1, 4)  ~ "One Month",
    lags %in% c(3, 13) ~ "Three Months",
    time %in% c(1, 4)  ~ "One Month",
    time %in% c(3, 13) ~ "Three Months"
    #TRUE ~ "Other"  # Keeps other values for reference
  ))

combined_df$treatment = factor(combined_df$treatment,levels = c("post_treat_passage", "post_treat", "post_treat_enforcement_date"))
combined_df$keyword = factor(combined_df$keyword,levels = c("porn", "vpn", "pornhub", "xvideos"))

augsynth_points<-combined_df %>%filter(model=='augsynth') %>% ungroup() %>% group_by(treatment,keyword,time_group) %>% filter(min(pre_mspe)==pre_mspe)
gsynth_points<-combined_df %>%filter(model=='gsynth') %>% group_by(treatment,keyword,time_group) %>% filter(min(mspe)==mspe)

shape_mapping <- c("One Month" = 16, "Three Months" = 17, "Other" = 18)  # 16 = filled circle, 17 = triangle, 18 = diamond

# Plot
combined_df %>%
  ggplot(aes(x = treatment, y = att)) +
  geom_violin(aes(fill = model), alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # Modified geom_point with explicit filtering and error handling
  geom_point(data = bind_rows(augsynth_points, gsynth_points) %>%
               filter(!is.na(att), 
                      is.finite(att),
                      !is.na(time_group)),
             aes(shape = time_group, group = model),
             size = 3,
             position = position_dodge(width = 0.9),
             na.rm = TRUE) +
  
  facet_wrap(~ keyword, nrow = 2) +
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

# ggsave(here::here('figures/combined_multiverse.png'),width = 8,height = 8)


combined_df %>% group_by(keyword) %>% filter(treatment!='post_treat_passage') %>% summarise(min(att),max(att), n())

combined_df %>% group_by(keyword,model) %>% filter(treatment!='post_treat_passage') %>% summarise(min(att),max(att), n()) %>% arrange(model)
combined_df$model

vpn<-  combined_df %>%filter(keyword=='vpn',model=='augsynth') %>% ungroup() %>% select(start_date:verification_method) 

pornhub<-  combined_df %>%filter(keyword=='pornhub',model=='augsynth') %>% ungroup() %>% select(start_date:verification_method)

names(vpn)
vpn %>% semester(pornhub)
names(pornhub)
3456/4
names(pornhub)
vpn %>% anti_join(pornhub)
combined_df %>% count(treatment)
levels(combined_df$treatment)