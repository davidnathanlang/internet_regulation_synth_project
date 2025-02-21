library(tidyverse)
library(janitor)
library(readr)
back_of_the_envelope_trends <- read_csv("data/back_of_the_envelope_trends.csv", 
                                        skip = 1) %>% clean_names()

back_of_the_envelope_trends %>% pivot_longer(-week,) %>%
  mutate(name=str_remove(name,"_united_states")) %>%
  ggplot(aes(x=week,y=value,color=name))+
  geom_line()+
labs(x="Date",y='Google Trends',title='National Level Google Trends',color='search term')+
  theme_minimal()
overall_average<-back_of_the_envelope_trends %>% summarise(across(xvideos_united_states:pornhub_united_states,mean))

overall_average
effect_vector<-c(48.1,-51)/100 
effect_vector



df<-
bind_rows(
  overall_average %>% mutate(title='2022 Average'),
as_tibble(overall_average*effect_vector ) %>%  mutate(title='Estimated Impact'),
tibble(net_effect=sum(overall_average*effect_vector),title='Net Effect')
)
df


df 

long_df<-df %>% pivot_longer(c(xvideos_united_states:pornhub_united_states,net_effect)) %>% drop_na() %>% mutate(name=str_remove(name,'_united_states')) %>% mutate(name=str_replace(name,'net_effect','Net Effect'))
long_df$name<- 
  long_df$name <- factor(long_df$name, levels = c("pornhub", "xvideos", 
                                         "Net Effect"))

age_df<-long_df %>% filter(title %in%  c('2022 Average','Estimated Impact')) %>% group_by(name) %>%
  summarise(value=sum(value)) %>% mutate(title='Post-Verification Trends')

final_df<- age_df %>% bind_rows(long_df)
final_df
ggplot(final_df, aes(x = name, y = value, fill = title)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(value, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 4) +
  labs(title = "Estimating The National Impact of Age Verification",
       x = "Keyword",
       y = "Google Trends",
       fill = "Category") +
  scale_fill_manual(values = c("2022 Average" = "#1f78b4", 
                               "Estimated Impact" = "#33a02c", 
                               'Post-Verification Trends'='tan',
                               "Net Effect" = "#e31a1c")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "top")

ggsave(here::here('figures/back_of_the_envelope.png'))
# Convert wide format to long format
df_long <- df %>%
  pivot_longer(cols = c(xvideos_united_states, pornhub_united_states, vpn_united_states, porn_united_states, net_effect), 
               names_to = "category", 
               values_to = "value") %>%
  filter(!is.na(value)) # Remove NA values

# Plot
ggplot(df_long, aes(x = title, y = value, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Effect Comparisons", x = "Category", y = "Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
