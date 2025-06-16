library(dplyr)
library(readr)
library(here)
library(glue)
library(gsynth)
library(tidyverse)
library(tidylog)
library(patchwork)
library(xtable)
library(kableExtra)
pacman::p_load(geofacet)
pacman::p_load(ggrepel)
pacman::p_load(did)
pacman::p_load(ptetools)
pacman::p_load(staggered)
# Create directories for saving outputs

figures_dir <- here("did_figures")

dir.create(figures_dir, showWarnings = FALSE)
dir.create(models_dir, showWarnings = FALSE)


seed<-12345
inference<-'parametric'
time_range <- '2022-01-01 2024-10-31'

keyword<-'vpn'
for (keyword in c('pornhub','vpn','xvideos','porn'))
{
keyword_df <- read_csv(here::here(str_glue("data/{keyword}.csv"))) %>%
  filter(time == time_range) # Adjust date range as needed
state_num<-keyword_df %>% distinct(state) %>% mutate(state_num=row_number())

start_date<-keyword_df %>% filter(post_treat==1L) %>% group_by(state) %>% summarise(first_treat=as.integer(min(date)-7))

pte_df<-keyword_df %>% left_join(start_date) %>%
  mutate(first_treat=if_else(is.na(first_treat),as.integer(0L),as.integer(first_treat)),date=as.integer(date)) %>% left_join(state_num,by='state') %>%
  mutate(relative_treatment_time=date-first_treat) %>%
  filter(first_treat==0|relative_treatment_time<91)
pte_df
pte_df
did_res <- did::att_gt(
  yname = "hits",
  gname = "first_treat",
  tname = "date",
  idname = "state_num",
  data = pte_df,
  allow_unbalanced_panel = TRUE,
  base_period = 'varying'
  # setup_pte_fun = setup_pte,
  # subset_fun = two_by_two_subset,
  # attgt_fun = did_attgt
  
)


states_treated<-pte_df %>%
  distinct(first_treat, state) %>%
  group_by(first_treat) %>%
  summarise(states_concat = str_flatten(state, collapse = ", "))

states_treated
p1<-ggdid(did_res) 
str(did_res)
group_time_treatment<-
p1$data %>% left_join(states_treated,by=c("group"='first_treat')) %>%
  mutate(state_date=str_c(as.Date(as.integer(as.character(group))),states_concat,sep=" ")) %>%
  mutate(relative_treatment_time=as.integer(group)-as.integer(as.character(year))) %>%
  mutate(calendar_date=as.Date(as.integer(year)))

group_time_treatment$relative_treatment_time
group_time_treatment %>%  filter(relative_treatment_time>=0L & relative_treatment_time<91) %>%
  group_by(state_date) %>%
  summarise(mean(att))
group_time_treatment %>%
  ggplot(aes(x = as.Date(as.integer(as.character(year))), y = att)) +
  geom_errorbar(aes(ymin = att - att.se * c, ymax = att + att.se * c, color = post), width = 5) +
  # Add line or point to show actual estimate if desired
  # geom_point(aes(color = post)) +
  geom_vline(aes(xintercept = as.Date(as.integer(as.character(group)))
                 ),
             linetype='dotted'
             )+
  facet_wrap(~state_date, scales = "free_y") +
  scale_x_date(date_breaks = '6 months', date_labels = "%Y-%m") +
  scale_color_manual(values = c("red", "steelblue")) +
  
  labs(
    x = "Date",
    y = "ATT",
    color = "Post-treatment",
    title = str_glue("ATT ({keyword}) Over Time by State and Date")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(here::here(figures_dir,str_glue('{keyword}_grouptime.jpg')),width = 8,height = 10)



dyn_did<-aggte(did_res,type='dynamic',min_e=-Inf,max_e = 84,na.rm = TRUE) %>% ggdid()
dyn_did$data
dyn_did$data$year<-dyn_did$data$year/7
dyn_did+scale_x_continuous()
ggsave(here::here(figures_dir,str_glue('{keyword}_eventstudy.jpg')),width = 8,height = 10)
gg_group<-aggte(did_res,type='group',min_e=-Inf,max_e=84,na.rm=TRUE)
#ggdid(gg_group)

aggte(did_res,type='group',min_e = 84,na.rm=TRUE)
aggte(did_res,type='group',na.rm=TRUE)


pte_df %>% distinct(first_treat,state) %>%
  group_by(first_treat) %>% 
  summarise(states=reduce(state,str_c(sep=' , ')))
gg<-ggdid(dyn_group)


(dyn_simple<-aggte(did_res,type='simple',bstrap = TRUE,cband=TRUE,na.rm = TRUE))

?aggte

dyn_simple$overall.att
plot_data<-gg$data %>% left_join(states_treated,by=c('year'='first_treat')) %>%
  mutate(dates_plus_states=str_c(as_date(year), states_concat))
overall_effect<-tibble(att=dyn_simple$overall.att,att.se=dyn_simple$overall.se,c=1.96,dates_plus_states="Average")
plot_data
plot_data %>% 
  bind_rows(overall_effect) %>%
  ggplot(aes(x=dates_plus_states, states_concat,y=att))+
  geom_point()+
  geom_errorbar(aes(ymin = att-att.se*c,ymax=att+att.se*c,y=att))  +coord_flip()+
  geom_hline(aes(yintercept = 0))+
  labs(title = keyword_df$term)
ggsave(here::here(figures_dir,str_glue('{keyword}_state_agg.jpg')))
}


