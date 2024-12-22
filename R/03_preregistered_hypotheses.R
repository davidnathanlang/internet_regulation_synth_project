library(dplyr)
library(readr)
library(here)
library(glue)
library(gsynth)
library(tidyverse)
library(tidylog)

pacman::p_load(patchwork)

keyword<-"pornhub"
get_mod_and_effects <- function(keyword) {
  
  keyword_df <- read_csv(here::here(str_glue("data/{keyword}.csv")))  %>% 
    filter(time=="2022-01-01 2024-10-31") # TODO update date range

    
  mod <- gsynth(
    hits~post_treat,
    data=keyword_df,
    index=c("state","int_date"),
    se=TRUE,
    seed=12345,
    inference="parametric"
  )
  
  ce_effects <- cumuEff(mod, period = c(0,12))
  plot(mod,main=str_glue("Estimated ATT ({keyword})"))
  
  # Generate plots for all states except "AL"
  p1 <- plot(mod, main = str_glue("AR: ({keyword})"), id = "AR")
  p2 <- plot(mod, main = str_glue("LA: ({keyword})"), id = "LA")
  p3 <- plot(mod, main = str_glue("MS: ({keyword})"), id = "MS")
  p4 <- plot(mod, main = str_glue("TX: ({keyword})"), id = "TX")
  p5 <- plot(mod, main = str_glue("UT: ({keyword})"), id = "UT")
  p6 <- plot(mod, main = str_glue("VA: ({keyword})"), id = "VA")
  p7 <- plot(mod, main = str_glue("IN: ({keyword})"), id = "IN")
  p8 <- plot(mod, main = str_glue("NE: ({keyword})"), id = "NE")
  p9 <- plot(mod, main = str_glue("KY: ({keyword})"), id = "KY")
  p10 <- plot(mod, main = str_glue("ID: ({keyword})"), id = "ID")
  p11 <- plot(mod, main = str_glue("KS: ({keyword})"), id = "KS")
  p12 <- plot(mod, main = str_glue("MT: ({keyword})"), id = "MT")
  p13 <- plot(mod, main = str_glue("NC: ({keyword})"), id = "NC")
  
  # Combine the plots into a 7x2 grid layout
  state_figure <- (p1 + p2 + p3 + p4 + p5 + p6 + p7) /
    (p8 + p9 + p10 + p11 + p12 + p13 + plot_spacer()) +
    labs(title = str_glue("{keyword}")) +
    theme(plot.title = element_text(hjust = 0.5))  # Center-align the title
  
  # Display the combined figure
  state_figure
  
  
overall_plt<-keyword_df %>% ggplot(aes(x=date,y=hits))+geom_line()+facet_wrap(~state)+
  labs(title=str_glue("{keyword}"))

att_plot<- plot(mod)+labs(title=str_glue('{keyword} ATT'))

  list("mod" = mod, "cum_effects" = ce_effects,state_figure=state_figure,overall_plot=overall_plt,att_plot=att_plot)
}

m1<-get_mod_and_effects("pornhub")
m2<-get_mod_and_effects("vpn")
m3<-get_mod_and_effects("xvideos")
m4<-get_mod_and_effects("porn")

plot(m1$mod)+labs(title='keyword')
(m1$att_plot+m2$att_plo)/ (m3$att_plot+m4$att_plot)
m3$state_figure
(m4$cum_effects)
m3$state_figure

summary(m1$mod)
pacman::p_load(gghighlight)
?gghighlight
pacman::p_load(ggrepel)
full_df  %>% count(enforcement_date)
term<-full_df %>% filter(keyword=="xvideos")
full_df %>% filter(keyword=="xvideos",time=='2022-01-01 2024-06-09') %>%
  ggplot(aes(x=date,y=hits,color=state))+
  geom_vline(aes(xintercept=enforcement_date))+
  facet_wrap(~state)+
  geom_line()+
  theme(legend.position = 'none')
  #gghighlight(use_direct_label = TRUE)
  
  




m1$state_figure
m3$state_figure
m4$state_figure
m1$cum_effects
m2$cum_effects
m3$cum_effects
m4$cum_effects




mod <- augsynth(
  hits ~ post_treat,           # Outcome and treatment variables
  unit = state,              # Unit identifier
  time = rn,           # Time identifier
  data = keyword_df,           # Data frame
  progfunc = "Ridge",          # Use ridge regression for outcome modeling
  scm = TRUE,                  # Synthetic control weights
  fixedeff = TRUE              # Include fixed effects
)
summary(mod)
plot(mod)
m1$overall_plot
m2$overall_plot
m3$overall_plot
m4$overall_plot

m4$cum_effects
plot(m1$mod,main = "Pornhub Search Volume (ATT)")
plot(m2$mod,main = "VPN Search Volume (ATT)")
plot(m3$mod,main = "Xvideos Search Volume (ATT)")
plot(m4$mod,main = "Porn Search Volume (ATT)")
keyword_df<-keyword_df %>%left_join(keyword_df %>% distinct(int_date) %>% mutate(rn=row_number()),by='int_date')
library(augsynth)
ms<-multisynth(hits~post_treat,data=keyword_df,unit=state,time=rn,n_lags=52,n_leads=12)

ms$n_leads
plot(ms)
keyword_df <- read_csv(here::here(str_glue("data/vpn.csv")))  %>% 
  filter(time=="2022-01-01 2024-10-31") # TODO update date range

ms$weights
?multisynth()
