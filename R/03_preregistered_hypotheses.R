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
    filter(time=="2022-01-01 2024-07-04") # TODO update date range

    
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
  
p1<-  plot(mod,main=str_glue("AR: ({keyword})"),id = "AR")
p2<-  plot(mod,main=str_glue("LA: ({keyword})"),id = "LA")
p3<-  plot(mod,main=str_glue("MS: ({keyword})"),id = "MS")
p4<-  plot(mod,main=str_glue("TX: ({keyword})"),id = "TX")
p5<-  plot(mod,main=str_glue("UT: ({keyword})"),id = "UT")
p6<-  plot(mod,main=str_glue("VA: ({keyword})"),id = "VA")

state_figure<-(p1
               + p2
               +p3)/(p4+p5+p6) +labs(title="{keyword}")

overall_plt<-keyword_df %>% ggplot(aes(x=date,y=hits))+geom_line()+facet_wrap(~state)+
  labs(title=str_glue("{keyword}"))

  list("mod" = mod, "cum_effects" = ce_effects,state_figure=state_figure,overall_plot=overall_plt)
}

m1<-get_mod_and_effects("pornhub")
m2<-get_mod_and_effects("vpn")
m3<-get_mod_and_effects("xvideos")
m4<-get_mod_and_effects("porn")

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
  
  




m1$cum_effects
m2$cum_effects
m3$cum_effects
m4$cum_effects






m1$overall_plot
m2$overall_plot
m3$overall_plot
m4$overall_plot

plot(m1$mod,main = "Pornhub Search Volume (ATT)")
plot(m2$mod,main = "VPN Search Volume (ATT)")
plot(m3$mod,main = "Xvideos Search Volume (ATT)")
plot(m4$mod,main = "Porn Search Volume (ATT)")





