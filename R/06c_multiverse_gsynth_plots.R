library(dplyr)
library(readr)
library(here)
library(glue)
library(tidyverse)
library(tidylog)
library(patchwork)
library(augsynth)
library(furrr)
pacman::p_load(geofacet)
pacman::p_load(unglue)
dir.create(here::here("combined_multiverse"))
# List files and parse data
mm <- list.files(here::here('gsynth_tables'),full.names = T)

outcomes<- map_dfr(mm,read_csv)

outcomes %>% count(Level)
state<-'Average'
for (state in unique(outcomes$Level)){
  print(state)

outcomes %>% filter(Level==state)  %>% 
  filter(
    (Time %in%c(4,13) &start_date=='2022-01-01')|
    (Time %in%c(1,3) &start_date=='2019-01-01')
  ) %>% write_csv(here::here("combined_multiverse/gsynth_results.csv"))
  ggplot(aes(y=ATT,x = treatment))+
  geom_violin()+
  facet_wrap(~keyword) +
  labs(title = str_glue('Multiverse_{state}'))
ggsave(here::here(str_glue('figures/multiverse_gsynth_{state}.png')),width=9,height=9)
}

outcomes %>% filter(Level=='Average') %>% count(treatment,keyword)

outcomes %>% filter(Level=='Average') %>% filter(keyword=='pornhub') %>% summarise(min(ATT),max(ATT))
outcomes %>% filter(Level=='Average') %>% filter(keyword=='xvideos') %>% summarise(min(ATT),max(ATT))
outcomes %>% filter(Level=='Average') %>% filter(keyword=='vpn') %>% summarise(min(ATT),max(ATT))
outcomes %>% filter(Level=='Average') %>% filter(keyword=='porn') %>% summarise(min(ATT),max(ATT))
outcomes %>% count(scm)
outcomes %>% count(start_date)
