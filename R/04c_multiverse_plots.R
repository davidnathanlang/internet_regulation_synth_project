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

# List files and parse data
mm <- list.files(here::here('multiverse_tables'),full.names = T)

outcomes<- map_dfr(mm,read_csv)

for (state in unique(outcomes$Level)){
  print(state)

outcomes %>% filter(Level==state)  %>% 
  ggplot(aes(y=ATT,x = treatment))+
  geom_violin()+
  facet_wrap(~keyword) +
  labs(title = str_glue('Multiverse_{state}'))
ggsave(here::here(str_glue('figures/multiverse_{state}.png')),width=9,height=9)
}

outcomes %>% filter(Level=='Average') %>% count(treatment,keyword)

outcomes %>% filter(Level=='Average') %>% filter(keyword=='pornhub') %>% summarise(min(CATT),max(CATT))
outcomes %>% filter(Level=='Average') %>% filter(keyword=='xvideos') %>% summarise(min(CATT),max(CATT))
outcomes %>% filter(Level=='Average') %>% filter(keyword=='vpn') %>% summarise(min(CATT),max(CATT))
outcomes %>% filter(Level=='Average') %>% filter(keyword=='porn') %>% summarise(min(CATT),max(CATT))
outcomes %>% count(scm)
