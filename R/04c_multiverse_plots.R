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
  ggplot(aes(y=ATT,colour = treatment))+
  geom_boxplot()+
  facet_wrap(~keyword) +
  labs(title = str_glue('Multiverse_{state}'))
ggsave(here::here(str_glue('figures/multiverse_{state}.png')),width=9,height=9)
}

outcomes %>% filter(Level=='Average') %>% count(treatment,keyword)

outcomes %>% filter(Level=='Average')
outcomes %>% count(scm)
