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
mm <- list.files(here::here('multiverse_tables'),full.names = T)

outcomes<- map_dfr(mm,read_csv)

outcomes %>% write_csv(here::here("combined_multiverse/augsynth_multiverse.csv"))

for (state in unique(outcomes$Level)){
  
  print(state)

  outcomes %>% 
    filter(Level==state) %>% 
    ggplot(aes(y=ATT,x = treatment)) +
    geom_violin() +
    facet_wrap(~keyword) +
    labs(title = str_glue('Multiverse_{state}'))
  
  ggsave(here::here(str_glue('figures/multiverse_{state}.png')),width=9,height=9)

}
