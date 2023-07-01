library(dplyr)
library(readr)
library(here)
library(glue)
library(gsynth)
library(tidylog)


get_mod_and_effects <- function(keyword) {
  
  keyword_df <- read_csv(here::here(str_glue("data/{keyword}.csv")))  %>%
    filter(time=="2022-01-01 2023-06-21") # TODO update date range
  
  mod <- gsynth(
    hits~post_treat,
    data=keyword_df,
    index=c("state","int_date"),
    se=TRUE,
    seed=12345,
    inference="parametric"
  )
  
  ce_effects <- cumuEff(mod, period = c(0,12))
  plot(mod)  
  list("mod" = mod, "cum_effects" = ce_effects)
}


get_mod_and_effects("pornhub")
get_mod_and_effects("vpn")
get_mod_and_effects("xvideos")
get_mod_and_effects("porn")


