pacman::p_load(rvest)
pacman::p_load(tidyverse)
pacman::p_load(tidycensus)
pacman::p_load(janitor)
pacman::p_load(lubridate)
#https://action.freespeechcoalition.com/age-verification-resources/state-avs-laws/

law_effective_date<-tibble::tribble(
  ~State, ~Abbreviation, ~LawEffectiveDate, ~RDateSyntax,
  "Idaho", "ID", "2024-07-01", "make_date(2024, 7, 1)",
  "Indiana", "IN", "2024-08-16", "make_date(2024, 7, 1)",
  "Kansas", "KS", "2024-07-01", "make_date(2024, 7, 1)",
  "Kentucky", "KY", "2024-07-15", "make_date(2024, 7, 15)",
  "Nebraska", "NE", "2024-07-18", "make_date(2024, 7, 18)",
  "Alabama", "AL", "2024-10-01", "make_date(2024, 10, 1)",
  "Oklahoma", "OK", "2024-11-01", "make_date(2024, 10, 1)",
  "Florida", "FL", "2025-01-01", "make_date(2025, 1, 1)",
  "South Carolina", "SC", "2025-01-01", "make_date(2025, 1, 1)",
  "Tennessee", "TN", "2025-01-01", "make_date(2025, 1, 1)",
  "Georgia", "GA", "2025-07-01", "make_date(2025, 7, 1)",
  "North Carolina", "NC", "2024-01-01", "make_date(2024, 1, 1)",
  "Montana", "MT", "2024-01-01", "make_date(2024, 1, 1)",
  "Texas", "TX", "2023-09-23", "make_date(2024, 1, 1)",
  "Arkansas", "AR", "2023-07-31", "make_date(2023, 7, 31)",
  "Virginia", "VA", "2023-07-01", "make_date(2023, 7, 1)",
  "Mississippi", "MS", "2023-07-01", "make_date(2023, 7, 1)",
  "Utah", "UT", "2023-05-03", "make_date(2023, 5, 3)",
  "Louisiana", "LA", "2023-01-01","") 
law_effective_date %>% write_csv(here::here("data/fsc_implementation_dates.csv"))
