pacman::p_load(tidyverse)
pacman::p_load(here)
pacman::p_load(furrr)
pacman::p_load(vroom)
pacman::p_load(glue)


# Merge together all google trends data files -----------------------------

state_paths <- dir(here::here("data-raw"),full.names = TRUE) %>% 
  keep(str_detect(.,pattern="csv"))

implementation_dates <-
  read_csv(here::here("data/implementation_dates.csv"))

# Adjust for your computer
plan(multisession, workers = availableCores()-1)

df_all_raw <- future_map_dfr(state_paths, vroom) # vroom is a fast read csv

df_with_imp <- df_all_raw %>%
  tidylog::left_join(implementation_dates %>% select(enforcement_date, state),
                     by = "state") %>%
  mutate(
    date = lubridate::as_date(date),
    treated_state = if_else(!is.na(enforcement_date), 1L, 0L, 0L),
    post_treat = if_else(treated_state &
                           date > enforcement_date, 1L, 0L, 0L),
    int_date = as.integer(date)) # typing issues with the synth_packages



# Merge on census data ----------------------------------------------------

census <- read_csv(here::here("data/census_data.csv"))

full_df <- df_with_imp %>% 
  tidylog::inner_join(census)

# Save the aggregated google trends data
full_df %>% saveRDS(here::here(str_glue("data/analysis_data_{Sys.Date()}.rds")))

# Also save separated files by keyword
full_df %>%
  group_by(keyword) %>%
  group_walk(~ write_csv(.x, glue(here::here("data/{.y$keyword}.csv"))))


