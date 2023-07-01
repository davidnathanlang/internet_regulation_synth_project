pacman::p_load(rvest)
pacman::p_load(tidyverse)
pacman::p_load(tidycensus)
pacman::p_load(janitor)
pacman::p_load(lubridate)

states_df <- tidycensus::fips_codes %>% 
  distinct(state, state_code, state_name) 

page <- read_html("https://action.freespeechcoalition.com/age-verification-bills/")
table <- html_table(html_nodes(page, "table")[[1]])

print(table)

# Clean and format the dates
table <- table %>%
  filter(str_detect(`Last Updated`,"Failed",negate=TRUE)) %>%
  mutate(
    `Last Updated` = gsub("<br />.*", "", `Last Updated`), # Remove "<br />" and anything after it
    `Last Updated` = as.Date(`Last Updated`, format = "%m/%d/%Y"), # Format as Date object
    `Enforcement Date` = gsub("z", "", `Enforcement Date`), # Remove "z"
    `Enforcement Date` = gsub("if passed", "", `Enforcement Date`), # Remove "if passed"
    `Enforcement Date` = as.Date(`Enforcement Date`, format = "%m/%d/%Y")
  ) # Format as Date object


# Print the cleaned and formatted table
# TODO Add Louisiana
# TODO If Two word states enforce the law, this code will require modification

implementation_df <-
  table %>%
  drop_na(`Enforcement Date`) %>%
  bind_rows(
    tibble(
      Bill = "Louisiana HB 142",
      `Last Updated` = lubridate::make_date(2022, 06, 15),
      `Enforcement Date` = lubridate::make_date(2023, 01, 1)
    )
  ) %>%
  mutate(state_name = word(Bill, 1)) %>%
  left_join(states_df, by = "state_name") %>%
  janitor::clean_names() %>%
  mutate_if(is.Date,lubridate::as_date)

implementation_df %>% write_csv(here::here("data/implementation_dates.csv"))
