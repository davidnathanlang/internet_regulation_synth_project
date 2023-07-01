pacman::p_load(tidycensus)
pacman::p_load(tidyverse)


# This product uses the Census Bureau Data API but is not endorsed or certified 
# by the Census Bureau.
# You can obtain an API key at https://api.census.gov/data/key_signup.html
# rename the file: `api_keys.sample` to remove .sample, and add your key value into that file

source("api_keys")
census_api_key(key = census_api_key)


all_vars_acs5 <- load_variables(year = 2021, dataset = "acs5")
states_df <-
  tidycensus::fips_codes %>% distinct(state, state_code, state_name)

# Specify the year and dataset
year <- 2021
dataset <- "acs5"

# Specify the variables for computer usage
variables_computer <- c("B28002_001", "B28002_002", "B28002_003")

# Specify the variables for racial distribution
variables_race <- c("B02001_001", "B02001_002", "B02001_003", "B02001_004", "B02001_005", "B02001_006", "B02001_007", "B02001_008")



variables_gender <- c("B01001_001", "B01001_002")
# Specify the variables for household income
variables_income <- c("B19013_001")

# Specify the variables for educational attainment
variables_education <- c("B15003_001", "B15003_017", "B15003_018", "B15003_022", "B15003_023", "B15003_024", "B15003_025")

# Get computer usage data by state
data_computer <- get_acs(geography = "state", variables = variables_computer, year = year, survey = dataset) %>%
  left_join(all_vars_acs5, by = c("variable" = "name")) %>%
  pivot_wider(id_cols = "NAME", names_from = "label", values_from = "estimate") %>%
  transmute(NAME, housesholds_with_internet_access = (`Estimate!!Total:!!With an Internet subscription` - `Estimate!!Total:!!With an Internet subscription!!Dial-up with no other type of Internet subscription`) / `Estimate!!Total:`)


data_race <- get_acs(geography = "state", variables = variables_race, year = year, survey = dataset)  %>%
  left_join(all_vars_acs5,by=c("variable"="name")) %>%
  pivot_wider(id_cols="NAME",names_from = "label", values_from = "estimate")  %>%
  transmute(
    NAME,
    white=   `Estimate!!Total:!!White alone`  /`Estimate!!Total:`,
    black=   `Estimate!!Total:!!Black or African American alone`  /`Estimate!!Total:`,
    amerindian=   `Estimate!!Total:!!American Indian and Alaska Native alone`  /`Estimate!!Total:`,
    asian=   `Estimate!!Total:!!Asian alone`  /`Estimate!!Total:`,
    native_hawaiian=   `Estimate!!Total:!!Native Hawaiian and Other Pacific Islander alone`  /`Estimate!!Total:`,
    two_or_more_races=   `Estimate!!Total:!!Two or more races:`  /`Estimate!!Total:`,
  )


data_gender <- get_acs(geography = "state", variables = variables_gender, year = year, survey = dataset)  %>% left_join(all_vars_acs5,by=c("variable"="name"))%>%
  pivot_wider(id_cols="NAME",names_from = "label", values_from = "estimate") %>%
  transmute(NAME,male=(`Estimate!!Total:!!Male:`/`Estimate!!Total:`))

# Get educational attainment data by state
data_education <- get_acs(geography = "state", variables = variables_education, year = year, survey = dataset) %>% left_join(all_vars_acs5,by=c("variable"="name")) %>%
  pivot_wider(id_cols="NAME",names_from = "label", values_from = "estimate") %>%
  transmute(NAME,
            high_school_or_ged=(`Estimate!!Total:!!Regular high school diploma`+`Estimate!!Total:!!GED or alternative credential`)/`Estimate!!Total:`,
            bachelors_plus=(`Estimate!!Total:!!Bachelor's degree`+`Estimate!!Total:!!Master's degree`+`Estimate!!Total:!!Professional school degree`+`Estimate!!Total:!!Doctorate degree`)/`Estimate!!Total:`,
  )

data_income <-  get_acs(geography = "state", variables = variables_income, year = year, survey = dataset) %>% left_join(all_vars_acs5,by=c("variable"="name")) %>%
  pivot_wider(id_cols="NAME",names_from = "label", values_from = "estimate") %>%
  transmute(NAME,household_income=`Estimate!!Median household income in the past 12 months (in 2021 inflation-adjusted dollars)`)

dat_objects <- ls(pattern = "^data_")

# Create a list of data frames
dat_list <- map(dat_objects, ~ get(.x))

# Merge the data frames based on the "NAME" variable using an inner join
merged_data <- reduce(dat_list, inner_join, by = "NAME")

merged_final_df <- merged_data %>% left_join(states_df,by=c("NAME"="state_name"))

# Save output
write_csv(merged_final_df, here::here("data/census_data.csv"))
