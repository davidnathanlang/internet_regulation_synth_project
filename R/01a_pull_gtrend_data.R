pacman::p_load(gtrendsR)
pacman::p_load(tidyverse)
pacman::p_load(tidycensus)


states_vector <-
  tidycensus::fips_codes %>% 
  filter(state_code <= "56") %>% # TODO: add explanation
  distinct(state) %>% 
  pull()


search_terms <- c(
  "porn",
  "pornhub",
  "xvideos",
  "vpn",
  "wicked",
  "wicked.com"
)

end_date <- '2024-10-31'


time_spans <- c(
  str_glue("2023-01-01 {end_date}"),
  str_glue("2022-01-01 {end_date}"),
  str_glue("2019-01-01 {end_date}")
)

parameter_tibble <- expand_grid(terms = search_terms,
                                state_abb = states_vector,
                                times = time_spans) %>% 
  arrange(terms, state_abb, times)

gtrends_call <- function(term = "VPN",
                         state = "CA",
                         time_span = "all") {
  
  filename <-
    here::here(str_glue("data-raw/{state}_{term}_{time_span}.csv"))
  
  # Check if the file already exists
  if (file.exists(filename)) { 
    print(paste("The file: ", filename, " already exists. The code will not run."))
  } else {
    print(str_glue("{term}_{state}_{time_span}"))
    
    retries <- 5
    while(retries > 0){
      tryCatch({
        my_trend <-
          gtrends(term,
                  str_c("US-", state),
                  time = time_span,
                  onlyInterest = TRUE)
        retries <- 0  # if no error, we exit the loop
      },
      error = function(e){
        if(e$message == 'Status code was not 200. Returned status code:429'){
          print(e$message)
          print("Sleeping for at least an hour")
          
          Sys.sleep(sample(900:3600, 1))  # sleep for 1-2 hours
          
          print(e$message)
          retries <- retries - 1  # decrement retry count
          print("Retries Remaining")
          print(retries)
          
        }else{
          stop(e)  # if error is not 429, we stop the execution
        }
      })
    }
    
    Sys.sleep(runif(1,300,600)) # necessary to keep the API happy
    
    if (is.null(my_trend$interest_over_time)) {
      return("Data Not Found")
    }
    
    gtrend_df <-
      my_trend$interest_over_time %>% mutate(term = term,
                                             state = state,
                                             time_span = time)
    
    gtrend_df %>% write_csv(filename)
    
  } 
}

pwalk(parameter_tibble,  ~gtrends_call(..1, ..2, ..3))

