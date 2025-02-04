library(dplyr)
library(readr)
library(here)
library(glue)
library(tidyverse)
library(tidylog)
library(patchwork)
library(augsynth)
library(parallel)
library(pbapply)  # for progress bar
pacman::p_load(geofacet)
pacman::p_load(unglue)

# List files and parse data
mm <- list.files(here::here('multiverse_mod'))

table_dir <- here::here("multiverse_tables")
if (!dir.exists(table_dir)) {
  dir.create(table_dir)
}

pattern <- "{keyword}_{start_date}_{end_date}_{covariate_tag}_scm{scm}_fixedeff{fixedeffects}_leads{leads}_lags{lags}_treatment_{treatment}_verification_method_{verification_method}.rds"
parsed_data <- unglue_data(mm, pattern) %>%
  mutate(
    path = list.files(here::here('multiverse_mod'), full.names = TRUE),
    rn = row_number()
  )

# Initial data exploration
# parsed_data %>% count(keyword)
# parsed_data %>% count(start_date)
# parsed_data %>% count(end_date)
# parsed_data %>% count(covariate_tag)
# parsed_data %>% count(scm)
# parsed_data %>% count(fixedeffects)
# parsed_data %>% count(leads)
# parsed_data %>% count(lags)
# parsed_data %>% count(treatment)
# parsed_data %>% count(verification_method)

# Define the processing function
process_row <- function(i) {
  if(i %% 100 == 0) gc()
  
  single_example <- parsed_data[i, ] %>% 
    mutate_at(c('leads','lags'), as.numeric)
  
  output_path <- here::here(glue("multiverse_tables/{single_example$rn}.csv"))
  
  if (file.exists(output_path)) {
    return(glue("Skipping row {i}: Output file already exists."))
  }
  
  tryCatch({
    ms_mod <- read_rds(single_example$path)
    ms_summary <- summary(ms_mod)
    
    post_treatment <- ms_summary$att %>%
      as.data.frame() %>%
      filter(Time >= 0, Time <= single_example$lags) %>%
      group_by(Level) %>%
      summarise(
        POST_MSPE = mean(Estimate^2),
        ATT = mean(Estimate),
        CATT = sum(Estimate),
        .groups = "drop"
      )
    
    if(is.null(single_example$leads))
      single_example$leads <- -Inf
    
    pre_treatment <- ms_summary$att %>%
      as.data.frame() %>%
      filter(Time < 0, Time >= -single_example$leads) %>%
      group_by(Level) %>%
      summarise(
        PRE_MSPE = mean(Estimate^2),
        MAB = mean(abs(Estimate)),
        MEAN_ERROR = mean(Estimate),
        .groups = "drop"
      )
    
    joined_results <- single_example %>%
      full_join(pre_treatment, by = character()) %>%
      full_join(post_treatment, by = "Level")
    
    write_csv(joined_results, output_path)
    
    return(glue("Processed row {i}: {single_example$path}"))
  }, error = function(e) {
    return(glue("Error processing row {i}: {e$message}"))
  })
}

# Set up parallel processing
num_cores <- detectCores() - 1  # Leave one core free for system processes
cl <- makeCluster(num_cores)

# Export necessary objects and packages to the cluster
clusterExport(cl, c("parsed_data", "process_row"))
clusterEvalQ(cl, {
  library(dplyr)
  library(readr)
  library(here)
  library(glue)
  library(tidylog)
  library(augsynth)
})

# Process in parallel with progress bar
results <- pblapply(
  X = seq_len(nrow(parsed_data)), 
  FUN = process_row,
  cl = cl
)

# Clean up
stopCluster(cl)

# Print results (optional)
cat(unlist(results), sep="\n")
