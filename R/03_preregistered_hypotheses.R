library(dplyr)
library(readr)
library(here)
library(glue)
library(gsynth)
library(tidyverse)
library(tidylog)
library(patchwork)

# Create directories for saving outputs
figures_dir <- here("figures")
models_dir <- here("mods")
dir.create(figures_dir, showWarnings = FALSE)
dir.create(models_dir, showWarnings = FALSE)
keyword<-'pornhub'
seed<-12345
inference<-'parametric'
time_range <- '2022-01-01 2024-10-31'
# Hyperparameter search function
hyperparameter_search <- function(keyword, time_range = '2022-01-01 2024-10-31', seed = 12345, inference = "parametric") {
  # Load data
  keyword_df <- read_csv(here::here(str_glue("data/{keyword}.csv"))) %>%
    filter(time == time_range) # Adjust date range as needed
  
  # Model fitting
  mod <- gsynth(
    hits ~ post_treat,
    data = keyword_df,
    index = c("state", "int_date"),
    se = TRUE,
    seed = seed,
    inference = inference,
    force = 'none'
  )
  
  # Cumulative effects
  ce_effects <- cumuEff(mod, period = c(0, 12))
  
  # Save model output
  saveRDS(mod, file = here(models_dir, str_glue("{keyword}_model.rds")))
  
  # Generate plots
  att_plot <- plot(mod) + labs(title = str_glue("{keyword} ATT"))
  treated_states <- keyword_df %>%
    filter(post_treat == 1) %>%
    distinct(state) %>%
    pull(state)
  
  # Generate plots for each state and store in a list
  state_plots <- lapply(treated_states, function(state) {
    plot(mod, id = state) + 
      labs(title = glue("{state}: ({keyword})"))
  })
  
  # Combine all plots into a single layout using patchwork
  combined_plot <- wrap_plots(state_plots) +
    plot_annotation(title = glue("All States: {keyword}")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Display the combined plot
  combined_plot
  
  # Save figures
  ggsave(filename = here(figures_dir, str_glue("{keyword}_state_plots.png")), plot = combined_plot)
  ggsave(filename = here(figures_dir, str_glue("{keyword}_att_plot.png")), plot = att_plot)
  
  # Overall keyword plot
  overall_plot <- keyword_df %>%
    ggplot(aes(x = date, y = hits,colour = factor(post_treat))) +
    geom_line() +
    facet_wrap(~state) +
    labs(title = str_glue("{keyword}"))
  
  ggsave(filename = here(figures_dir, str_glue("{keyword}_overall_plot.png")), plot = overall_plot)
  
  list(mod = mod, cum_effects = ce_effects, att_plot = att_plot, state_plots = combined_plot, overall_plot = overall_plot)
}

# Run hyperparameter search for multiple keywords
keywords <- c("pornhub", "vpn", "xvideos", "porn")
results <- lapply(keywords, function(keyword) {
  hyperparameter_search(keyword = keyword)
})

# Access and display results
lapply(results, function(res) {
  print(summary(res$mod))
  res$cum_effects
})

