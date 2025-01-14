library(dplyr)
library(readr)
library(here)
library(glue)
library(gsynth)
library(tidyverse)
library(tidylog)
library(patchwork)
pacman::p_load(geofacet)
pacman::p_load(ggrepel)

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
  ce_effects <- cumuEff(mod, period = c(0, 12))$est.catt %>%as.tibble()%>% mutate(topic=keyword) %>% mutate(rn=row_number()-1)
  ce_effects
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
    labs(
      x = "Time (Weeks) ",    # New x-axis label
      y =  "ATT", # (Average Treatment Effect)",         # New y-axis label
      title = str_glue("{state}") # Unique title for each plot
    ) +
      theme_minimal()  # Optional: Use a clean theme
  })
  
  # Combine all plots into a single layout using patchwork
  combined_plot <- wrap_plots(state_plots) +
    plot_annotation(title = glue("All States: {keyword}")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Display the combined plot
  combined_plot
  
  # Save figures
  ggsave(filename = here(figures_dir, str_glue("{keyword}_state_plots.png")), plot = combined_plot,width = 9,height = 9)
  ggsave(filename = here(figures_dir, str_glue("{keyword}_att_plot.png")), plot = att_plot,width = 9,height = 9)
  
  vline_dates<-keyword_df %>% distinct(enforcement_date,state) %>% drop_na()
  # Overall keyword plot
  overall_plot <- keyword_df %>%
    ggplot(aes(x = date, y = hits,colour = factor(post_treat))) +
    geom_vline( aes(xintercept = enforcement_date),data=vline_dates,linetype='dashed') +
    geom_line() +
    #geom_point()+
    facet_geo(~state) +
    labs(
      title = str_glue("{keyword}"),
      colour = "Age Verification Status"  # Update legend title
    ) +
    scale_colour_manual(
      values = c("0" = "red", "1" = "blue"),  # Customize colors if desired
      labels = c("0" = "Age Verification Laws Not Passed", "1" = "Age Verification Passed")
    ) +
    scale_x_date(
      date_labels = "'%y",  # Display last two digits of the year
      date_breaks = "1 year"  # Adjust breaks as necessary
    ) +
    theme_minimal() + # Use a cleaner theme 
  theme(legend.position = 'bottom')
  
  
  ggsave(filename = here(figures_dir, str_glue("{keyword}_overall_plot.png")), plot = overall_plot,width = 9,height = 9)
  
  list(mod = mod, cum_effects = ce_effects, att_plot = att_plot, state_plots = combined_plot, overall_plot = overall_plot,search_term=keyword)
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

str(results)
ce_pre_registered<-
bind_rows(
results[[1]]$cum_effects  ,
results[[2]]$cum_effects ,
results[[3]]$cum_effects  ,
results[[4]]$cum_effects
) %>%   mutate(
  time_point = case_when(
    rn == 4 ~ "1 Month",
    rn == 12 ~ "3 Months"
  )
)
#ce_pre_registered %>% 
ce_fig<-ce_pre_registered %>% filter(rn %in% c(4,12)) %>% 
  ggplot(aes(x = time_point, y = CATT, fill = topic)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = CI.lower, ymax = CI.upper), width = 0.2, position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Add zero line
  geom_label_repel(
    aes(label = round(CATT, 1)), # Add point estimates as labels
    position = position_dodge(width = 0.7),
    size = 4,
    color = "black"
  ) +
  labs(
    x = "Evaluation Time Point",
    y = "Cumulative ATT",
    title = "Cumulative ATT with Confidence Intervals at 1 and 3 Months",
    fill = "Topic"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "top"
  ) 

ggsave(filename = here(figures_dir, str_glue("pre-registered_cumulative_effects.png")), plot = ce_fig,width = 9,height = 9)

  
plots <- lapply(1:4, function(i) {
  results[[i]]$att_plot +
    labs(
      x = "Time (Weeks) Relative to Law Passage",    # New x-axis label
      y =  "ATT", # (Average Treatment Effect)",         # New y-axis label
      title = str_glue("Estimated ATT Effect Over Time (Search Term {results[[i]]$search_term})") # Unique title for each plot
    ) +
    theme_minimal()  # Optional: Use a clean theme
})

# Assign individual plots for reference
p1 <- plots[[1]]
p2 <- plots[[2]]
p3 <- plots[[3]]
p4 <- plots[[4]]

# Combine plots using patchwork
combined_plot <- p1 / p3 / p2 / p4  # Stack them vertically

# Display the combined plot
print(combined_plot)

# Save the combined plot
ggsave(
  filename = here(figures_dir, str_glue("pre_registered_specification_plot.png")),
  plot = combined_plot,
  width = 10,
  height = 15
)


