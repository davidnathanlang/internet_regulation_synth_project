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
  three_month_att<- ce_effects %>% filter(rn+1==13) %>% transmute(ATT=CATT/13) %>% pull()
  
  # Generate plots
  att_plot <- plot(mod) + labs(title = str_glue("{keyword} ATT")) +
    geom_hline(yintercept = three_month_att,linetype='dashed')+
    annotate("text", x = -12, y = three_month_att+2, label = str_c("ATT = ",round(three_month_att,1)))
  
  treated_states <- keyword_df %>%
    filter(post_treat == 1) %>%
    distinct(state) %>%
    pull(state)
  
  # Generate plots for each state and store in a list
  state_plots <- lapply(treated_states, function(state) {
    thre_month_est<-cumuEff(mod,cumu = FALSE,id=state,c(0,12)) %>% as_tibble() %>% summarise(three_month_att=mean(catt)) %>% pull(three_month_att)
    plot(mod, id = state) + 
      labs(
        x = "Time (Weeks) ",    # New x-axis label
        y =  "ATT", # (Average Treatment Effect)",         # New y-axis label
        title = str_glue("{state}") # Unique title for each plot
      ) +
      geom_hline(yintercept = thre_month_est,linetype='dashed')+
      annotate("text", x = -25, y = thre_month_est+2, label = str_c("ATT = ",round(thre_month_est,1))) +
      theme_minimal()  # Optional: Use a clean theme
  })
  
  # Combine all plots into a single layout using patchwork
  combined_plot <- wrap_plots(state_plots) +
    plot_annotation(title = glue("All States: {keyword}")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Display the combined plot
  # combined_plot
  
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
# lapply(results, function(res) {
#   print(summary(res$mod))
#   res$cum_effects
# })

calculate_pct_change <- function(result, mod_index) {
  plot(result[[mod_index]]$mod, type = 'counterfactual')$data %>%
    pivot_wider(names_from = type, values_from = outcome) %>%
    filter(time >= 0) %>%
    mutate(pct_change = cumsum(tr - co) / cumsum(co)) %>% mutate(topic=result[[mod_index]]$search_term,rn=time)
}

pct_change<-bind_rows(
  calculate_pct_change(results, 1),
  calculate_pct_change(results, 2),
  calculate_pct_change(results, 3),
  calculate_pct_change(results, 4)
)


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
    )) %>% 
  left_join(pct_change,by=c('topic','rn'))

#ce_pre_registered %>% 
ce_fig <- ce_pre_registered %>% 
  filter(rn %in% c(4, 12)) %>% 
  ggplot(aes(x = time_point, y = CATT, fill = topic)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6,alpha=1) +
  geom_errorbar(aes(ymin = CI.lower, ymax = CI.upper), 
                width = 0.2, 
                position = position_dodge(width = 0.8),
                size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_label_repel(
    aes(label = str_c(round(CATT, 1), '\n (', round(pct_change * 100, 1), '%)')),
    position = position_dodge(width = 0.8), 
    size = 3.5,
    color = "black",
    #nudge_y = 10,
    segment.color = "grey70"
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Evaluation Time Point",
    y = "Cumulative ATT",
    title = "Cumulative ATT with Confidence Intervals at 1 and 3 Months",
    subtitle = "Displaying differences by topic across time points",
    fill = "Topic"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

# ce_fig
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

three_month_att<-plots[[1]]$data %>% filter(time>0,time<13) %>% summarise(mean(ATT)) %>% pull()
# Assign individual plots for reference
p1 <- plots[[1]] 
p2 <- plots[[2]]
p3 <- plots[[3]]
p4 <- plots[[4]]

# Combine plots using patchwork
combined_plot <- p1 / p3 / p2 / p4  # Stack them vertically

# Display the combined plot
# print(combined_plot)

# Save the combined plot
ggsave(
  filename = here(figures_dir, str_glue("pre_registered_specification_plot.png")),
  plot = combined_plot,
  width = 10,
  height = 15
)
