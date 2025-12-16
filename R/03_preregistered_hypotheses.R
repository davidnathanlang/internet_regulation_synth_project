library(dplyr)
library(readr)
library(here)
library(glue)
pacman::p_load(gsynth)
library(gsynth)
library(tidyverse)
library(tidylog)
library(patchwork)
library(xtable)
library(kableExtra)
pacman::p_load(geofacet)
pacman::p_load(ggrepel)
weeks_in_three_months<-13
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
    filter(time == time_range) 
  
  
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
  three_month_att<- ce_effects %>% filter(rn+1==weeks_in_three_months) %>% transmute(ATT=CATT/weeks_in_three_months) %>% pull()
  
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
    
    three_month <- cumuEff(mod, cumu = TRUE, id = state, c(0, 12))$est.catt %>%
      as_tibble() %>%
      mutate(
        three_month_att = CATT / weeks_in_three_months,
        SE = round(`S.E.` / weeks_in_three_months, 1)
      ) %>%
      slice_tail(n = 1)
    
    att <- three_month$three_month_att
    se  <- three_month$SE
    
    label_txt <- str_c(
      "ATT = ",
      round(att, 1),
      " (",
      se,
      ")"
    )
    
    plot(mod, id = state) +
      labs(
        x = "Time (Weeks)",
        y = "ATT",
        title = str_glue("{state}")
      ) +
      geom_hline(yintercept = att, linetype = "dashed") +
      geom_label(
        data = data.frame(x = -Inf, y = att, label = label_txt),
        aes(x = x, y = y, label = label),
        inherit.aes = FALSE,
        hjust = -0.05,
        fill = scales::alpha("white", 0.7),
        label.size = 0
      ) +
      theme_minimal()
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
      colour = "Age Verification Status" # Update legend title
    ) +
    scale_colour_manual(
      values = c("0" = "red", "1" = "blue"), # Customize colors if desired
      labels = c("0" = "Age Verification Laws Not Passed", "1" = "Age Verification Passed")
    ) +
    scale_x_date(
      date_labels = "'%y", # Display last two digits of the year
      date_breaks = "1 year" # Adjust breaks as necessary
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
#  print(summary(res$mod))
#  res$cum_effects
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
    results[[1]]$cum_effects ,
    results[[2]]$cum_effects ,
    results[[3]]$cum_effects ,
    results[[4]]$cum_effects
  ) %>%  mutate(
    time_point = case_when(
      rn == 4 ~ "1 Month",
      rn == 12 ~ "3 Months"
    )) %>% 
  left_join(pct_change,by=c('topic','rn'))

#ce_pre_registered %>% 
ce_fig <- ce_pre_registered %>% 
  filter(rn %in% c(4, 12)) %>% 
  mutate(topic = factor(topic, levels = c("pornhub", "xvideos", "vpn", "porn"))) %>%
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
ce_fig
# ce_fig
ggsave(filename = here(figures_dir, str_glue("pre-registered_cumulative_effects.png")), plot = ce_fig,width = 9,height = 9)


plots <- lapply(1:4, function(i) {
  results[[i]]$att_plot +
    labs(
      x = "Time (Weeks) Relative to Law Passage",  # New x-axis label
      y = "ATT", # (Average Treatment Effect)",     # New y-axis label
      title = str_glue("Estimated ATT Effect Over Time (Search Term {results[[i]]$search_term})") # Unique title for each plot
    ) +
    theme_minimal() # Optional: Use a clean theme
})

three_month_att<-plots[[1]]$data %>% filter(time>0,time<weeks_in_three_months) %>% summarise(mean(ATT)) %>% pull()
# Assign individual plots for reference
p1 <- plots[[1]] 
p2 <- plots[[2]]
p3 <- plots[[3]]
p4 <- plots[[4]]

# Combine plots using patchwork
combined_plot <- p1 / p3 / p2 / p4 # Stack them vertically

# Display the combined plot
# print(combined_plot)

# Save the combined plot
ggsave(
  filename = here(figures_dir, str_glue("pre_registered_specification_plot.png")),
  plot = combined_plot,
  width = 10,
  height = 15
)

pre_registered_table<-ce_pre_registered%>% filter(rn==12) %>% mutate_at(c("CATT",'CI.lower','CI.upper','S.E.'), ~./weeks_in_three_months)


pretreatment_fit<-
  bind_rows(
    results[[1]]$att_plot$data %>% filter(time<0) %>% summarise(pretreatment_difference=mean(abs(ATT)),topic=results[[1]]$search_term),
    results[[2]]$att_plot$data %>% filter(time<0) %>% summarise(pretreatment_difference=mean(abs(ATT)),topic=results[[2]]$search_term),
    results[[3]]$att_plot$data %>% filter(time<0) %>% summarise(pretreatment_difference=mean(abs(ATT)),topic=results[[3]]$search_term),
    results[[4]]$att_plot$data %>% filter(time<0) %>% summarise(pretreatment_difference=mean(abs(ATT)),topic=results[[4]]$search_term)
  )

latex_table <- pretreatment_fit %>%
  left_join(pre_registered_table, by = 'topic') %>%
  select(topic, pretreatment_difference, CATT, CI.lower, CI.upper, p.value) 

print(xtable(latex_table, caption = "Results Table", label = "tab:results"), include.rownames = FALSE)


cat("\\begin{landscape}
\\begin{table}[ht]
\\centering\n")
xtab_1<-xtable(results[[1]]$mod$wgt.implied)
xtab_1
results[[2]]$mod$wgt.implied %>% round(digits = 3) %>% xtable(caption='Model Weights (Gsynth)',label='vpn_weights')
results[[3]]$mod$wgt.implied %>% round(digits = 3) %>% xtable(caption='Model Weights (Gsynth)',label='xvideos_weights',)
results[[3]]$search_term
oof<-results[[1]]$mod$wgt.implied  /colSums(results[[1]]$mod$wgt.implied)


export_landscape_xtable <- function(data, filename = "table.tex",
                                    caption = "Your Caption",
                                    label = "your_label",
                                    digits = NULL,
                                    align = NULL) {
  # Create xtable object
  xtab <- xtable(data, caption = caption, label = label, digits = digits, align = align)
  
  # Capture the table body
  tab_lines <- capture.output(
    print(xtab,
          include.rownames = TRUE,
          floating = FALSE,
          tabular.environment = "tabular",
          hline.after = c(-1, 0, nrow(data)))
  )
  
  # Combine with LaTeX wrappers
  full_table <- c(
    "\\begin{landscape}",
    "\\begin{table}[ht]",
    "\\centering",
    "\\resizebox{\\textwidth}{!}{%",
    tab_lines,
    "}",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\end{table}",
    "\\end{landscape}"
  )
  
  # Write to file
  writeLines(full_table, filename)
}


individual_states <- c("AL","AR","ID","IN","KS","KY","LA","MS","MT","NC","NE","TX","UT","VA")

plot_data_all <- purrr::map_dfr(seq_along(results), function(i) {
  mod <- results[[i]]$mod
  term<- results[[i]]$search_term
  
  purrr::map_dfr(individual_states, function(st) {
    p <- plot(mod, id = st)
    
    tibble::as_tibble(p$data) %>%
      dplyr::mutate(
        state = st,
        keyword  =term ,
        .before = 1
      )
  })
})



pre_fit_aug <- plot_data_all %>%
  filter(time < 0) %>%
  group_by(state, keyword) %>%
  summarise(
    MAE = mean(abs(ATT), na.rm = TRUE),
    n_pre = sum(!is.na(ATT)),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = state,
    names_from = keyword,
    values_from = c(MAE,  n_pre)
)


# Packages
library(dplyr)
library(knitr)
library(kableExtra)

# Drop redundant n_pre variables and keep one
pre_fit_aug_clean <- pre_fit_aug %>%
  select(
    state,
    MAE_porn,
    MAE_pornhub,
    MAE_vpn,
    MAE_xvideos,
    n_pre = n_pre_porn
  ) %>%
  rename(
    State = state,
    `MAE (Porn)` = MAE_porn,
    `MAE (Pornhub)` = MAE_pornhub,
    `MAE (VPN)` = MAE_vpn,
    `MAE (XVideos)` = MAE_xvideos,
    `Pre-period N` = n_pre
  )

state_avg <- pre_fit_aug_clean %>%
  summarise(
    State = "Macro-level State Average",
    across(
      starts_with("MAE"),
      ~ mean(.x, na.rm = TRUE)
    )
  )

# Create LaTeX table with horizontal rules every 5 rows
kable(
  bind_rows(pre_fit_aug_clean,state_avg),
  format = "latex",
  booktabs = TRUE,
  digits = 2,
caption = "Pre-fit Mean Absolute Error by State and Series\\label{tab:prefit_mae}",
) %>%
  kable_styling(
    latex_options = "hold_position",
    font_size = 11
  ) %>%
  row_spec(
    c(5,10,14),
    hline_after = TRUE
  )




