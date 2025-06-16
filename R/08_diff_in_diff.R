library(dplyr)
library(readr)
library(here)
library(glue)
library(gsynth)
library(tidyverse)
library(tidylog)
library(patchwork)
library(xtable)
library(kableExtra)
pacman::p_load(geofacet)
pacman::p_load(ggrepel)
pacman::p_load(did)
pacman::p_load(ptetools)
pacman::p_load(staggered)
# Create directories for saving outputs
figures_dir <- here("figures")
models_dir <- here("mods")
dir.create(figures_dir, showWarnings = FALSE)
dir.create(models_dir, showWarnings = FALSE)
keyword<-'xvideos'
seed<-12345
inference<-'parametric'
time_range <- '2022-01-01 2024-10-31'


keyword_df <- read_csv(here::here(str_glue("data/{keyword}.csv"))) %>%
  filter(time == time_range) # Adjust date range as needed
state_num<-keyword_df %>% distinct(state) %>% mutate(state_num=row_number())

start_date<-keyword_df %>% filter(post_treat==1L) %>% group_by(state) %>% summarise(first_treat=as.integer(min(date)-7))

pte_df<-keyword_df %>% left_join(start_date) %>%
  mutate(first_treat=if_else(is.na(first_treat),as.integer(0L),as.integer(first_treat)),date=as.integer(date)) %>% left_join(state_num,by='state') %>%
  mutate(relative_treatment_time=date-first_treat) %>%
  filter(first_treat==0|relative_treatment_time<91)
pte_df
pte_df
did_res <- did::att_gt(
  yname = "hits",
  gname = "first_treat",
  tname = "date",
  idname = "state_num",
  data = pte_df,
  allow_unbalanced_panel = TRUE,
  base_period = 'varying'
  # setup_pte_fun = setup_pte,
  # subset_fun = two_by_two_subset,
  # attgt_fun = did_attgt
  
)


states_treated<-pte_df %>%
  distinct(first_treat, state) %>%
  group_by(first_treat) %>%
  summarise(states_concat = str_flatten(state, collapse = ", "))

states_treated
p1<-ggdid(did_res) 
str(did_res)
group_time_treatment<-
p1$data %>% left_join(states_treated,by=c("group"='first_treat')) %>%
  mutate(state_date=str_c(as.Date(as.integer(as.character(group))),states_concat,sep=" ")) %>%
  mutate(relative_treatment_time=as.integer(group)-as.integer(as.character(year))) %>%
  mutate(calendar_date=as.Date(as.integer(year)))

group_time_treatment$relative_treatment_time
group_time_treatment %>%  filter(relative_treatment_time>=0L & relative_treatment_time<91) %>%
  group_by(state_date) %>%
  summarise(mean(att))
group_time_treatment %>%
  ggplot(aes(x = as.Date(as.integer(as.character(year))), y = att)) +
  geom_errorbar(aes(ymin = att - att.se * c, ymax = att + att.se * c, color = post), width = 5) +
  # Add line or point to show actual estimate if desired
  # geom_point(aes(color = post)) +
  geom_vline(aes(xintercept = as.Date(as.integer(as.character(group)))
                 ),
             linetype='dotted'
             )+
  facet_wrap(~state_date, scales = "free_y") +
  scale_x_date(date_breaks = '6 months', date_labels = "%Y-%m") +
  scale_color_manual(values = c("red", "steelblue")) +
  
  labs(
    x = "Date",
    y = "ATT",
    color = "Post-treatment",
    title = str_glue("ATT ({keyword}) Over Time by State and Date")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


dyn_did<-aggte(did_res,type='dynamic',min_e=-Inf,max_e = 84,na.rm = TRUE) %>% ggdid()
dyn_did$data
dyn_did$data$year<-dyn_did$data$year/7
dyn_did+scale_x_continuous()
gg_group<-aggte(did_res,type='group',min_e=-Inf,max_e=84,na.rm=TRUE)
ggdid(gg_group)
?aggte()
aggte(did_res,type='group',min_e = 91,na.rm=TRUE)



pte_df %>% distinct(first_treat,state) %>%
  group_by(first_treat) %>% 
  summarise(states=reduce(state,str_c(sep=' , ')))
gg<-ggdid(dyn_group)

gg$data
dyn_simple<-aggte(did_res,type='simple',min_e=-Inf,max_e = 84,bstrap = TRUE,cband=TRUE,na.rm = TRUE)
dyn_simple$overall.se*1.96+dyn_simple$overall.att
str(dyn_simple)
?aggte
dyn_simple$overall.att
plot_data<-gg$data %>% left_join(states_treated,by=c('year'='first_treat')) %>%
  mutate(dates_plus_states=str_c(as_date(year), states_concat))
overall_effect<-tibble(att=dyn_simple$overall.att,att.se=dyn_simple$overall.se,c=1.96,dates_plus_states="Average")
plot_data
plot_data %>% 
  bind_rows(overall_effect) %>%
  ggplot(aes(x=dates_plus_states, states_concat,y=att))+
  geom_point()+
  geom_errorbar(aes(ymin = att-att.se*c,ymax=att+att.se*c,y=att))  +coord_flip()+
  geom_hline(aes(yintercept = 0))+
  labs(title = keyword_df$term)


dyn_group$egt %>% left_join(states_treated,by=c('Group'='states_concat'))
ggdid(dyn_did) +
  scale_x_date(
    date_breaks = "2 weeks",           # or "1 month", depending on your data
    date_labels = "%b %d"              # e.g., Jan 01, or use "%Y-%m" for year-month
  ) 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Date", y = "Average Effect", color = "Group")
ggdid(dyn_simple)
dyn
did_res
did_res2 <- did::att_gt(
  yname = "hits",
  gname = "first_treat",
  tname = "date",
  idname = "state_lookup",
  data = pte_df %>% left_join(pte_labels)
  
)
mod<-summary(dyn_simple)

remotes::install_github("tjhon/ssynthdid")
pacman::p_load(bacondecomp)
pte_labels<-pte_df %>% distinct(state) %>% mutate(state_lookup=row_number())
fixest::feols(hits~sunab(post_treat|state+date|state,data=pte_df))
pacman::p_load(fixest)
staggered(df=pte_df,
          y="hits",
          g='first_treat',
          t='date',
          i='state',
          estimand = 'simple'
          
          )
summary(did_res)
ggpte(did_res)
df <- staggered::pj_officer_level_balanced #load the officer data

Simple aggregate parameters
data(mdpta)
# Hyperparameter search function
hyperparameter_search <- function(keyword, time_range = '2022-01-01 2024-10-31', seed = 12345, inference = "parametric") {
  # Load data

  pte(hits,geo,date,)
  keyword_df
  
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
        x = "Time (Weeks) ",  # New x-axis label
        y = "ATT", # (Average Treatment Effect)",     # New y-axis label
        title = str_glue("{state}") # Unique title for each plot
      ) +
      geom_hline(yintercept = thre_month_est,linetype='dashed')+
      annotate("text", x = -25, y = thre_month_est+2, label = str_c("ATT = ",round(thre_month_est,1))) +
      theme_minimal() # Optional: Use a clean theme
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

library(staggered) #load the staggered package
library(ggplot2)   #load ggplot2 for plotting the results
#> Warning: package 'ggplot2' was built under R version 4.3.1

df <- staggered::pj_officer_level_balanced #load the officer data
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

three_month_att<-plots[[1]]$data %>% filter(time>0,time<13) %>% summarise(mean(ATT)) %>% pull()
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

pre_registered_table<-ce_pre_registered%>% filter(rn==12) %>% mutate_at(c("CATT",'CI.lower','CI.upper','S.E.'), ~./13)


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
