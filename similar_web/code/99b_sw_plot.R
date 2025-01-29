# Code to plot gtrends + SW data

# Load required libraries
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(tidyr)
library(patchwork)
library(lubridate)

# Get list of files from Downloads folder
files <- list.files(
  path = "~/Downloads",
  pattern = "age-verification-bsl_data_data_\\d+_\\d+_\\d+\\.csv$", 
  full.names = TRUE
)

# Read and bind all files
combined_data <- files %>%
  lapply(read_csv) %>%
  bind_rows()

# Filter and process traffic data
filter_combined_data <- combined_data %>% 
  tibble() %>% 
  filter(DOMAIN %in% c('pornhub.com', 'xvideos.com'),
         DATE >= '2022-01-01') %>% 
  arrange(DATE) 

# Calculate weekly traffic
weekly_traffic <- filter_combined_data %>%
  mutate(WEEK = floor_date(DATE, unit = "week")) %>%
  group_by(DOMAIN, WEEK) %>%
  summarize(WEEKLY_VISITS = sum(ALL_TRAFFIC_VISITS), .groups = 'drop') %>%
  arrange(WEEK, DOMAIN)

# Create wide format data
wide_traffic <- weekly_traffic %>%
  mutate(DOMAIN = gsub(".com", "", DOMAIN)) %>%
  pivot_wider(
    id_cols = WEEK,
    names_from = DOMAIN,
    values_from = WEEKLY_VISITS
  ) %>%
  rename(
    week = WEEK,
    xvideos = xvideos,
    pornhub = pornhub
  ) %>%
  arrange(week)

# Read Google Trends data
xvideos_trends <- read.csv("~/Downloads/multiTimeline (94).csv") %>% janitor::clean_names()
pornhub_trends <- read.csv("~/Downloads/multiTimeline (93).csv") %>% janitor::clean_names()

# Merge Google Trends data
combined_trends <- merge(xvideos_trends, pornhub_trends, by="week")
combined_trends$week <- as.Date(combined_trends$week, format="%m/%d/%y")
combined_trends <- arrange(combined_trends, week) %>% 
  filter(week >= '2022-01-01')

# Create long format for trends data
long_trends <- pivot_longer(combined_trends, 
                            cols = c("xvideos_worldwide", "pornhub_worldwide"),
                            names_to = "platform",
                            values_to = "traffic")

# Merge all data
merged_data <- wide_traffic %>%
  left_join(combined_trends, by = "week") %>%
  arrange(week) %>% 
  .[-1,]

# Function to calculate correlation and create label
get_correlation_label <- function(data, x_var, y_var) {
  corr <- cor(data[[x_var]], data[[y_var]], use = "complete.obs")
  return(paste0("r = ", round(corr, 3)))
}

# Function to create correlation plots
create_correlation_plot <- function(data, x_var, y_var, title, base_color) {
  corr_label <- get_correlation_label(data, x_var, y_var)
  
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(alpha = 0.6, color = base_color) +
    geom_smooth(method = "lm", color = base_color, 
                fill = base_color, alpha = 0.2) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = title,
         x = "Relative Search Interest (Google Trends)",
         y = "Total Traffic (SimilarWeb)") +
    annotate("text", x = Inf, y = -Inf, 
             label = corr_label,
             hjust = 1.1, vjust = -0.5,
             size = 4) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 12))
}

# Define common date range and breaks for time series
date_limits <- c(min(weekly_traffic$WEEK), max(weekly_traffic$WEEK))
time_series_x_scale <- scale_x_date(
  date_breaks = "6 months",
  date_labels = "%b %Y",
  limits = date_limits
)

# Create total traffic time series
total_traffic_plot <- weekly_traffic[-c(1,2),] %>%
  ggplot(aes(x = WEEK, y = WEEKLY_VISITS, color = DOMAIN)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = label_number(scale = 1/1e6, suffix = "M")) +
  time_series_x_scale +
  scale_color_manual(values = c("pornhub.com" = "#4A90E2", "xvideos.com" = "#FF6B6B")) +
  labs(title = "Total Traffic",
       x = "Date",
       y = "Visits (Millions)",
       color = "Domain") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Create US index time series
us_index_plot <- ggplot(long_trends, aes(x = week, y = traffic, color = platform)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("xvideos_worldwide" = "#FF6B6B", 
                                "pornhub_worldwide" = "#4A90E2"),
                     labels = c("XVideos", "Pornhub")) +
  time_series_x_scale +
  labs(title = "Relative Search Interest",
       x = "Week",
       y = "Interest",
       color = "Platform") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Create correlation plots
xvideos_corr <- create_correlation_plot(merged_data, 
                                        "xvideos_worldwide", 
                                        "xvideos", 
                                        "XVideos", 
                                        "#FF6B6B")
pornhub_corr <- create_correlation_plot(merged_data, 
                                        "pornhub_worldwide", 
                                        "pornhub", 
                                        "Pornhub", 
                                        "#4A90E2")

# Combine all plots
combined_plots <- (total_traffic_plot / us_index_plot) | (xvideos_corr / pornhub_corr) +
  plot_annotation(
    title = "Traffic Analysis: Total vs Relative Search Interest",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14))
  )

# Display the combined plots
combined_plots
