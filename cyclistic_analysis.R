##### PREPARATION #####
# Install Packages
install.packages("tidyverse")
install.packages("janitor")
install.packages("skimr")
install.packages("reactable")

library(tidyverse)
library(janitor)
library(dplyr)
library(lubridate)
library(skimr)
library(ggplot2)
library(reactable)

# Table Theming
teal_reactable_theme <- reactableTheme(
  backgroundColor = "#FFFFFF",             # Table background
  color = "#367177",                       # Body text: Soft Dark Teal
  stripedColor = "#EAF6F7",                # Very soft teal stripe (use a very light teal like #EAF6F7)
  borderColor = "#43939A",                 # Table borders: Teal Blue
  highlightColor = "#E8F23855",            # Subtle lime highlight for selected/hovered rows
  headerStyle = list(
    background = "#43939A",                # Header bar: Teal Blue
    color = "#FFFFFF",                     # Header text: White
    fontWeight = "bold",
    fontSize = "1.1em",
    borderBottom = "2px solid #43939A"
  ),
  cellStyle = list(
    background = "#FFFFFF",                # All cell backgrounds white
    color = "#367177",
    fontSize = "1.03em"
  ),
  tableStyle = list(
    fontFamily = "Segoe UI, Verdana, Arial, sans-serif"
  )
)

# Set working directory
setwd('C:/Users/James/Desktop/Cyclistic')

# Import data
data_2024_09 <- read_csv('202409-divvy-tripdata.csv')
data_2024_10 <- read_csv('202410-divvy-tripdata.csv')
data_2024_11 <- read_csv('202411-divvy-tripdata.csv')
data_2024_12 <- read_csv('202412-divvy-tripdata.csv')
data_2025_01 <- read_csv('202501-divvy-tripdata.csv')
data_2025_02 <- read_csv('202502-divvy-tripdata.csv')
data_2025_03 <- read_csv('202503-divvy-tripdata.csv')
data_2025_04 <- read_csv('202504-divvy-tripdata.csv')
data_2025_05 <- read_csv('202505-divvy-tripdata.csv')
data_2025_06 <- read_csv('202506-divvy-tripdata.csv')
data_2025_07 <- read_csv('202507-divvy-tripdata.csv')
data_2025_08 <- read_csv('202508-divvy-tripdata.csv')
data_2025_09 <- read_csv('202509-divvy-tripdata.csv')

dataframes <- list(data_2024_09, data_2024_10, data_2024_11,
                   data_2024_12, data_2025_01, data_2025_02,
                   data_2025_03, data_2025_04, data_2025_05,
                   data_2025_06, data_2025_07, data_2025_08,
                   data_2025_09)

# Check for column mismatches
compare_df_cols(dataframes, return = "mismatch")

# Combine dataframes
combined_df <- bind_rows(dataframes)

# Summary stats
str(combined_df)
glimpse(combined_df)

##### CLEANING #####
# Check for null values
colSums(is.na(combined_df))
round(sapply(combined_df, function(x) mean(is.na(x))) * 100, 2)

# Check for duplicate rows
dupes <- get_dupes(combined_df)

# Set datatypes
combined_df$started_at <- as_datetime(combined_df$started_at)
combined_df$ended_at <- as_datetime(combined_df$ended_at)

# Check to ensure dates lie within assumed boundaries
start_range <- ymd("2024-09-01")
end_range   <- ymd("2025-09-30")

combined_df <- combined_df %>%
  filter(started_at >= start_range & started_at <= end_range)

# Check categorical variables
unique(combined_df$rideable_type)
unique(combined_df$member_casual)

# Sanitise categorical variables
combined_df <- combined_df %>%
  mutate(
    rideable_type = str_to_lower(trimws(rideable_type)),
    member_casual = str_to_lower(trimws(member_casual))
  ) %>%
  mutate(
    member_casual = recode(member_casual,
                                 "member" = "Member",
                                 "casual" = "Casual"),
    rideable_type = recode(rideable_type,
                               "classic_bike" = "Classic Bike",
                               "electric_bike" = "Electric Bike",
                               "electric_scooter" = "Electric Scooter")
  )

tabyl(combined_df$member_casual)

# Remove impossible trip durations
combined_df <- combined_df %>%
  mutate(duration = as.numeric(ended_at - started_at)) %>%
  filter(duration > 0)

summary(combined_df$duration)

# Check proportion of rides below 30 and 60 seconds
combined_df %>%
  summarise(
    below_30 = mean(duration < 30, na.rm = TRUE),
    below_60 = mean(duration < 60, na.rm = TRUE)
  )

# Remove rides less than 60 seconds and greater than 24 hours in duration
combined_df <- combined_df %>%
  filter(duration >= 60 & duration <= 86400)

# Drop empty rows and columns, and rows with null values
combined_df <- combined_df %>% 
  remove_empty(which = c("rows", "cols"))  %>% 
  drop_na()

# Create columns
combined_df <- combined_df %>% 
  mutate(day_of_the_week = weekdays(as.Date(combined_df$started_at)))

# Check for outliers
# Define a helper function to detect outliers
find_outliers <- function(x) {
  q1 <- quantile(x, 0.05, na.rm = TRUE)
  q3 <- quantile(x, 0.95, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  return(x < lower | x > upper)
}

outliers_df <- combined_df %>%
  filter(find_outliers(duration))

print(outliers_df)

# Filter out improbable coordinates (Based on Chicago city boundaries)
combined_df <- combined_df %>% 
  filter(
    between(start_lat, 41.4, 42.3),
    between(start_lng, -88.3, -87.3),
    between(end_lat, 41.4, 42.3),
    between(end_lng, -88.3, -87.3)
  )

# Add month and season columns
combined_df <- combined_df %>%
  mutate(month = lubridate::month(started_at, label = TRUE),
         season = case_when(
           month %in% c("Dec", "Jan", "Feb") ~ "Winter",
           month %in% c("Mar", "Apr", "May") ~ "Spring",
           month %in% c("Jun", "Jul", "Aug") ~ "Summer",
           month %in% c("Sep", "Oct", "Nov") ~ "Fall"
         ))

write_csv(combined_df, "cleaned_combined_df.csv")

##### ANALYSIS #####
summary(combined_df$duration)
skim(combined_df$duration)

# Duration Breakdown
combined_df <- combined_df %>%
  mutate(
    duration_mins = duration / 60,
    duration_bucket = cut(duration_mins,
                          breaks = c(0, 10, 20, 30, 60, 90, 120, 180, Inf),
                          labels = c("0-10", "11-20", "21-30", "31-60", "61-90", "91-120", "121-180", "180+"),
                          right = TRUE, include.lowest = TRUE
    )
  )

duration_summary <- combined_df %>%
  count(member_casual, duration_bucket) %>%
  group_by(member_casual) %>%
  mutate(percent = round(100 * n / sum(n), 2)) %>%
  ungroup()

reactable(
  duration_summary,
  columns = list(
    member_casual = colDef(name = "User Type"),
    duration_bucket = colDef(name = "Trip Duration (mins)"),
    n = colDef(name = "Number of Rides"),
    percent = colDef(name = "Percent within User Type", format = colFormat(suffix = "%", digits = 2))
  ),
  striped = TRUE,
  bordered = TRUE,
  theme = teal_reactable_theme
)

# User type breakdown
combined_df %>%
  count(member_casual) %>%
  mutate(percent = round(100 * n / sum(n), 2)) %>%
  reactable(
    columns = list(
      member_casual = colDef(name = "User Type"),
      n = colDef(name = "Number of Rides"),
      percent = colDef(
        name = "Percent of Total",
        format = colFormat(suffix = "%", digits = 2)
      )
    ),
    striped = TRUE,
    bordered = TRUE,
    theme = teal_reactable_theme
  )

# Bike type by user type
combined_df %>%
  count(rideable_type, member_casual) %>%
  group_by(member_casual) %>%
  mutate(percent = round(100 * n / sum(n), 2)) %>%
  ungroup() %>%
  reactable(
    columns = list(
      rideable_type = colDef(name = "Bike Type"),
      member_casual = colDef(name = "User Type"),
      n = colDef(name = "Number of Rides"),
      percent = colDef(
        name = "Percent within Group",
        format = colFormat(suffix = "%", digits = 2)
      )
    ),
    striped = TRUE,
    bordered = TRUE,
    theme = teal_reactable_theme
  )

# Duration Descriptive Summary
combined_df %>%
  group_by(member_casual) %>%
  summarise(
    min = round(min(duration/60, na.rm = TRUE), 2),
    Q1 = round(quantile(duration/60, 0.25, na.rm = TRUE), 2),
    median = round(median(duration/60, na.rm = TRUE), 2),
    mean = round(mean(duration/60, na.rm = TRUE), 2),
    Q3 = round(quantile(duration/60, 0.75, na.rm = TRUE), 2),
    max = round(max(duration/60, na.rm = TRUE), 2),
    sd = round(sd(duration/60, na.rm = TRUE), 2),
    IQR = round(IQR(duration/60, na.rm = TRUE), 2)
  ) %>%
  reactable(
    columns = list(
      member_casual = colDef(name = "User Type"),
      min = colDef(name = "Min (mins)"),
      Q1 = colDef(name = "Q1 (mins)"),
      median = colDef(name = "Median (mins)"),
      mean = colDef(name = "Mean (mins)"),
      Q3 = colDef(name = "Q3 (mins)"),
      max = colDef(name = "Max (mins)"),
      sd = colDef(name = "SD (mins)"),
      IQR = colDef(name = "IQR (mins)")
    ),
    striped = TRUE,
    bordered = TRUE,
    theme = teal_reactable_theme
  )

# By day of week
combined_df %>%
  mutate(day_of_week = wday(started_at, label = TRUE)) %>%
  count(member_casual, day_of_week) %>%
  group_by(member_casual) %>%
  mutate(percent = round(100 * n / sum(n), 2)) %>%
  ungroup() %>%
  reactable(
    columns = list(
      member_casual = colDef(name = "User Type"),
      day_of_week = colDef(name = "Day of Week"),
      n = colDef(name = "Number of Rides"),
      percent = colDef(name = "Percent within Group")
    ),
    striped = TRUE,
    bordered = TRUE,
    theme = teal_reactable_theme
  )

# By hour of day
combined_df %>%
  mutate(hour = hour(started_at)) %>%
  count(member_casual, hour) %>%
  group_by(member_casual) %>%
  mutate(percent = round(100 * n / sum(n), 2)) %>%
  ungroup() %>%
  (\(df) reactable(
    df,
    columns = list(
      member_casual = colDef(name = "User Type"),
      hour = colDef(name = "Hour of Day"),
      n = colDef(name = "Number of Rides"),
      percent = colDef(name = "Percent within Group")
    ),
    striped = TRUE,
    bordered = TRUE,
    theme = teal_reactable_theme,
    pagination = TRUE,
    defaultPageSize = nrow(df),
    showPageSizeOptions = FALSE
  ))()

# Most popular start stations
combined_df %>%
  group_by(start_station_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  reactable(
    columns = list(
      start_station_name = colDef(name = "Start Station"),
      n = colDef(name = "Number of Rides")
    ),
    striped = TRUE,
    bordered = TRUE,
    theme = teal_reactable_theme
  )

# Most popular end stations
combined_df %>%
  group_by(end_station_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  reactable(
    columns = list(
      end_station_name = colDef(name = "End Station"),
      n = colDef(name = "Number of Rides")
    ),
    striped = TRUE,
    bordered = TRUE,
    theme = teal_reactable_theme
  )

# Top start-end routes
combined_df %>%
  group_by(start_station_name, end_station_name) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  reactable(
    columns = list(
      start_station_name = colDef(name = "Start Station"),
      end_station_name = colDef(name = "End Station"),
      n = colDef(name = "Number of Rides")
    ),
    striped = TRUE,
    bordered = TRUE,
    theme = teal_reactable_theme
  )

##### BUILD VISUALISATIONS #####
# By day of week (% of total rides)
total_rides <- nrow(combined_df)

day_df <- combined_df %>%
  mutate(day_of_week = wday(started_at, label = TRUE)) %>%
  mutate(member_casual = recode(member_casual,
                                "member" = "Member",
                                "casual" = "Casual")) %>% 
  count(member_casual, day_of_week) %>%
  mutate(percent_total = 100 * n / total_rides)

ggplot(day_df, aes(x = day_of_week, y = percent_total, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Member" = "#43939A", "Casual" = "#E8F238")) +
  labs(
    title = "Rides by Day of Week (Percent of Total Rides)",
    x = "Day of Week",
    y = "Percent of Total Rides",
    fill = "User Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "#367177", face = "bold"),
    axis.title = element_text(color = "#43939A"),
    axis.text = element_text(color = "#367177"),
    panel.background = element_rect(fill = "#FFFFFF", color = NA)
  )

# By hour of day (% of total rides)
hour_df <- combined_df %>%
  mutate(hour = hour(started_at)) %>%
  mutate(member_casual = recode(member_casual,
                                "member" = "Member",
                                "casual" = "Casual")) %>% 
  count(member_casual, hour) %>%
  mutate(percent_total = 100 * n / total_rides)

# Line plot
ggplot(hour_df, aes(x = hour, y = percent_total, color = member_casual, group = member_casual)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Member" = "#43939A", "Casual" = "#E8F238")) +
  labs(
    title = "Rides by Hour of Day (Percent of Total Rides)",
    x = "Hour of Day",
    y = "Percent of Total Rides",
    color = "User Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "#367177", face = "bold"),
    axis.title = element_text(color = "#43939A"),
    axis.text = element_text(color = "#367177"),
    panel.background = element_rect(fill = "#FFFFFF", color = NA)
  )

# Bar plot alternative
ggplot(hour_df, aes(x = factor(hour), y = percent_total, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Member" = "#43939A", "Casual" = "#E8F238")) +
  labs(
    title = "Rides by Hour of Day (Percent of Total Rides)",
    x = "Hour of Day",
    y = "Percent of Total Rides",
    fill = "User Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "#367177", face = "bold"),
    axis.title = element_text(color = "#43939A"),
    axis.text = element_text(color = "#367177"),
    panel.background = element_rect(fill = "#FFFFFF", color = NA)
  )

# By day of week (% within user type)
day_df <- combined_df %>%
  mutate(day_of_week = wday(started_at, label = TRUE)) %>%
  mutate(member_casual = recode(member_casual,
                               "member" = "Member",
                               "casual" = "Casual")) %>% 
  count(member_casual, day_of_week) %>%
  group_by(member_casual) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup()

# Bar Plot
ggplot(day_df, aes(x = day_of_week, y = percent, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Member" = "#43939A", "Casual" = "#E8F238")) +
  labs(
    title = "Rides by Day of Week",
    x = "Day of Week",
    y = "Percent within User Type",
    fill = "User Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "#367177", face = "bold"),
    axis.title = element_text(color = "#43939A"),
    axis.text = element_text(color = "#367177"),
    panel.background = element_rect(fill = "#FFFFFF", color = NA)
  )

# By hour of day (% within user type)
hour_df <- combined_df %>%
  mutate(hour = hour(started_at)) %>%
  mutate(member_casual = recode(member_casual,
                                "member" = "Member",
                                "casual" = "Casual")) %>% 
  count(member_casual, hour) %>%
  group_by(member_casual) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup()

# Line plot
ggplot(hour_df, aes(x = hour, y = percent, color = member_casual, group = member_casual)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Member" = "#43939A", "Casual" = "#E8F238")) +
  labs(
    title = "Rides by Hour of Day",
    x = "Hour of Day",
    y = "Percent within User Type",
    color = "User Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "#367177", face = "bold"),
    axis.title = element_text(color = "#43939A"),
    axis.text = element_text(color = "#367177"),
    panel.background = element_rect(fill = "#FFFFFF", color = NA)
  )

# Stacked / Grouped Bar Plot
ggplot(hour_df, aes(x = factor(hour), y = percent, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Member" = "#43939A", "Casual" = "#E8F238")) +
  labs(
    title = "Rides by Hour of Day",
    x = "Hour of Day",
    y = "Percent within User Type",
    fill = "User Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "#367177", face = "bold"),
    axis.title = element_text(color = "#43939A"),
    axis.text = element_text(color = "#367177"),
    panel.background = element_rect(fill = "#FFFFFF", color = NA)
  )

# Most Popular Start Stations
start_stations <- combined_df %>%
  count(start_station_name) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

ggplot(start_stations, aes(x = reorder(start_station_name, n), y = n)) +
  geom_col(fill = "#43939A") +
  coord_flip() +
  labs(title = "Top 10 Start Stations",
       x = "Start Station", y = "Number of Rides") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "#367177", face = "bold"),
    axis.title = element_text(color = "#43939A"),
    axis.text = element_text(color = "#367177"),
    panel.background = element_rect(fill = "#FFFFFF", color = NA)
  )

# Most Popular End Stations
end_stations <- combined_df %>%
  count(end_station_name) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

ggplot(end_stations, aes(x = reorder(end_station_name, n), y = n)) +
  geom_col(fill = "#43939A") +
  coord_flip() +
  labs(title = "Top 10 End Stations",
       x = "End Station", y = "Number of Rides") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "#367177", face = "bold"),
    axis.title = element_text(color = "#43939A"),
    axis.text = element_text(color = "#367177"),
    panel.background = element_rect(fill = "#FFFFFF", color = NA)
  )

# By User Type
user_types <- combined_df %>%
  mutate(member_type_display = recode(
    member_casual,
    "member" = "Member",
    "casual" = "Casual"
  )) %>%
  count(member_type_display) %>%
  mutate(percent = 100 * n / sum(n))

ggplot(user_types, aes(x = member_type_display, y = percent, fill = member_type_display)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  scale_fill_manual(values = c("Member" = "#43939A", "Casual" = "#E8F238")) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), vjust = -0.4, color = "#367177", size = 5, fontface = "bold") +
  labs(title = "User Type Breakdown", x = NULL, y = "Percent of Total") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "#367177", face = "bold"),
    axis.title = element_text(color = "#43939A"),
    axis.text = element_text(color = "#367177"),
    panel.background = element_rect(fill = "#FFFFFF", color = NA)
  )

# By Bike Type
bike_types <- combined_df %>%
  mutate(
    member_type_display = recode(member_casual, "member" = "Member", "casual" = "Casual"),
    bike_type_display = recode(rideable_type, "classic_bike" = "Classic Bike", "electric_bike" = "Electric Bike", "electric_scooter" = "Electric Scooter")
  ) %>%
  count(bike_type_display, member_type_display) %>%
  group_by(member_type_display) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup()

ggplot(bike_types, aes(x = bike_type_display, y = percent, fill = member_type_display)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Member" = "#43939A", "Casual" = "#E8F238")) +
  labs(title = "Bike Type by User Type", x = "Bike Type", y = "Percent within User Type", fill = "User Type") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "#367177", face = "bold"),
    axis.title = element_text(color = "#43939A"),
    axis.text = element_text(color = "#367177"),
    panel.background = element_rect(fill = "#FFFFFF", color = NA)
  )

# By Duration
ggplot(duration_summary, aes(x = duration_bucket, y = percent, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Member" = "#43939A", "Casual" = "#E8F238")) +
  labs(
    title = "Trip Duration Distribution by User Type",
    x = "Trip Duration (mins bucket)",
    y = "Percent within User Type",
    fill = "User Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "#367177", face = "bold"),
    axis.title = element_text(color = "#43939A"),
    axis.text = element_text(color = "#367177"),
    panel.background = element_rect(fill = "#FFFFFF", color = NA)
  )

ggplot(combined_df, aes(x = duration_mins, color = member_casual)) +
  stat_ecdf(geom = "step", size = 1.2) +
  xlim(0, 120) +
  scale_color_manual(values = c("Member" = "#43939A", "Casual" = "#E8F238")) +
  labs(
    title = "Cumulative Distribution of Trip Duration",
    x = "Trip Duration (minutes)",
    y = "Cumulative Proportion",
    color = "User Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "#367177", face = "bold"),
    axis.title = element_text(color = "#43939A"),
    axis.text = element_text(color = "#367177"),
    panel.background = element_rect(fill = "#FFFFFF", color = NA)
  )

# Seasonality plot by month
ggplot(combined_df, aes(x = month, fill = member_casual)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Member" = "#43939A", "Casual" = "#E8F238")) +
  labs(
    title = "Monthly Bike Usage by User Type",
    x = "Month",
    y = "Number of Rides",
    fill = "User Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "#367177", face = "bold"),
    axis.title = element_text(color = "#43939A"),
    axis.text = element_text(color = "#367177"),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    legend.title = element_text(color = "#43939A"),
    legend.text = element_text(color = "#367177")
  )

# Group and summarize by season
season_summary <- combined_df %>%
  group_by(season, member_casual) %>%
  summarise(
    rides = n(),
    avg_duration = mean(duration / 60, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(season_summary, aes(x = season, y = rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Member" = "#43939A", "Casual" = "#E8F238")) +
  labs(
    title = "Seasonal Bike Usage by User Type",
    x = "Season",
    y = "Total Rides",
    fill = "User Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "#367177", face = "bold"),
    axis.title = element_text(color = "#43939A"),
    axis.text = element_text(color = "#367177"),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    legend.title = element_text(color = "#43939A"),
    legend.text = element_text(color = "#367177")
  )
