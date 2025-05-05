library(tidyverse)
library(gganimate)

# Load and clean the data
ice_data <- read_csv("lake_mendota_ice.csv") %>%
  rename(
    year_range = Category,
    annual_days = `Annual number of days`,
    avg_5yr = `Five-year running average`
  ) %>%
  mutate(
    year_start = as.numeric(str_sub(year_range, 1, 4))
  ) %>%
  drop_na(annual_days)



p <- ggplot(ice_data, aes(x = year_start, y = annual_days)) +
  geom_line(color = "blue", linewidth = 0.6) +
  geom_point(color = "red", size = 2) +
  labs(
    title = 'Lake Mendota Ice Cover: {frame_time}',
    x = 'Winter Starting Year',
    y = 'Annual Ice Cover Duration (days)',
    caption = 'Data: Wisconsin State Climatology Office'
  ) +
  theme_minimal()

animated_plot <- p +
  transition_time(year_start) +
  shadow_mark(past = TRUE, future = FALSE, alpha = 0.5) +  # Keeps past line and points
  ease_aes('linear')

# Render and view
anim <- animate(animated_plot, width = 800, height = 600, duration = 12, renderer = gifski_renderer())
anim