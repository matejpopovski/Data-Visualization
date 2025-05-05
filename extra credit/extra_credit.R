library(tidyverse)
library(gganimate)

# Load and prepare data
ice_data <- read_csv("lake_mendota_ice.csv") %>%
  rename(
    year_range = Category,
    annual_days = `Annual number of days`,
    avg_5yr = `Five-year running average`
  ) %>%
  mutate(
    year_start = as.numeric(str_sub(year_range, 1, 4))
  ) %>%
  drop_na(annual_days) %>%
  arrange(year_start) %>%
  mutate(frame_id = row_number())  # For progressive filtering

# Plot setup
p <- ggplot(ice_data, aes(x = year_start, y = annual_days)) +
  geom_line(color = "blue", linewidth = 0.8) +
  geom_point(color = "red", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1.2, fullrange = TRUE) +
  labs(
    title = 'Lake Mendota Ice Cover Duration',
    subtitle = 'Year: {frame_along}',
    x = 'Winter Starting Year',
    y = 'Annual Ice Cover Duration (days)',
    caption = 'Data: Wisconsin State Climatology Office'
  ) +
  theme_minimal()

# Animate using transition_reveal for line and smooth
animated_plot <- p +
  transition_reveal(along = year_start)

# Render animation
anim <- animate(animated_plot, width = 800, height = 600, duration = 10, renderer = gifski_renderer())
anim