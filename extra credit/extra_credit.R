library(tidyverse)
library(gganimate)
library(broom)
library(magick)

# --- Part 1: Basic Reveal Animation ---
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

# Static reveal plot
p1 <- ggplot(ice_data, aes(x = year_start, y = annual_days)) +
  geom_line(color = "blue", linewidth = 0.8) +
  geom_point(color = "red", size = 2) +
  labs(
    title = 'Lake Mendota Ice Cover: {frame_along}',
    x = 'Winter Starting Year',
    y = 'Annual Ice Cover Duration (days)',
    caption = 'Data: Wisconsin State Climatology Office'
  ) +
  theme_minimal()

animated_plot1 <- p1 +
  transition_reveal(along = year_start)

# Save first animation
anim1 <- animate(animated_plot1, width = 800, height = 600, duration = 12, renderer = gifski_renderer())
anim1

# --- Part 2: Evolving Regression Animation ---
ice_data <- ice_data %>% arrange(year_start)

# Generate regression lines over time
regression_lines <- map_dfr(1:nrow(ice_data), function(i) {
  data_so_far <- ice_data[1:i, ]
  model <- lm(annual_days ~ year_start, data = data_so_far)
  data.frame(
    frame_year = ice_data$year_start[i],
    year_start = data_so_far$year_start,
    fitted = predict(model, newdata = data_so_far)
  )
})

ice_data_long <- ice_data %>%
  rename(frame_year = year_start) %>%
  mutate(label_point = TRUE)

p2 <- ggplot() +
  geom_line(data = ice_data, aes(x = year_start, y = annual_days), color = "grey80") +
  geom_point(data = ice_data_long, aes(x = frame_year, y = annual_days, group = 1), color = "red", size = 2) +
  geom_line(data = regression_lines, aes(x = year_start, y = fitted, group = frame_year), color = "black", linewidth = 1.2) +
  labs(
    title = 'Lake Mendota Ice Cover Trend: Up to Year {closest_state}',
    x = 'Winter Starting Year',
    y = 'Ice Cover Duration (days)',
    caption = 'Data: Wisconsin State Climatology Office'
  ) +
  theme_minimal() +
  transition_states(frame_year, transition_length = 1, state_length = 1, wrap = FALSE)

anim2 <- animate(p2, width = 800, height = 600, duration = 15, renderer = gifski_renderer())
anim2


