classify_mean_sd <- function(x) {
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  
  case_when(
    x >= mean_val - 1 * sd_val & x < mean_val - 0 * sd_val ~ "Mean - 1 SD",
    x >= mean_val + 0 * sd_val & x < mean_val + 1 * sd_val ~ "Mean + 1 SD",
    x >= mean_val - 2 * sd_val & x < mean_val - 1 * sd_val ~ "Mean - 2 SD",
    x >= mean_val + 1 * sd_val & x < mean_val + 2 * sd_val ~ "Mean + 2 SD",
    x >= mean_val - 3 * sd_val & x < mean_val - 2 * sd_val ~ "Mean - 3 SD",
    x >= mean_val + 2 * sd_val & x < mean_val + 3 * sd_val ~ "Mean + 3 SD",
    x < mean_val - 3 * sd_val ~ "Beyond Mean - 3 SD",
    x > mean_val + 3 * sd_val ~ "Beyond Mean + 3 SD"
  )
}

pop <- tibble(value = rnorm(10000, 5,1))

# Compute density with the same x-axis range as pop
pop_density <- density(pop$value, n = 1000)  # Increased resolution

density_df <- tibble(
  value = pop$value,  # Keep original x-axis values
  density = approx(pop_density$x, pop_density$y, xout = pop$value)$y
) %>%
  mutate(range = classify_mean_sd(value))

# Plot the density distribution
ggplot(density_df, aes(x = value, y = density, fill = range)) +
  geom_area(position = "identity", alpha = 0.6)



ggplot(density_df, aes(x = value, y = density, fill = range, group = range)) +
  geom_polygon(aes(group = range, x = c(value, rev(value)), 
                   y = c(density, rep(0, length(density)))), alpha = 0.6) +
  scale_fill_manual(values = c(
    "Mean - 1 SD" = "blue",
    "Mean + 1 SD" = "blue",
    "Mean - 2 SD" = "green",
    "Mean + 2 SD" = "green",
    "Mean - 3 SD" = "yellow",
    "Mean + 3 SD" = "yellow",
    "Beyond Mean - 3 SD" = "red",
    "Beyond Mean + 3 SD" = "red"
  )) +
  labs(title = "Density Distribution with Mean ± SD Shading") +
  theme_minimal()
