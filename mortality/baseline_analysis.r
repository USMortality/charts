source("lib/common.r")

df <- read_remote("mortality/world_baseline.csv")

df |>
  group_by(chart_type, type) |>
  summarize(window = mean)

data <- df |>
  as_tibble() |>
  mutate(window = as.integer(window)) |>
  filter(type == "cmr", !is.na(window)) |>
  select(window)

ggplot(data, aes(x = window)) +
  geom_histogram() +
  labs(
    title = paste0(
      "Distribution of Optimal Baseline Length"
    ),
    subtitle = paste(
      "Data-Source: mortality.watch",
      "Crude Mortality Rate (CMR)",
      "Baseline with lowest RMSE for length of 3-15y",
      sep = " · "
    ),
    x = "Baseline Length (Years)",
    y = "Count"
  ) +
  theme_bw() +
  watermark() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "top", legend.title = element_blank()) +
  scale_x_continuous(
    breaks = min(unique(data$window)):max(unique(data$window))
  )
