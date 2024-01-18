source("lib/common.r")
sf <- 4
options(vsc.dev.args = list(width = 600 * sf, height = 335 * sf, res = 72 * 2))

df <- read_remote("mortality/world_baseline.csv")

data <- df |>
  as_tibble() |>
  mutate(window = as.integer(window)) |>
  filter(!is.na(window))

ggplot(data, aes(x = window)) +
  geom_histogram() +
  labs(
    title = paste0(
      "Distribution of Optimal Baseline Length"
    ),
    subtitle = paste(
      "Data-Source: mortality.watch",
      "Baseline lowest RMSE, length of 3-10y",
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
  ) +
  facet_wrap(vars(chart_type, type), scales = "free")
