source("lib/common.r")

data <- read_remote("mortality/Germany/deaths_icd10_year.csv")

df <- data %>%
  filter(icd10 == "T881") %>%
  mutate(date = ymd(year, truncated = 2L))

ggplot(df, aes(x = date, y = deaths)) +
  labs(
    title = "T88.1 Deaths [Germany]",
    subtitle = "T88.1 = Other complications following immunization, not elsewhere classified",
    y = "Deaths",
    x = "Year"
  ) +
  # geom_smooth(
  #   data = df %>% filter(year < 2020),
  #   method = "lm_right",
  #   fullrange = TRUE,
  #   se = TRUE,
  #   level = .99,
  #   linetype = "dashed",
  # ) +
  # geom_smooth(
  #   data = df %>% filter(year < 2020),
  #   method = "lm",
  #   fullrange = FALSE,
  #   se = FALSE,
  #   linetype = "solid"
  # ) +
  geom_line(linewidth = 1.2, alpha = 0.7) +
  twitter_theme() +
  watermark(df$date, df$cmr) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
