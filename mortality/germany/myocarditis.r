source("lib/common.r")

data <- read_remote("mortality/Germany/deaths_icd10_year.csv")

codes <- c("I40", "I400", "I401", "I408", "I409", "I514", "I515")
df <- data %>%
  filter(icd10 %in% codes) %>%
  mutate(date = ymd(year, truncated = 2L)) %>%
  group_by(year) %>%
  summarise(deaths = sum(deaths))

ggplot(df, aes(x = year, y = deaths)) +
  labs(
    title = "Myocarditis Deaths [Germany]",
    subtitle = paste(codes, collapse = ", "),
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
  # scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
