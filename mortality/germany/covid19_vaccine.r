source("lib/common.r")

data <- read_remote("mortality/Germany/deaths_icd10_year.csv")

df <- data %>%
  filter(icd10 %in% c("T881", "U129")) %>%
  group_by(year) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(date = ymd(year, truncated = 2L))

ggplot(df, aes(x = date, y = deaths)) +
  labs(
    title = "Vaccine Deaths [Germany]",
    subtitle = "ICD-10: U12.9 & T88.1; Source: Destatis",
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
  geom_col(linewidth = 1.2, alpha = 0.7, fill = "#ff0000") +
  twitter_theme() +
  watermark(df$date, df$cmr) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
