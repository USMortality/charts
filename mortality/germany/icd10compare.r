source("lib/common.r")

data <- read_remote("mortality/Germany/deaths_icd10_year.csv")

df <- data %>%
  mutate(icd10 = left(icd10, 3)) %>%
  group_by(year, icd10) %>%
  summarise(deaths = sum(deaths)) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = deaths)

df <- df %>%
  filter(df$`2019` >= 10000) %>%
  pivot_longer(!icd10, names_to = "year", values_to = "deaths") %>%
  relocate(year)

ggplot(df, aes(x = year, y = deaths, group = icd10, group_name = "ICD-10", color = icd10)) +
  labs(
    title = "",
    subtitle = "",
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
  # twitter_theme() +
  # watermark(df$date, df$cmr) +
  # scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
