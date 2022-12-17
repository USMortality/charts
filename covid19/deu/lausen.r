source("lib/common.r")

data <- as_tibble(read.csv("./data/KBV-Datenpakete.csv", sep = ";"))

df <- data %>%
  filter(grepl("R96|R98|R99|I46.1|I46.9", Diagnose)) %>%
  pivot_longer(2:ncol(data)) %>%
  mutate(name = right(name, 5)) %>%
  group_by(name) %>%
  summarise(sum(value, na.rm = TRUE)) %>%
  ungroup()

df2 <- df %>%
  mutate(year = as.numeric(left(name, 4))) %>%
  mutate(quarter = as.numeric(right(name, 1))) %>%
  mutate(date = make_yearquarter(year = year, quarter = quarter)) %>%
  select(5, 2) %>%
  setNames(c("date", "count")) %>%
  as_tsibble(index = date)

training_data <- df2 %>% filter(date < make_yearquarter(year = 2020, quarter = 1))

prediction <- training_data %>%
  model(RW(count ~ drift())) %>%
  forecast(h = 4)
d_2021 <- df2 %>% filter(year(date) == 2021)
excess <- round(sum(d_2021$count) - sum(prediction$.mean))

ggplot(df2, aes(x = date, y = count)) +
  labs(
    title = "Death Diagnostics - R96.*, R98.*, R99.*, I46.1, I46.9",
    subtitle = "Quelle: KBV",
    y = "Diagnosen",
    x = "Quartal"
  ) +
  twitter_theme() +
  geom_col(fill = "#5383EC") +
  geom_text(
    aes(label = round(count)),
    vjust = 2.5, colour = "#ffffff"
  ) +
  geom_smooth(
    data = training_data,
    method = "lm_right",
    fullrange = TRUE,
    se = TRUE,
    level = .95,
    linetype = "dashed",
  ) +
  geom_smooth(
    data = training_data,
    method = "lm",
    fullrange = FALSE,
    se = FALSE,
    linetype = "solid"
  ) +
  scale_x_yearquarter(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  scale_y_continuous(labels = comma_format(decimal.mark = ","))
