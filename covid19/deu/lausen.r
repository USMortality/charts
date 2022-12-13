source("lib/common.r")

data <- as_tibble(read.csv("./data/KBV-Datenpakete.csv", sep = ";"))

df <- data %>%
  filter(grepl("R96|R98|R99|I46.1|I46.9", Diagnose)) %>%
  pivot_longer(2:ncol(data)) %>%
  group_by(name) %>%
  summarise(sum(value, na.rm = TRUE)) %>%
  ungroup()

df <- df %>%
  filter(left(name, 1) == "X") %>%
  mutate(year = as.numeric(mid(name, 2, 4))) %>%
  mutate(quarter = as.numeric(right(name, 1))) %>%
  mutate(date = make_yearquarter(year = year, quarter = quarter)) %>%
  select(5, 2) %>%
  setNames(c("date", "count")) %>%
  as_tsibble(index = date)

training_data <- df %>% filter(date < make_yearquarter(year = 2020, quarter = 1))

ggplot(df, aes(x = date, y = count)) +
  labs(
    title = "R96.*, R98.*, R99.*, I46.1, I46.9 - Kohorte 1",
    subtitle = "Kohorte 1 = Versicherte, die in 2021 eine ICD-Kodierung zu Impfnebenwirkung hatten.",
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
