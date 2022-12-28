source("lib/common.r")

data <- read_remote("mortality/Germany/deaths_icd10_year.csv")

# By Year
df <- data %>%
  filter(icd10 %in% c("T881")) %>%
  as_tsibble(index = year) %>%
  fill_gaps(deaths = 0) %>%
  mutate(date = ymd(year, truncated = 2L))

ggplot(df, aes(x = date, y = deaths)) +
  labs(
    title = "Other Immunization Deaths [Germany]",
    subtitle = "T88.1, Underlying Cause of Deaths; Source: German Federal Statistical Office (Destatis)",
    y = "Deaths",
    x = "Year"
  ) +
  geom_col(linewidth = 1.2, alpha = 0.7, fill = "#ff0000") +
  twitter_theme() +
  watermark(df$date, df$cmr) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))

# Pre 2020 vs 2021
pre_2021 <- df %>%
  as_tibble() %>%
  filter(year < 2021) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE))
d_2021 <- df %>%
  filter(year == 2021)
df2 <- as_tibble(data.frame(
  year = c("2003-2020", "2021"), deaths = c(pre_2021$deaths, d_2021$deaths)
))

ggplot(df2, aes(x = year, y = deaths)) +
  labs(
    title = "Other Immunization Deaths [Germany]",
    subtitle = "T88.1, Underlying Cause of Deaths; Source: German Federal Statistical Office (Destatis)",
    y = "Deaths",
    x = "Years"
  ) +
  geom_col(linewidth = 1.2, alpha = 0.7, fill = "#ff0000") +
  geom_text(
    aes(label = deaths),
    vjust = 1.5, colour = "#000000"
  ) +
  twitter_theme() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))


# Pre-Pandemic
training_set <- df %>%
  filter(year >= 2010) %>%
  filter(year < 2020)

df3 <- df
df3[19, ]$deaths <- NA
model <- training_set %>%
  model(RW(deaths ~ drift())) %>%
  forecast(h = 2)

hilo(model, level = 99)

model %>%
  autoplot(df3, level = 99) +
  geom_smooth(
    mapping = aes(y = deaths),
    data = training_set,
    method = "lm",
    fullrange = FALSE,
    se = FALSE,
    linetype = "dashed"
  ) +
  labs(
    title = "Pre-Pandemic - Other Immunization Deaths [Germany]",
    subtitle = "T88.1, Underlying Cause of Deaths; Source: German Federal Statistical Office (Destatis)",
    y = "Deaths",
    x = "Year"
  ) +
  twitter_theme()


# By Age Group
data <- read_remote("mortality/Germany/deaths_icd10_year_age.csv")

df <- data %>%
  filter(icd10 == "T881") %>%
  mutate(date = ymd(year, truncated = 2L))

ggplot(df, aes(x = date, y = deaths, groups = age_group, group_name = age_group, color = age_group)) +
  labs(
    title = "Other Immunization Deaths [Germany]",
    subtitle = "T88.1, Underlying Cause of Deaths; Source: German Federal Statistical Office (Destatis)",
    y = "Deaths",
    x = "Year"
  ) +
  geom_line() +
  twitter_theme() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  facet_wrap(~age_group)

# By Sex
data <- read_remote("mortality/Germany/deaths_icd10_year_sex.csv")

df <- data %>%
  filter(icd10 == "T881") %>%
  mutate(date = ymd(year, truncated = 2L))

ggplot(df, aes(x = date, y = deaths, groups = sex, group_name = sex, color = sex)) +
  labs(
    title = "Other Immunization Deaths [Germany]",
    subtitle = "T88.1, Underlying Cause of Deaths; Source: German Federal Statistical Office (Destatis)",
    y = "Deaths",
    x = "Year"
  ) +
  geom_line() +
  twitter_theme() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  facet_wrap(~sex)
