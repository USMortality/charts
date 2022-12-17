source("lib/common.r")

data <- read_remote("mortality/Germany/deaths_icd10_year.csv")

# By Year
df <- data %>%
  filter(icd10 %in% c("T881", "U129")) %>%
  group_by(year) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(date = ymd(year, truncated = 2L))

ggplot(df, aes(x = date, y = deaths)) +
  labs(
    title = "Vaccine Deaths [Germany]",
    subtitle = "ICD-10: U12.9 & T88.1; Source: German Federal Statistical Office (Destatis.de)",
    y = "Deaths",
    x = "Year"
  ) +
  geom_col(linewidth = 1.2, alpha = 0.7, fill = "#ff0000") +
  twitter_theme() +
  watermark(df$date, df$cmr) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))


# Pre-Pandemic
training_set <- df %>%
  filter(year >= 2010) %>%
  filter(year < 2020)

df2 <- df %>%
  filter(year < 2020) %>%
  add_row(year = 2020, deaths = NA, date = ymd("2020/1/1")) %>%
  add_row(year = 2021, deaths = NA, date = ymd("2021/1/1"))

ggplot(df2, aes(x = date, y = deaths)) +
  labs(
    title = "Pre-Pandemic Vaccine Deaths [Germany]",
    subtitle = "ICD-10: U12.9 & T88.1; Source: German Federal Statistical Office (Destatis.de)",
    y = "Deaths",
    x = "Year"
  ) +
  geom_smooth(
    data = training_set,
    method = "lm_right",
    fullrange = TRUE,
    se = TRUE,
    level = .95,
    linetype = "dashed",
  ) +
  geom_smooth(
    data = training_set,
    method = "lm",
    fullrange = FALSE,
    se = FALSE,
    linetype = "solid"
  ) +
  geom_col(linewidth = 1.2, alpha = 0.7, fill = "#ff0000") +
  twitter_theme() +
  watermark(df$date, df$cmr) +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))


# By Age Group
data <- read_remote("mortality/Germany/deaths_icd10_year_age.csv")

df <- data %>%
  filter(icd10 %in% c("T881", "U129")) %>%
  # mutate(
  #   age_group = case_when(
  #     age_group %in% c("0", "1-4") ~ "00-04",
  #     age_group %in% c("5-9", "10-14", "15-19") ~ "05-19",
  #     age_group %in% c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59") ~ "20-59",
  #     age_group %in% c("60-64", "65-69", "70-74", "75-80", "80-84", "85-89", "90+") ~ "60+"
  #   )
  # ) %>%
  group_by(year, age_group) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(date = ymd(year, truncated = 2L))

ggplot(df, aes(x = date, y = deaths, groups = age_group, group_name = age_group, color = age_group)) +
  labs(
    title = "Vaccine Deaths by Age Group [Germany]",
    subtitle = "ICD-10: U12.9 & T88.1; Source: German Federal Statistical Office; (c) @USMortality",
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
  filter(icd10 %in% c("T881", "U129")) %>%
  group_by(year, sex) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(date = ymd(year, truncated = 2L))

ggplot(df, aes(x = date, y = deaths, groups = sex, group_name = sex, color = sex)) +
  labs(
    title = "Vaccine Deaths by Gender [Germany]",
    subtitle = "ICD-10: U12.9 & T88.1; Source: German Federal Statistical Office; (c) @USMortality",
    y = "Deaths",
    x = "Year"
  ) +
  geom_line() +
  twitter_theme() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5)) +
  facet_wrap(~sex)
