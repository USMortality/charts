source("lib/common.r")

data <- read.csv("./data/covid19_deu_vaccinations.csv")

df <- as_tibble(data) %>%
  mutate(Impfdatum = date_parse(Impfdatum, format = "%F")) %>%
  mutate(jahr = lubridate::isoyear(Impfdatum)) %>%
  mutate(woche = lubridate::week(Impfdatum)) %>%
  group_by(jahr, woche, Altersgruppe, Impfschutz) %>%
  summarize(n = sum(Anzahl)) %>%
  ungroup() %>%
  mutate(jahr_woche = make_yearweek(
    year = jahr, week = woche
  )) %>%
  group_by(jahr_woche, Altersgruppe, Impfschutz) %>%
  summarize(n = sum(n)) %>%
  select(jahr_woche, Altersgruppe, Impfschutz, n) %>%
  setNames(c(
    "year_week", "age_group", "dose", "count"
  )) %>%
  pivot_wider(names_from = dose, values_from = count) %>%
  ungroup()

df["all"] <- rowSums(df[, 3:ncol(df)], na.rm = TRUE)
save_csv(df, "covid19/deu/vaccinations")

# Chart
ts <- df %>% pivot_longer(
  cols = 3:ncol(df),
  names_to = "dose",
  values_to = "count"
)

chart <-
  ggplot(ts %>% filter(dose == "all"), aes(x = year_week, y = count)) +
  labs(
    title = "Weekly COVID-19 Vaccinations by Age Group [Germany]",
    subtitle = "Source: rki.de",
    x = "Week of Year",
    y = "COVID-19 Vaccinations"
  ) +
  geom_line(color = "#5383EC", linewidth = 1) +
  twitter_theme() +
  watermark(df$yearmonth, df$value_p) +
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6)) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
  facet_wrap(vars(age_group)) +
  theme(panel.spacing = unit(0.3, "in"))

chart2 <-
  ggplot(
    ts %>% filter(dose != "all"),
    aes(x = year_week, y = count, group = dose, color = dose)
  ) +
  labs(
    title = "Weekly COVID-19 Vaccinations by Age Group/Dose [Germany]",
    subtitle = "Source: rki.de",
    x = "Week of Year",
    y = "COVID-19 Vaccinations"
  ) +
  geom_line(linewidth = 1) +
  twitter_theme() +
  watermark(df$yearmonth, df$value_p) +
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6)) +
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
  facet_wrap(vars(age_group)) +
  theme(panel.spacing = unit(0.3, "in"))

quarterly <- df %>%
  mutate(quarter = yearquarter(year_week)) %>%
  group_by(quarter) %>%
  summarise(all = sum(all))

save_csv(quarterly, "covid19/deu/vaccinations_quarterly")

chart3 <-
  ggplot(quarterly, aes(x = quarter, y = all)) +
  labs(
    title = "Quarterly COVID-19 Vaccinations [Germany]",
    subtitle = "Source: rki.de",
    x = "Quarter of Year",
    y = "COVID-19 Vaccinations"
  ) +
  geom_col(fill = "#5383EC") +
  twitter_theme() +
  watermark(df$yearmonth, df$value_p) +
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6))

save_chart(chart, "covid19/deu/vaccinations_age")
save_chart(chart2, "covid19/deu/vaccinations_age_dose")
save_chart(chart3, "covid19/deu/vaccinations_quarterly")
