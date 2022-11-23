source("lib/common.r")

data <- read.csv("./data/covid19_deu_vaccinations.csv")

df <- as_tibble(data) %>%
  mutate(Impfdatum = date_parse(Impfdatum, format = "%F")) %>%
  mutate(jahr = format(Impfdatum, "%Y")) %>%
  mutate(woche = format(Impfdatum, "%V")) %>%
  mutate(altersgruppe = Altersgruppe) %>%
  group_by(jahr, woche, altersgruppe, Impfschutz) %>%
  summarize(n = sum(Anzahl)) %>%
  mutate(jahr_woche = make_yearweek(
    year = as.numeric(jahr), week = as.numeric(woche)
  ), .before = jahr) %>%
  group_by(jahr_woche, altersgruppe, Impfschutz) %>%
  summarize(n = sum(n)) %>%
  setNames(c(
    "year_week", "age_group", "dose", "count"
  )) %>%
  pivot_wider(names_from = dose, values_from = count) %>%
  mutate(all = sum(`1`, `2`, `3`, `4`, na.rm = TRUE)) %>%
  ungroup()

save_csv(df, "covid19/deu/vaccinations")

# Chart
ts <- df %>% pivot_longer(cols = 3:7, names_to = "dose", values_to = "count")

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
  facet_wrap(vars(age_group))

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
  facet_wrap(vars(age_group))

save_chart(chart, "covid19/deu/vaccinations_age")
save_chart(chart2, "covid19/deu/vaccinations_age_dose")
