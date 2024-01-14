# Get ICU data.
df <- read_csv(paste0(
  "https://raw.githubusercontent.com/robert-koch-institut/",
  "Intensivkapazitaeten_und_COVID-19-Intensivbettenbelegung_in_Deutschland/",
  "main/Intensivregister_Bundeslaender_Kapazitaeten.csv"
))

# Filter for Saxony/Sachsen
sn <- df |> filter(bundesland_name == "Sachsen")

# Erstaufnahmen/NewlyAdmitted are missing for the first season

# 2021/2022
ts21_22 <- sn |>
  filter(datum >= as.Date("2021-10-01"), datum < as.Date("2022-06-01"))
covid_daily <- sum(ts21_22$faelle_covid_aktuell)
covid_new <- sum(ts21_22$faelle_covid_erstaufnahmen)
days_icu <- covid_daily / covid_new
print(paste0(
  "Season 2021/2022: COVID-19 - New: ", covid_new,
  "; Total Daily: ", covid_daily,
  "; AVG Days in ICU: ", round(days_icu, 2)
))

# 2020/2021
ts20_21 <- sn |>
  filter(datum >= as.Date("2020-10-01"), datum < as.Date("2021-06-01"))

covid_daily <- sum(ts20_21$faelle_covid_aktuell)
covid_new <- covid_daily / 14.01
print(paste0(
  "Season 2020/2021: COVID-19 - New: ", round(covid_new),
  "(ESTIMATED); Total Daily: ", covid_daily,
  "; AVG Days in ICU: ", round(days_icu, 2)
))
