urls <- list(
  c(
    "https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/blob/master/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv?raw=true",
    "covid19_deu_vaccinations.csv"
  ),
  c(
    "https://data.cdc.gov/api/views/hmz2-vwda/rows.csv?accessType=DOWNLOAD",
    "covid19_usa_live_births.csv"
  ),
  c(
    "https://mortality.org/File/GetDocument/Public/STMF/Outputs/stmf.csv",
    "mortality_org.csv"
  ),
  c(
    "https://github.com/akarlinsky/world_mortality/raw/main/world_mortality.csv",
    "world_mortality.csv"
  ),
  c(
    "https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD&bom=true&format=true",
    "usa_deaths_causes_2020_n.csv"
  ),
  c(
    "https://data.cdc.gov/api/views/y5bj-9g5w/rows.csv?accessType=DOWNLOAD",
    "usa_states_age_weekly.csv"
  ),
  c(
    "https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD",
    "usa_states_age_cause_weekly.csv"
  )
)

dir <- "data/"
for (url in urls) {
  print(paste0(dir, url[2]))
  download.file(url[1], paste0(dir, url[2]))
}
