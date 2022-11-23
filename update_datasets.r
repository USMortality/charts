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
    "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/state/asrh/SC-EST2020-AGESEX-CIV.csv",
    "population_usa_2010-2020.csv"
  ),
  c(
    "https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/state/asrh/sc-est2021-agesex-civ.csv",
    "population_usa_2020-2021.csv"
  ),
  c(
    "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/asrh/CC-EST2020-ALLDATA-36.csv",
    "population_usa_county_2010-2020.csv"
  ),
  c(
    "https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/asrh/cc-est2021-alldata-36.csv",
    "population_usa_county_2020-2021.csv"
  ),
  c(
    "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_General/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",
    "WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx"
  ),
  c(
    "https://mortality.org/File/GetDocument/Public/STMF/Outputs/stmf.csv",
    "mortality_org.csv"
  ),
  c(
    "https://github.com/akarlinsky/world_mortality/raw/main/world_mortality.csv",
    "world_mortality.csv"
  )
)

dir <- "data/"
for (url in urls) {
  download.file(url[1], paste0(dir, url[2]))
}
