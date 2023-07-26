urls <- list(
  c(
    "https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/state/st-est00int-agesex.csv",
    "population_usa_2000-2010.csv"
  ),
  c(
    "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/state/asrh/SC-EST2020-AGESEX-CIV.csv",
    "population_usa_2010-2020.csv"
  ),
  c(
    "https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/state/asrh/sc-est2022-agesex-civ.csv",
    "population_usa_2020-2022.csv"
  ),
  c(
    "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/co-est2020.csv",
    "population_usa_county_2010-2020.csv"
  ),
  c(
    "https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/counties/totals/co-est2021-alldata.csv",
    "population_usa_county_2020-2021.csv"
  ),
  c(
    "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_General/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",
    "WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx"
  ),
  c(
    "https://data.cdc.gov/api/views/3yf8-kanr/rows.csv?accessType=DOWNLOAD&bom=true&format=true",
    "usa_deaths_causes_2014_2019.csv"
  ),
  c(
    "https://raw.githubusercontent.com/dr5hn/countries-states-cities-database/master/csv/countries.csv",
    "countries.csv"
  ),
  c(
    "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle-endgueltige-daten.xlsx?__blob=publicationFile",
    "sonderauswertung-sterbefaelle-endgueltige-daten.xlsx"
  )
)

dir <- "data_static/"
for (url in urls) {
  print(paste0(dir, url[2]))
  download.file(url[1], paste0(dir, url[2]))
}
