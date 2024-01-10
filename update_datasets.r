source("./lib/common.r")
options(timeout = 600)

urls <- list(
  c(
    "https://data.cdc.gov/api/views/hmz2-vwda/rows.csv?accessType=DOWNLOAD",
    "covid19_usa_live_births.csv"
  ),
  c(
    paste0(
      "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/",
      "Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle",
      ".xlsx?__blob=publicationFile"
    ),
    "sonderauswertung-sterbefaelle.xlsx"
  ),
  c(
    "https://covid.ourworldindata.org/data/owid-covid-data.csv",
    "owid.csv"
  ),
  c(
    paste0(
      "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
      "demo_r_mwk_10/?format=TSV&compressed=true&i"
    ),
    "eurostat_weekly.tsv.gz"
  ),
  c(
    paste0(
      "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
      "demo_pjan/?format=TSV&compressed=true&i"
    ),
    "eurostat_population.tsv.gz"
  ),
  c(
    "https://mortality.org/File/GetDocument/Public/STMF/Outputs/stmf.csv",
    "mortality_org.csv"
  ),
  c(
    paste0(
      "https://github.com/akarlinsky/world_mortality/raw/main/",
      "world_mortality.csv"
    ),
    "world_mortality.csv"
  )
)

dir <- "data/"
for (url in urls) {
  print(paste0(dir, url[2]))
  retry_download(url[1], paste0(dir, url[2]))
}

# source("./update_datasets.r")
