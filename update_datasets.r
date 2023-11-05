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
      "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/",
      "BulkDownloadListing?file=data/demo_r_mwk_10.tsv.gz"
    ),
    "eurostat_weekly.tsv.gz"
  ),
  c(
    paste0(
      "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/",
      "BulkDownloadListing?file=data/demo_pjan.tsv.gz"
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
  download.file(url[1], paste0(dir, url[2]))
}
