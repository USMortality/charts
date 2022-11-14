libs <- c(
  "jsonlite",
  "dplyr",
  "httr",
  "tsibble",
  "ggplot2",
  "aws.s3",
  "quantmod",
  "scales",
  "tidyverse",
  "readxl",
  "clock",
  "fable",
  "feasts",
  "ggpubr"
)

cran_repos <- c(
  MAIN_CRAN_MIRROR = "https://cran.rstudio.com",
  ALT_CRAN_MIRROR = "http://cran.r-project.org/"
)

installed_packages <- installed.packages()[, "Package"]
for (pkg_name in libs) {
  if (!(pkg_name %in% installed_packages)) {
    message("Installing ", pkg_name)
    install.packages(pkg_name, repos = cran_repos, Ncpus = 16)
    if (!(pkg_name %in% installed.packages()[, "Package"])) {
      stop(
        pkg_name,
        " is a required package and it could not be installed, stopping!"
      )
    }
  }
  message("Loading ", pkg_name)
  library(pkg_name, character.only = TRUE)
}
