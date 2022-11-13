source("_deps.r")

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "minio",
  "AWS_SECRET_ACCESS_KEY" = "m!FCcXn_uuuMgkUxMDc7",
  "AWS_S3_ENDPOINT" = "xls2csv.com",
  "AWS_DEFAULT_REGION" = "storage",
  "AWS_SESSION_TOKEN" = ""
)

sf <- 2
options(vsc.dev.args = list(width = 600 * sf, height = 335 * sf, res = 72 * sf))
charts_bucket <- "charts"
data_bucket <- "data"

watermark <- function(x, y) {
  annotate("text",
    y = Inf,
    x = structure(Inf, class = "Date"),
    label = "@USMortality",
    vjust = 1,
    hjust = 1,
    col = "#000000",
    cex = 6,
    fontface = "bold",
    alpha = 0.1
  )
}

twitter_theme <- function() {
  theme_gray() +
    theme(
      plot.title = element_text(size = 18, face = "bold", color = "#333333"),
      plot.subtitle = element_text(size = 14, color = "#999999"),
      axis.title = element_text(size = 13, color = "#999999"),
      axis.text = element_text(size = 12, color = "#000000"),
    )
}

save_csv <- function(df, name) {
  file_name <- paste0(name, ".csv")
  write.csv(df, file_name, row.names = FALSE)

  put_object(
    file = file_name,
    object = file_name,
    bucket = data_bucket
  )
  system(paste0("rm ", file_name))
}

save_chart <- function(chart, name) {
  file_name <- paste0(name, ".png")
  ggsave(
    filename = file_name,
    plot = chart,
    width = 600 * sf,
    height = 335 * sf,
    units = "px",
    dpi = 72 * sf,
    device = grDevices::png,
    type = c("cairo")
  )

  put_object(
    file = file_name,
    object = file_name,
    bucket = charts_bucket
  )
  system(paste0("rm ", file_name))
}

right <- function(string, length) {
  substring(string, nchar(string) - length + 1, nchar(string))
}
