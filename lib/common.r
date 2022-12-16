libs <- read.table("dependencies.txt")
for (lib in libs$V1) {
  library(lib, character.only = TRUE, quietly = TRUE)
}

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
  local_file_name <- paste0("out/", file_name)
  dir.create(dirname(local_file_name), recursive = TRUE)
  write.csv(df, local_file_name, row.names = FALSE)

  put_object(
    file = local_file_name,
    object = file_name,
    bucket = data_bucket,
    multipart = TRUE
  )
}

save_chart <- function(chart, name, scale) {
  if (missing(scale)) scale <- sf
  file_name <- paste0(name, ".png")
  local_file_name <- paste0("out/", file_name)
  dir.create(dirname(local_file_name), recursive = TRUE)

  print(paste0("Saving ", local_file_name))

  ggsave(
    filename = local_file_name,
    plot = chart,
    width = 600,
    height = 335,
    units = "px",
    scale = scale,
    dpi = 144,
    device = grDevices::png,
    type = c("cairo")
  )

  put_object(
    file = local_file_name,
    object = file_name,
    bucket = charts_bucket
  )
}

left <- function(string, length) {
  substr(string, 1, length)
}

right <- function(string, length) {
  substr(string, nchar(string) - length + 1, nchar(string))
}

mid <- function(string, start, length) {
  substr(string, start, start + length - 1)
}

save_collage <- function(..., path = NULL, ncol = 2, nrow = 2, scale = 4) {
  figure <- ggarrange(..., ncol = ncol, nrow = nrow)
  save_chart(figure, path, scale = scale)
}

lm_right <- function(formula, data, ...) {
  mod <- lm(formula, data)
  class(mod) <- c("lm_right", class(mod))
  mod
}

predictdf.lm_right <- function(model, xseq, se, level) {
  ## here the main code: truncate to x values at the right
  init_range <- range(model$model$x)
  xseq <- xseq[xseq >= init_range[1]]
  ggplot2:::predictdf.default(model, xseq, se, level)
}

fluseason_ <- function(date) {
  x <- date
  month(x) <- 10
  day(x) <- 1
  if (date >= x) {
    paste0(year(date), "-", year(date) + 1)
  } else {
    paste0(year(date) - 1, "-", year(date))
  }
}

fluseason2_ <- function(date) {
  y <- year(date)
  if (month(date) <= 9) {
    paste0(y - 1, "-", y)
  } else {
    paste0(y, "-", y + 1)
  }
}

fluseason <- function(data) {
  if (length(data) > 1) {
    sapply(data, fluseason2_)
  } else {
    fluseason2_(data)
  }
}

read_remote <- function(path) {
  as_tibble(read.csv(paste0("https://s3.mortality.watch/data/", path)))
}
