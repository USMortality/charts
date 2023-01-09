source("lib/common.r")

url_base <- "https://s3.mortality.watch/charts/mortality/cmr/"
my_app <- rtweet_bot(
  api_key = Sys.getenv("TWITTER_API_KEY"),
  api_secret = Sys.getenv("TWITTER_API_SECRET"),
  access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_SECRET")
)
auth_as(my_app)

world_max_date_old <- read_remote("mortality/world_max_date.csv")
data_ytd <- read_remote("mortality/world_ytd.csv")

df <- data_ytd %>%
  group_by(iso3c, name) %>%
  summarise(max = max(max_date_cmr))

downloadImage <- function(name, type) {
  url <- paste0(url_base, name, type)
  download.file(URLencode(url), "/tmp/tweet.png", mode = "wb")
}

tweet <- function(name, max) {
  downloadImage(name, "/weekly_52w_sma_line.png")
  post_tweet(
    paste0(
      "Mortality Data for ", name,
      " has been updated. Latest data now available through ", max, "."
    ),
    media = paste0("/tmp/tweet.png"),
    media_alt_text = paste("Weekly Mortality (52W SMA)", name)
  )
  Sys.sleep(10)
  downloadImage(name, "/weekly_line.png")
  post_tweet(
    paste("Weekly Mortality in", name),
    media = paste0("/tmp/tweet.png"),
    media_alt_text = paste("Weekly Mortality", name),
    in_reply_to_status_id = get_my_timeline()$id_str[1]
  )
  Sys.sleep(10)
  downloadImage(name, "/yearly_bar.png")
  post_tweet(
    paste("Yearly Mortality in", name),
    media = paste0("/tmp/tweet.png"),
    media_alt_text = paste("Yearly Mortality", name),
    in_reply_to_status_id = get_my_timeline()$id_str[1]
  )
  Sys.sleep(10)
  downloadImage(name, "/ytd_bar.png")
  post_tweet(
    paste("YTD Mortality in", name, "through", max),
    media = paste0("/tmp/tweet.png"),
    media_alt_text = paste("YTD Mortality", name),
    in_reply_to_status_id = get_my_timeline()$id_str[1]
  )
  Sys.sleep(10)
  downloadImage(name, "/collage.png")
  post_tweet(
    paste0("Find all charts at: https://www.mortality.watch/?country=", name),
    media = paste0("/tmp/tweet.png"),
    media_alt_text = paste("Mortality", name),
    in_reply_to_status_id = get_my_timeline()$id_str[1]
  )
}

tweets <- 0
for (n in seq_len(nrow(df))) {
  val <- df[n, ]
  val_old <- world_max_date_old %>% filter(iso3c == val$iso3c)
  if (length(val_old$iso3c) == 0 || val$max != val_old$max) {
    print(paste(val$name, "changed"))
    tweet(val$name, val$max)
    Sys.sleep(15 * 60)
    tweets <- tweets + 1
  } else {
    print(paste(val$name, "unchanged"))
  }
}

save_csv(df, "mortality/world_max_date")

if (tweets == 0) {
  rnd_index <- round(runif(1) * length(df$name))
  name <- df$name[rnd_index]
  downloadImage(name, "/weekly_52w_sma_line.png")
  post_tweet(
    paste0(
      "No data updates today - but here's the lateset Mortality Data for ",
      name, "."
    ),
    media = paste0("/tmp/tweet.png"),
    media_alt_text = paste("Weekly Mortality (52W SMA)", name)
  )
}
