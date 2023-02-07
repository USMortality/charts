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
  group_by(iso3c, jurisdiction) %>%
  summarise(max = max(max_date_cmr))

downloadImage <- function(name, type) {
  url <- paste0(url_base, name, type)
  download.file(URLencode(url), "/tmp/tweet.png", mode = "wb")
}

tweet <- function(name, max) {
  downloadImage(name, "/weekly_52w_sma_line.png")
  url <- paste0(
    "https://www.mortality.watch/?q=%7B%22c%22%3A%5B%22",
    URLencode(name),
    "%22%5D%2C%22cs%22%3A0%2C%22ct%22%3A0%2C%22t%22%3A2%2C%22df%22%3A%222009+W47%22%2C%22dt%22%3A%222022+W46%22%2C%22m%22%3A0%7D"
  )
  post_tweet(
    paste0(
      "Mortality Data for ", name,
      " has been updated. Latest data now available through ",
      max, ". ", url
    ),
    media = paste0("/tmp/tweet.png"),
    media_alt_text = paste("Weekly Mortality (52W SMA)", name)
  )
}

tweets <- 0
for (n in seq_len(nrow(df))) {
  val <- df[n, ]
  if (left(val$jurisdiction, 6) != "USA - " &&
    left(val$jurisdiction, 6) != "DEU - ") {
    val_old <- world_max_date_old %>% filter(iso3c == val$iso3c)
    if (length(val_old$iso3c) == 0 || val$max != val_old$max) {
      print(paste(val$jurisdiction, "changed"))
      tweet(val$jurisdiction, val$max)
      Sys.sleep(5 * 60)
      tweets <- tweets + 1
    } else {
      print(paste(val$jurisdiction, "unchanged"))
    }
  }
}

save_csv(df, "mortality/world_max_date")
