source("lib/common.r")

my_app <- rtweet_bot(
  api_key = Sys.getenv("TWITTER_API_KEY"),
  api_secret = Sys.getenv("TWITTER_API_SECRET"),
  access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_SECRET")
)
auth_as(my_app)

if (file.exists("out/mortality/world_max_date.csv")) {
  world_max_date_old <- read.csv("out/mortality/world_max_date.csv")
}
data_ytd <- as_tibble(read.csv("./out/mortality/world_ytd.csv"))

df <- data_ytd %>%
  group_by(iso3c, name) %>%
  summarise(max = max(max_date))

save_csv(df, "mortality/world_max_date")

for (n in seq_len(nrow(df))) {
  val <- df[n, ]
  val_old <- world_max_date_old %>% filter(iso3c == val$iso3c)
  if (val$max != val_old$max) {
    print(paste(val$name, "changed"))
    post_tweet(
      paste0(
        "Mortality Data for ", val$name,
        " has been updated. Latest data now available through ", val$max, "."
      ),
      media = paste0("./out/mortality/", val$name, ".png"),
      media_alt_text = paste("Mortality", val$name)
    )
  }
}
