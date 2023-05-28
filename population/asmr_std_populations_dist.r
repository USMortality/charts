source("lib/common.r")
source("lib/asmr.r")

age_groups <- c(
  "0-9",
  "10-19",
  "20-29",
  "30-39",
  "40-49",
  "50-59",
  "60-69",
  "70-79",
  "80-89",
  "90+"
)

df <- get_who2015_bins(age_groups) |> rename(who = weight)
df$esp <- get_esp2013_bins(age_groups)$weight
df$usa <- get_usa2000_bins(age_groups)$weight

data <- df |> pivot_longer(
  cols = 2:4,
  names_to = "std_pop",
  values_to = "weight"
)

ggplot(
  df,
  aes(x = age_group, group = 1)
) +
  labs(
    title = paste0("Standard Populations"),
    subtitle = "Source: seer.cancer.gov",
    y = "Weight (%)",
    x = "Age Group"
  ) +
  geom_line(aes(y = who, colour = "who")) +
  geom_line(aes(y = esp, colour = "esp")) +
  geom_line(aes(y = usa, colour = "usa")) +
  twitter_theme() +
  scale_y_continuous(
    labels = scales::percent_format(scale = 100)
  ) +
  theme(legend.title = element_blank())
