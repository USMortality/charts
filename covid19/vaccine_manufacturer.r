source("lib/common.r")

df_raw <- read_csv("./data_static/covid-vaccine-doses-by-manufacturer.csv")

df <- df_raw |>
  select(2:5) |>
  pivot_longer(
    cols = 3:4,
    names_to = "type",
    values_to = "doses"
  ) |>
  mutate(doses = as.integer(doses))

all <- df |>
  group_by(Code, type) |>
  summarize(doses = max(doses))

sum(all$doses) # 3,063,639,394 --> 0.55%
