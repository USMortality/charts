source("lib/common.r")

data_yearly <- read.csv("out/mortality/world_yearly_asmr_bl.csv")

df <- data_yearly %>%
  filter(iso3c == "DEU") %>%
  distinct(date, .keep_all = TRUE) %>%
  as_tsibble(index = date)

autoplot(df, .vars = excess_p)

sum(tail(df, 4)$sign_excess_p, na.rm = TRUE)
