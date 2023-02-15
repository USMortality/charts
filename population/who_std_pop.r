source("lib/common.r")

file <- read_html("https://seer.cancer.gov/stdpopulations/world.who.html")
tables <- html_nodes(file, "table")
pop <- html_table(tables[1], fill = TRUE)

pop_5 <- pop[[1]][, 1:2] %>%
  setNames(c("age_group", "percentage")) %>%
  mutate(
    # Translate years
    age_group = case_when(
      age_group %in% c("0-4", "5-9", "10-14") ~ "0-14",
      age_group %in% c(
        "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
        "55-59", "60-64"
      ) ~ "15-64",
      age_group %in% c("65-69", "70-74") ~ "65-74",
      age_group %in% c("75-79", "80-84") ~ "75-84",
      age_group %in% c("85-89", "90-94", "95-99", "100+") ~ "85+"
    )
  ) %>%
  group_by(age_group) %>%
  summarise(percentage = sum(percentage)) %>%
  ungroup() %>%
  mutate(percentage = percentage / 100) %>%
  filter(!is.na(age_group))

save_csv(pop_5, "population/who_std_pop")


pop_6 <- pop[[1]][, 1:2] %>%
  setNames(c("age_group", "percentage")) %>%
  mutate(
    # Translate years
    age_group = case_when(
      age_group %in% c("0-4", "5-9", "10-14", "15-19", "20-24") ~ "0-24",
      age_group %in% c("25-29", "30-34", "35-39", "40-44") ~ "25-44",
      age_group %in% c("45-49", "50-54", "55-59", "60-64") ~ "45-64",
      age_group %in% c("65-69", "70-74") ~ "65-74",
      age_group %in% c("75-79", "80-84") ~ "75-84",
      age_group %in% c("85-89", "90-94", "95-99", "100+") ~ "85+"
    )
  ) %>%
  group_by(age_group) %>%
  summarise(percentage = sum(percentage)) %>%
  ungroup() %>%
  mutate(percentage = percentage / 100) %>%
  filter(!is.na(age_group))

save_csv(pop_6, "population/who_std_pop_2")


pop_4 <- pop[[1]][, 1:2] %>%
  setNames(c("age_group", "percentage")) %>%
  mutate(
    # Translate years
    age_group = case_when(
      age_group %in% c(
        "0-4", "5-9", "10-14", "15-19", "20-24",
        "25-29", "30-34", "35-39", "40-44",
        "45-49", "50-54", "55-59", "60-64"
      ) ~ "0-64",
      age_group %in% c("65-69", "70-74") ~ "65-74",
      age_group %in% c("75-79", "80-84") ~ "75-84",
      age_group %in% c("85-89", "90-94", "95-99", "100+") ~ "85+"
    )
  ) %>%
  group_by(age_group) %>%
  summarise(percentage = sum(percentage)) %>%
  ungroup() %>%
  mutate(percentage = percentage / 100) %>%
  filter(!is.na(age_group))

save_csv(pop_4, "population/who_std_pop_3")


pop_9 <- pop[[1]][, 1:2] %>%
  setNames(c("age_group", "percentage")) %>%
  mutate(
    # Translate years
    age_group = case_when(
      age_group %in% c("0-4", "5-9") ~ "0-9",
      age_group %in% c("10-19", "15-19") ~ "10-19",
      age_group %in% c("20-24", "25-29") ~ "20-29",
      age_group %in% c("30-34", "35-39") ~ "30-39",
      age_group %in% c("40-44", "45-49") ~ "40-49",
      age_group %in% c("50-54", "55-59") ~ "50-59",
      age_group %in% c("60-64", "65-69") ~ "60-69",
      age_group %in% c("70-74", "75-79") ~ "70-79",
      age_group %in% c("80-84", "85-89", "90-94", "95-99") ~ "80+"
    )
  ) %>%
  group_by(age_group) %>%
  summarise(percentage = sum(percentage)) %>%
  ungroup() %>%
  mutate(percentage = percentage / 100) %>%
  filter(!is.na(age_group))

save_csv(pop_9, "population/who_std_pop_9")
