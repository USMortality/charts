source("./lib/common.r")

meta <- read_csv("./out/mortality/world_meta.csv")

codes <- unique(meta$iso3c)
types <- c(
    "weekly",
    "monthly",
    "quarterly",
    "yearly",
    "midyear",
    "fluseason",
    "weekly_104w_sma",
    "weekly_52w_sma",
    "weekly_26w_sma",
    "weekly_13w_sma"
)

for (code in codes) {
    for (type in types) {
        file <- paste0(
            code,
            "/",
            type,
            ".csv"
        )
        print(file)
        file <- "alb/weekly.csv"
        df <- read_csv(
            paste0("./out/mortality/", file),
            col_types = "cciiiiiiiiddddddddddddddddddddddddddddddddddd"
        )
        # problems(df)
    }
}
