path <- strsplit(commandArgs(trailingOnly = FALSE)[4], "--file=")[[1]][2]
path <- ifelse(is.na(path), ".", dirname(path))
source(paste(path, "_deps.r", sep = "/"))
source(paste(path, "lib/common.r", sep = "/"))

mtcars <- mtcars
p <- ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(size = 3)
save_chart(p, "cars")
