options(progressr.enable = TRUE)

# Multithreading
n_cores <- detectCores()
if (n_cores > 2) {
    plan(multisession, workers = max(1, n_cores - 1))
} else {
    plan(sequential)
}
