
require(jsonlite)

.args <- if (interactive()) c(
  file.path("input", "population.rds"),
  file.path("input", "param_GBR_FLU.rds"),
  "GBR", "FLU",
  file.path("output", "sim_GBR_FLU.rds")
) else commandArgs(trailingOnly = TRUE)


iso <- match.arg(.args[2], c("AFR", "GBR"))

pop_dt <- readRDS(.args[1])[iso3 == iso]
config <- read_json(.args[2])

dt <- data.table()

dt |> saveRDS(tail(.args, 1))
