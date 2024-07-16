
require(jsonlite)

.args <- if (interactive()) c(
  file.path("input", "population.rds"),
  file.path("input", "param_GBR_FLU.rds"),
  file.path("output", "sim_GBR_FLU.rds"),
  "GBR",
  file.path("output", "distill_GBR_FLU.rds")
) else commandArgs(trailingOnly = TRUE)


iso <- match.arg(.args[4], c("AFR", "GBR"))

pop_dt <- readRDS(.args[1])[iso3 == iso]
config <- read_json(.args[2])
sim_dt <- readRDS(.args[3])

dt <- data.table()

dt |> saveRDS(tail(.args, 1))
