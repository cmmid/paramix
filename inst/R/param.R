
require(data.table)
require(jsonlite)
require(paramix)

.args <- if (interactive()) c(
  file.path("input", "population.rds"),
  "GBR", "FLU",
  file.path("input", "param_GBR_FLU.json")
) else commandArgs(trailingOnly = TRUE)

iso <- match.arg(.args[2], c("AFR", "GBR"))
ifr <- match.arg(.args[3], c("FLU", "SC2"))

pop_dt <- readRDS(.args[1])[iso3 == iso]

f_ifr <- switch (ifr,
  FLU = \(age) {},
  SC2 = \(age) {}
)

obj <- list()

obj |> write_json(tail(.args, 1), pretty = TRUE)
