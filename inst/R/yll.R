
require(data.table)

.args <- if (interactive()) c(
  file.path("input", "lex.rds"),
  file.path("output", "distill_GBR_SC2.rds"),
  "GBR",
  file.path("output", "yll_GBR_SC2.rds")
) else commandArgs(trailingOnly = TRUE)

lex_dt <- readRDS(.args[1])
lex_dt <- lex_dt[
  iso3 == lex_dt[, match.arg(.args[3], unique(iso3))]
]

distill_dt <- readRDS(.args[2])

yll_dt <- distill_dt[
  lex_dt, on = .(partition = age)
][, .(YLL = sum(value*ex)), by = .(method)]

yll_dt |> saveRDS(tail(.args, 1))
