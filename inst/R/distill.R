
require(data.table)
require(paramix)

.args <- if (interactive()) c(
  file.path("input", "population.rds"),
  file.path("input", "param_GBR_SC2.rda"),
  file.path("output", "sim_GBR_SC2.rds"),
  "GBR",
  file.path("output", "distill_GBR_SC2.rds")
) else commandArgs(trailingOnly = TRUE)


pop_dt <- readRDS(.args[1])
pop_dt <- pop_dt[
  iso3 == pop_dt[, match.arg(.args[4], unique(iso3))]
]

load(.args[2])

sim_dt <- readRDS(.args[3])[, .(sim_method = method, intervention, time, model_from, value = deaths)]

dt <- sim_dt[,{
  distill_summary(.SD, pop_dt, mapping_dt)
}, by = .(sim_method, intervention, time)]

dt |> saveRDS(tail(.args, 1))
