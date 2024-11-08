
require(data.table)
require(paramix)

.args <- if (interactive()) c(
  file.path("input", "population.rds"),
  file.path("input", "param_AFG_SC2.rda"),
  file.path("output", "sim_AFG_SC2.rds"),
  "GBR",
  file.path("output", "distill_AFG_SC2.rds")
) else commandArgs(trailingOnly = TRUE)


pop_dt <- readRDS(.args[1])
pop_dt <- pop_dt[
  iso3 == pop_dt[, match.arg(.args[4], unique(iso3))]
]

load(.args[2])

sim_dt <- readRDS(.args[3])[, .(
  sim_method = method, intervention, time, model_partition, value = deaths
)][, .(value = sum(value)), by = .(sim_method, intervention, model_partition)]

partial_dt <- sim_dt[sim_method != "full"]
full_dt <- setnames(
  sim_dt[sim_method == "full"], "model_partition", "partition"
)[, method := "hrm"]

dt <- rbind(
  partial_dt[,{
    distill_summary(mapping_dt, .SD)
  }, by = .(sim_method, intervention)],
  full_dt
)

dt[, capita := pop_dt[, sum(weight*1000)] ]

dt |> saveRDS(tail(.args, 1))
