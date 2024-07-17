
require(data.table)

.args <- if (interactive()) c(
  file.path("input", "population.rds"),
  file.path("input", "param_GBR_FLU.rda"),
  "GBR",
  file.path("output", "sim_GBR_FLU.rds")
) else commandArgs(trailingOnly = TRUE)

pop_dt <- readRDS(.args[1])
pop_dt <- pop_dt[
  iso3 == pop_dt[, match.arg(.args[3], unique(iso3))]
]

load(.args[2])

############## WARNING: NEEDS TO REPLACED #################

### TODO actually run a simulation

dt <- CJ(
  intervention = c(
    "none", "vax_young", "vax_working", "vax_elderly"
  ),
  age_group = head(model_agelimits, -1)
)
dt[, deaths := rep(ifr_params*1e6, 4)]
dt[intervention == "vax_elderly" & age_group == 65, deaths := deaths*0.3]
dt[intervention == "vax_young" & age_group == 5, deaths := deaths*0.3]
dt[intervention == "vax_working" & age_group == 20, deaths := deaths*0.3]

dt |> saveRDS(tail(.args, 1))
