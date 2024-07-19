
require(data.table)
require(paramix)

.args <- if (interactive()) c(
  file.path("input", "population.rds"),
  "GBR", "SC2",
  file.path("input", "param_GBR_FLU.rda")
) else commandArgs(trailingOnly = TRUE)

# TODO update FLU IFR calculation; using SC2 as placeholder
ifr_opts <- list(
  FLU = \(age_in_years) {
    scaled <- exp(-7.56 + 0.121 * age_in_years)
    scaled / (100 + scaled)
  },
  SC2 = \(age_in_years) {
    scaled <- exp(-7.56 + 0.121 * age_in_years)
    scaled / (100 + scaled)
  }
)

pop_dt <- readRDS(.args[1])
pop_dt <- pop_dt[
  iso3 == pop_dt[, match.arg(.args[2], unique(iso3))]
]
f_ifr <- ifr_opts[[match.arg(.args[3], names(ifr_opts))]]

model_agelimits <- c(0, 5, 20, 65, 101)

mapping_dt <- alembic(
  f_param = f_ifr, densities = pop_dt,
  model_partition = model_agelimits,
  new_partition = pop_dt[, seq(min(from), max(from) + 1L)]
)

ifr_params <- blend(mapping_dt)

save(
  model_agelimits, ifr_params, mapping_dt,
  file = tail(.args, 1)
)
