
require(data.table)
require(paramix)

.args <- if (interactive()) c(
  file.path("input", "population.rds"),
  "GBR", "SC2",
  file.path("input", "param_GBR_FLU.rda")
) else commandArgs(trailingOnly = TRUE)

iso <- match.arg(.args[2], c("AFR", "GBR"))
ifr <- match.arg(.args[3], c("FLU", "SC2"))

pop_dt <- readRDS(.args[1])[iso3 == iso]

f_ifr <- switch (ifr,
  FLU = \(age_in_years) {},
  SC2 = \(age_in_years) {
    scaled <- exp(-7.56 + 0.121 * age_in_years)
    scaled / (100 + scaled)
  }
)

model_agelimits <- c(0, 5, 20, 65, 101)

ifr_params <- blend(
  f_param = f_ifr,
  densities = pop_dt,
  model_partition = model_agelimits
)

mapping_dt <- alembic(
  f_param = f_ifr, densities = pop_dt,
  model_partition = model_agelimits,
  new_partition = pop_dt[, seq(min(from), max(from) + 1L)]
)

save(
  model_agelimits, ifr_params, mapping_dt,
  file = tail(.args, 1)
)
