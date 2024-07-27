
require(data.table)
require(paramix)
require(socialmixr)

.args <- if (interactive()) c(
  file.path("input", "population.rds"),
  file.path("input", "disease_pars.rda"),
  "GBR",
  "SC2",
  file.path("input", "param_GBR_FLU.rda")
) else commandArgs(trailingOnly = TRUE)

pop_dt <- readRDS(.args[1])
load(.args[2]) # ifr_opts

# now downselect to focal parameters
pop_dt <- pop_dt[
  iso3 == pop_dt[, match.arg(.args[3], unique(iso3))]
]
f_ifr <- ifr_opts[[match.arg(.args[4], names(ifr_opts))]]
sim_pars <- disease_pars[[match.arg(.args[4], names(ifr_opts))]]

model_agelimits <- c(0, 5, 20, 65, 101)

# assume all age-contact patterns based off POLYMOD results
cmij <- contact_matrix(
  polymod, survey.pop = pop_dt[, .(lower.age.limit = from, population = weight*1e3) ],
  age.limits = model_agelimits, missing.participant.age = "remove",
  missing.contact.age = "remove"
)$matrix

mapping_dt <- alembic(
  f_param = f_ifr, densities = pop_dt,
  model_partition = model_agelimits,
  new_partition = pop_dt[, seq(min(from), max(from) + 1L)]
)

# using `parameter_summary` instead of `blend`, because we want to compare to
# the naive alternatives
ifr_params <- parameter_summary(
  f_param = f_ifr, densities = pop_dt,
  model_partition = model_agelimits
)

ifr_params[, model_from := model_agelimits[model_category]]

save(
  model_agelimits, ifr_params, mapping_dt, cmij, sim_pars,
  file = tail(.args, 1)
)
