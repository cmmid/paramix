
require(data.table)
require(paramix)
require(socialmixr)

.args <- if (interactive()) c(
  file.path("input", "population.rds"),
  file.path("input", "disease_pars.rda"),
  "AFG",
  "FLU",
  file.path("input", "param_AFG_FLU.rda")
) else commandArgs(trailingOnly = TRUE)

pop_dt <- readRDS(.args[1])
load(.args[2]) # ifr_opts

# now downselect to focal parameters
pop_dt <- pop_dt[
  iso3 == pop_dt[, match.arg(.args[3], unique(iso3))]
][, .(iso3, from, weight)]
f_ifr <- ifr_opts[[match.arg(.args[4], names(ifr_opts))]]
sim_pars <- disease_pars[[match.arg(.args[4], names(ifr_opts))]]

model_agelimits <- c(0, 5, 20, 65, 101)

# assume all age-contact patterns based off POLYMOD results
cmij <- contact_matrix(
  polymod, survey.pop = pop_dt[, .(lower.age.limit = from, population = weight*1e3) ],
  age.limits = model_agelimits, missing.participant.age = "remove",
  missing.contact.age = "remove", symmetric = TRUE
)$matrix

cmijfull <- contact_matrix(
  polymod, survey.pop = pop_dt[, .(lower.age.limit = from, population = weight*1e3) ],
  age.limits = pop_dt[, from], missing.participant.age = "remove",
  missing.contact.age = "remove", symmetric = TRUE
)$matrix

mapping_dt <- alembic(
  f_param = f_ifr, f_dense = pop_dt,
  model_partition = model_agelimits,
  output_partition = pop_dt[, seq(min(from), max(from) + 1L)]
)

# using `parameter_summary` instead of `blend`, because we want to compare to
# the naive alternatives
ifr_params <- parameter_summary(
  f_param = f_ifr, f_dense = pop_dt,
  model_partition = model_agelimits
)

ifr_params[, model_from := model_agelimits[model_category]]

save(
  model_agelimits, ifr_params, mapping_dt, cmij, sim_pars, cmijfull,
  file = tail(.args, 1)
)
