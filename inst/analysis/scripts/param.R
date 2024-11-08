
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

# for checking a high resolution approximation;
# assume contacts are uniformly distributed
# e.g. 0-4 => 0-4 becomes 0 (same from) => 0 ... 4 (to divided by 5),
# 1 (same from) => 0 ... 4 (to divided by 5), etc
# but the divided rates add up effectively means same as 0-4 contact rate

widths <- diff(model_agelimits)
cmijfull <- array(0, dim = c(sum(widths), sum(widths)))
for (i in seq_along(widths)) {
  newrow <- rep(cmij[i,], times = widths)
  for (j in model_agelimits[i]:(model_agelimits[i + 1] - 1)) {
    cmijfull[j,] <- newrow
  }
}

bound_pop_dt <- pop_dt[, .(from = c(from, max(from) + 1L), weight = c(weight, 0))]

mapping_dt <- alembic(
  f_param = f_ifr, f_pop = bound_pop_dt,
  model_partition = model_agelimits,
  output_partition = bound_pop_dt[, seq(min(from), max(from))]
)

# using `parameter_summary` instead of `blend`, because we want to compare to
# the naive alternatives
ifr_params <- parameter_summary(
  f_param = f_ifr, f_pop = bound_pop_dt,
  model_partition = model_agelimits, resolution = 102L
)

ifr_params[, model_partition := model_agelimits[model_category]]

save(
  model_agelimits, ifr_params, mapping_dt, cmij, sim_pars, cmijfull,
  file = tail(.args, 1)
)
