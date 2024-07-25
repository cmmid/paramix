
require(data.table)
require(paramix)
require(wpp2022)
require(countrycode)
require(socialmixr)

.args <- if (interactive()) c(
  file.path("input", "population.rds"),
  "GBR", "SC2",
  file.path("input", "param_GBR_FLU.rda")
) else commandArgs(trailingOnly = TRUE)

ifr_opts <- list(
  SC2 = \(age_in_years) {
    scaled <- exp(-7.56 + 0.121 * age_in_years)
    scaled / (100 + scaled)
  }
)

pop_dt <- readRDS(.args[1])

# TODO a bit slow + repeated - move into own script?
# for FLU, going to assume looks like all-cause mortality in an HIC setting
ref_demo_dt <- pop_dt[iso3 == "GBR"]
data("mx1dt")
ref_mx_dt <- mx1dt[
  year == 2021
][,
  iso3 := countrycode(country_code, "iso3n", "iso3c")
][!is.na(iso3), .(iso3, age, mx = mxB)][
  ref_demo_dt, on = .(iso3, age = from)
]

scaling <- ref_mx_dt[, sum(ifr_opts$SC2(age)*weight)/sum(mx*weight),]
all_cause_mort <- ref_mx_dt[, approxfun(age, scaling*mx, rule = c(1:2))]

ifr_opts$FLU <- all_cause_mort

disease_pars <- list(
  FLU = list(
    infection_delays = c(0.8, 1.8), # incubation, typical duration infectious [days]
    susc_par = 0.55,
    transmissibility = 0.1
  ),
  SC2 = list(
    infection_delays = c(3, 5),
    susc_par = 0.3,
    transmissibility = 0.1
  )
)

# now downselect to focal parameters
pop_dt <- pop_dt[
  iso3 == pop_dt[, match.arg(.args[2], unique(iso3))]
]
f_ifr <- ifr_opts[[match.arg(.args[3], names(ifr_opts))]]
sim_pars <- disease_pars[[match.arg(.args[3], names(ifr_opts))]]

model_agelimits <- c(0, 5, 20, 65, 101)

cmij <- contact_matrix(
  polymod, survey.pop = pop_dt[, .(lower.age.limit = from, population = weight*1e3) ],
  age.limits = model_agelimits
)$matrix

mapping_dt <- alembic(
  f_param = f_ifr, densities = pop_dt,
  model_partition = model_agelimits,
  new_partition = pop_dt[, seq(min(from), max(from) + 1L)]
)

ifr_params <- blend(mapping_dt)

save(
  model_agelimits, ifr_params, mapping_dt, cmij, sim_pars,
  file = tail(.args, 1)
)
