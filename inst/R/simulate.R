
require(data.table)

.args <- if (interactive()) c(
  file.path("input", "population.rds"),
  file.path("input", "odin.rda"),
  file.path("input", "param_GBR_FLU.rda"),
  "GBR",
  file.path("output", "sim_GBR_FLU.rds")
) else commandArgs(trailingOnly = TRUE)

pop_dt <- readRDS(.args[1])
pop_dt <- pop_dt[
  iso3 == pop_dt[, match.arg(.args[4], unique(iso3))]
]

load(.args[2])
load(.args[3])

epidemic_time_series <- function(
  vacc_cov, # 4-vector of proportion of each model age group vaccinated
  demog, # population by age group
  init_infected, # 4-vector of number of infected individuals in each age group at start of epidemic
  length_of_epid, # epidemic length, in days
  disease_pars,
  contact_mat
) with(disease_pars, {

  susceptibility <-  c((0.2*1 + 0.8*susc_par), rep(susc_par, 3))

  return(epidemic_run(
    init_infected,
    susceptibility,
    transmissibility,
    demography_input = demog,
    contacts = contact_mat,
    init_vaccinated = vacc_cov,
    efficacy = rep(0.5, 4), infection_delays = infection_delays,
    duration = length_of_epid
  ))

})

pop_dt[, age_group := findInterval(from, model_agelimits)]
demog <- pop_dt[, .(pop = sum(weight * 1000)), keyby = age_group]$pop

# for scenarios, assume we can get 75% of oldest pop vaccinated =>
# use this number of doses for all interventions considered
vax_num <- round(demog[4]*0.75)

vax_cov_opts <- list(
  none = rep(0, 4),
  vax_young = c(0, vax_num, 0, 0),
  vax_working = c(0, 0, vax_num, 0),
  vax_older = c(0, 0, 0, vax_num)
)

sim_dt <- names(vax_cov_opts) |> setNames(nm = _) |> lapply(\(opt) {
  epidemic_time_series(
    vacc_cov = vax_cov_opts[[opt]],
    demog = demog, init_infected = c(0, 0, 1000, 0),
    length_of_epid = 280, disease_pars = sim_pars,
    contact_mat = cmij
  )
}) |> rbindlist(idcol = "intervention")

sim_dt[, model_from := model_agelimits[age_group]]
sim_dt[ifr_params, on = .(model_from), deaths := value*i.value]

sim_dt |> saveRDS(tail(.args, 1))
