
require(data.table)

.args <- if (interactive()) c(
  file.path("input", "population.rds"),
  file.path("input", "param_AFG_FLU.rda"),
  file.path("scripts", "odin.R"),
  "AFG",
  file.path("output", "sim_AFG_FLU.rds")
) else commandArgs(trailingOnly = TRUE)

pop_dt <- readRDS(.args[1])
pop_dt <- pop_dt[
  iso3 == pop_dt[, match.arg(.args[4], unique(iso3))]
]

load(.args[2])
source(.args[3])

epidemic_time_series <- function(
  vacc_cov, # 4-vector of proportion of each model age group vaccinated
  demog, # population by age group
  init_infected, # 4-vector of number of infected individuals in each age group at start of epidemic
  length_of_epid, # epidemic length, in days
  disease_pars,
  contact_mat
) with(disease_pars, {

  # print(eigen(disease_pars$infection_delays[2]*
  #               disease_pars$transmissibility*
  #               contact_mat)$values[1])

  return(epidemic_run(
    init_infected,
    transmissibility,
    demography_input = demog,
    contacts = contact_mat,
    init_vaccinated = vacc_cov,
    efficacy = rep(0.5, length(vacc_cov)), infection_delays = infection_delays,
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

seed_infected <- 1000

approx_time <- system.time(sim_dt <- names(vax_cov_opts) |> setNames(nm = _) |> lapply(\(opt) {
  epidemic_time_series(
    vacc_cov = vax_cov_opts[[opt]],
    demog = demog, init_infected = c(0, 0, seed_infected, 0),
    length_of_epid = 20*7, disease_pars = sim_pars,
    contact_mat = cmij
  )
}) |> rbindlist(idcol = "intervention"))

I0 <- numeric(length = pop_dt[, .N])
I0[which(pop_dt$age_group == 3)] <- pop_dt[age_group == 3, seed_infected*weight/sum(weight)]

full_time <- system.time(simfull_dt <- names(vax_cov_opts) |> setNames(nm = _) |> lapply(\(opt) {
  dup_pop_dt <- copy(pop_dt)
  dup_pop_dt[, vax := 0]
  if (sum(vax_cov_opts[[opt]])) {
    targroup <- which(vax_cov_opts[[opt]] != 0)
    dup_pop_dt[
      age_group == targroup,
      vax := vax_num * weight / sum(weight)
    ]
  }

  epidemic_time_series(
    vacc_cov = dup_pop_dt$vax,
    demog = dup_pop_dt$weight * 1e3, init_infected = I0,
    length_of_epid = 20*7, disease_pars = sim_pars,
    contact_mat = cmijfull
  )
}) |> rbindlist(idcol = "intervention"))

cat(
  "approx time",
  approx_time[3],
  "full time",
  full_time[3],
  file = stdout(),
  sep = "\n"
)

sim_dt[, model_partition := model_agelimits[age_group]]
nonfull_params <- ifr_params[method != "f_val", .(model_partition, method, value) ] |> unique()

exp_sim_dt <- nonfull_params[, unique(method)] |> setNames(nm = _) |> lapply(\(m) sim_dt) |> rbindlist(idcol = "method")

exp_sim_dt[nonfull_params, on = .(model_partition, method), deaths := value * i.value]

capita_dt <- data.table(age_group = seq_along(demog), capita = demog)

simfull_dt[, from := age_group - 1L]
simfull_dt[pop_dt, on = .(from), capita := weight * 1e3]
setnames(simfull_dt, "from", "model_partition")
simfull_dt[, method := "full"]
simfull_dt[ifr_params[method == "f_val"], on = .(model_partition = x), deaths := value * i.value]

exp_sim_dt[capita_dt, on = .(age_group), capita := capita]

rbind(exp_sim_dt, simfull_dt) |> saveRDS(tail(.args, 1))
