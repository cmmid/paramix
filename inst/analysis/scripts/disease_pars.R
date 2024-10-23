
require(data.table)
require(wpp2022)
require(countrycode)

.args <- if (interactive()) c(
  file.path("input", "population.rds"),
  file.path("input", "lex.rds"),
  file.path("input", "disease_pars.rda")
) else commandArgs(trailingOnly = TRUE)

# from Levin et al https://doi.org/10.1007/s10654-020-00698-1
ifr_opts <- list(
  SC2 = \(age_in_years) {
    (10^(-3.27 + 0.0524 * age_in_years))/100
  }
)

pop_dt <- readRDS(.args[1])
lex_dt <- readRDS(.args[2])

# TODO a bit slow + repeated - move into own script?
# for FLU, going to assume looks like all-cause mortality in an HIC setting
ref_demo_dt <- pop_dt[iso3 == "GBR"]
ref_mx_dt <- lex_dt[
  iso3 == "GBR", .(iso3, age, mx)
][ref_demo_dt, on = .(iso3, age = from)]

scaling <- ref_mx_dt[, sum(ifr_opts$SC2(age)*weight)/sum(mx*weight),]
all_cause_mort <- ref_mx_dt[, approxfun(age, scaling*mx, rule = c(1:2))]

ifr_opts$FLU <- all_cause_mort

disease_pars <- list(
  FLU = list(
    infection_delays = c(1, 2), # incubation, typical duration infectious [days]
    transmissibility = 0.15
  ),
  SC2 = list(
    infection_delays = c(3, 5),
    transmissibility = 0.1
  )
)

save(ifr_opts, disease_pars, file = tail(.args, 1))
