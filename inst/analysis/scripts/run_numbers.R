
require(data.table)
require(paramix)

.args <- if (interactive()) c(
  file.path("figure", "fig_utilities.rda"),
  file.path("input", "population.rds"),
  file.path("input", "disease_pars.rda"),
  file.path("output", "incidence.rds"),
  file.path("output", "consolidated.rds")
) else commandArgs(trailingOnly = TRUE)

load(.args[1])

inc_dt <- readRDS(.args[4])
model_partition <- c(inc_dt[method == "f_mean", unique(model_partition)], 101)
inc_dt <- inc_dt[method == "f_mean"][,
  .(value = sum(value), capita = sum(unique(capita))),
  by = .(iso3 = place, pathogen, intervention, time)
]
isoset <- inc_dt[, unique(iso3)]

load(.args[3])
pop_dt <- readRDS(.args[2])[iso3 %in% isoset]

bound_pop_dt <- pop_dt[, .(
  from = c(from, max(from) + 1L), weight = c(weight, 0)
), by = iso3]

ifr_dt <- bound_pop_dt[,
  ifr_opts |> lapply(\(fp) parameter_summary(fp, .SD, model_partition, resolution = 102)) |>
    rbindlist(idcol = "pathogen"),
  by = iso3
][model_category == 4][x == max(x)] |>
  dcast(iso3 + pathogen ~ method, value.var = "value")

ifr_dt[, place := fifelse(iso3 == "AFG", "LMIC", "HIC")]

for (path in ifr_dt[, unique(pathogen)]) {
  cat(path, ' 65+ IFR', "\n")
  for (plc in ifr_dt[, unique(place)]) {
    cat(plc, ': [1] MEAN METHOD [2] PARAMIX', "\n")
    cat(paste0(ifr_dt[pathogen==path & place==plc, signif(100*c(f_mean, wm_f), 3)], '%'), "\n")
    cat('% INCREASE WITH PARAMIX', "\n")
    cat(ifr_dt[pathogen == path & place == plc, signif(100*(wm_f - f_mean)/f_mean, 3)], '%', "\n")
  }
  cat('\n')
}

ts_dt <- readRDS(.args[4])

joinvars <- c("method", "place", "pathogen")
base_dt <- ts_dt[
  intervention == "none", .SD, .SDcols = -c("intervention", "capita")
][,.(deaths = sum(deaths)), by = c(joinvars)]
int_dt <- ts_dt[intervention != "none", .(
  deaths = sum(deaths), capita = sum(unique(capita))
), by = c(joinvars, "intervention")][
  base_dt, on = c(joinvars)
]
int_dt[, averted_death := i.deaths - deaths]
int_dt[, place := fifelse(place == "AFG", "LMIC", "HIC")]

melt_int_dt <- int_dt[method %in% c("f_mid", "wm_f")] |>
  dcast(place + pathogen + intervention ~ method, value.var = "averted_death")

cat('% OVERESTIMATE BENEFIT (DEATHS AVERTED) COMPARED TO PARAMIX', "\n")
for (plc in melt_int_dt[, unique(place)]) {
  for (path in melt_int_dt[, unique(pathogen)]) {
    cat(path, ' ', plc, ': [1] VAX OLDER [2] VAX WORKING [3] VAX YOUNG', "\n")
    cat(paste0(melt_int_dt[
      pathogen == path & place == plc,
      setNames(signif(100*(f_mid - wm_f)/wm_f, 3), intervention)
    ], "%"), "\n")
  }
  cat('\n')
}

ylls_dt <- readRDS(.args[5])
base_dt <- ylls_dt[
  intervention == "none", .SD, .SDcols = -c("intervention")
]
int_dt <- ylls_dt[intervention != "none"][
  base_dt, on = setdiff(names(base_dt), "YLL")
]
int_dt[, averted_yll := i.YLL - YLL]
int_dt[, sim_method := factor(sim_method, levels = names(model_assumption_labels), ordered = TRUE)]
int_dt[, method := factor(method, levels = names(model_assumption_labels), ordered = TRUE)]
int_dt <- int_dt[sim_method=='wm_f'][, sim_method:=NULL]
int_dt[, place := fifelse(place == "AFG", "LMIC", "HIC")]

melt_int_dt <- int_dt[method %in% c("mean_f", "wm_f")] |>
  dcast(pathogen + place + intervention ~ method, value.var = "averted_yll")

cat('% OVERESTIMATE YLLS COMPARED TO PARAMIX', "\n")
for (plc in melt_int_dt[, unique(place)]) {
  for (path in melt_int_dt[, unique(pathogen)]) {
    cat(path, ' ', plc, ': [1] VAX OLDER [2] VAX WORKING [3] VAX YOUNG', "\n")
    cat(paste0(melt_int_dt[
      pathogen == path & place == plc,
      setNames(signif(100*(mean_f - wm_f)/wm_f, 3), intervention)
    ], "%"), "\n")
  }
  cat('\n')
}
