
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
model_partition <- c(inc_dt[, unique(model_from)], 101)
inc_dt <- inc_dt[method == "f_mean"][,
                                     .(value = sum(value), capita = sum(unique(capita))),
                                     by = .(iso3 = place, pathogen, intervention, time)
]
isoset <- inc_dt[, unique(iso3)]

load(.args[3])
pop_dt <- readRDS(.args[2])[iso3 %in% isoset]

ifr_dt <- pop_dt[,
                 ifr_opts |> lapply(\(fp) parameter_summary(fp, .SD, model_partition)) |>
                   rbindlist(idcol = "pathogen"),
                 by = iso3
]

cat('SC2 65+ IFR', "\n")
cat('LMIC: [1] MEAN METHOD [2] PARAMIX', "\n")
cat(paste0(round(100*ifr_dt[x==100 & method %in% c('f_mean','wm_f') & pathogen=='SC2' & iso3=='AFG']$value, 1), '%'), "\n")
cat('HIC: [3] MEAN METHOD [4] PARAMIX', "\n")
cat(paste0(round(100*ifr_dt[x==100 & method %in% c('f_mean','wm_f') & pathogen=='SC2' & iso3=='GBR']$value, 1), '%'), "\n")
cat('\n')

cat('SC2 65+ IFR, % INCREASE WITH PARAMIX', "\n")
cat('LMIC:', "\n")
cat(paste0(100*(round(ifr_dt[x==100 & method %in% c('wm_f') & pathogen=='SC2' & iso3=='AFG']$value, 1) -
              round(ifr_dt[x==100 & method %in% c('f_mean') & pathogen=='SC2' & iso3=='AFG']$value, 1))/
             round(ifr_dt[x==100 & method %in% c('f_mean') & pathogen=='SC2' & iso3=='AFG']$value, 1), '%'), "\n")
cat('HIC:', "\n")
cat(paste0(100*(round(ifr_dt[x==100 & method %in% c('wm_f') & pathogen=='SC2' & iso3=='GBR']$value, 1) -
                  round(ifr_dt[x==100 & method %in% c('f_mean') & pathogen=='SC2' & iso3=='GBR']$value, 1))/
             round(ifr_dt[x==100 & method %in% c('f_mean') & pathogen=='SC2' & iso3=='GBR']$value, 1), '%'), "\n")
cat('\n')
cat('FLU 65+ IFR, % INCREASE WITH PARAMIX', "\n")
cat('LMIC:', "\n")
cat(paste0(100*(round(ifr_dt[x==100 & method %in% c('wm_f') & pathogen=='FLU' & iso3=='AFG']$value, 1) -
                  round(ifr_dt[x==100 & method %in% c('f_mean') & pathogen=='FLU' & iso3=='AFG']$value, 1))/
             round(ifr_dt[x==100 & method %in% c('f_mean') & pathogen=='FLU' & iso3=='AFG']$value, 1), '%'), "\n")
cat('HIC:', "\n")
cat(paste0(100*(round(ifr_dt[x==100 & method %in% c('wm_f') & pathogen=='FLU' & iso3=='GBR']$value, 1) -
                  round(ifr_dt[x==100 & method %in% c('f_mean') & pathogen=='FLU' & iso3=='GBR']$value, 1))/
             round(ifr_dt[x==100 & method %in% c('f_mean') & pathogen=='FLU' & iso3=='GBR']$value, 1), '%'), "\n")
cat('\n')

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
int_dt$method <- factor(int_dt$method, levels=unique(int_dt$method))

cat('% OVERESTIMATE DEATHS COMPARED TO PARAMIX', "\n")
cat('SC2 LMIC: [1] VAX YOUNG [2] VAX WORKING [3] VAX OLDER', "\n")
cat(paste0(round(100*(int_dt[method=='f_mid'&pathogen=='SC2'&place=='AFG']$averted_death-int_dt[method=='wm_f'&pathogen=='SC2'&place=='AFG']$averted_death)/
                     int_dt[method=='wm_f'&pathogen=='SC2'&place=='AFG']$averted_death, 2), '%'), "\n")
cat('FLU LMIC: [1] VAX YOUNG [2] VAX WORKING [3] VAX OLDER', "\n")
cat(paste0(round(100*(int_dt[method=='f_mid'&pathogen=='FLU'&place=='AFG']$averted_death-int_dt[method=='wm_f'&pathogen=='FLU'&place=='AFG']$averted_death)/
                     int_dt[method=='wm_f'&pathogen=='FLU'&place=='AFG']$averted_death, 2), '%'), "\n")
cat('SC2 HIC: [1] VAX YOUNG [2] VAX WORKING [3] VAX OLDER', "\n")
cat(paste0(round(100*(int_dt[method=='f_mid'&pathogen=='SC2'&place=='GBR']$averted_death-int_dt[method=='wm_f'&pathogen=='SC2'&place=='GBR']$averted_death)/
                     int_dt[method=='wm_f'&pathogen=='SC2'&place=='GBR']$averted_death, 2), '%'), "\n")
cat('FLU HIC: [1] VAX YOUNG [2] VAX WORKING [3] VAX OLDER', "\n")
cat(paste0(round(100*(int_dt[method=='f_mid'&pathogen=='FLU'&place=='GBR']$averted_death-int_dt[method=='wm_f'&pathogen=='FLU'&place=='GBR']$averted_death)/
                     int_dt[method=='wm_f'&pathogen=='FLU'&place=='GBR']$averted_death, 2), '%'), "\n")
cat('\n')

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

cat('% OVERESTIMATE YLLS COMPARED TO PARAMIX', "\n")
cat('LMIC: [1] SC2 [2] FLU', "\n")
cat(paste0(100*round((int_dt[method=='mean_f' & intervention=='vax_working' & place=='AFG']$averted_yll -
                      int_dt[method=='wm_f' & intervention=='vax_working' & place=='AFG']$averted_yll)/
                     int_dt[method=='wm_f' & intervention=='vax_working' & place=='AFG']$averted_yll,2), '%'), "\n")
cat('HIC: [1] SC2 [2] FLU', "\n")
cat(paste0(100*round((int_dt[method=='mean_f' & intervention=='vax_working' & place=='GBR']$averted_yll -
                          int_dt[method=='wm_f' & intervention=='vax_working' & place=='GBR']$averted_yll)/
                         int_dt[method=='wm_f' & intervention=='vax_working' & place=='GBR']$averted_yll,2), '%'), "\n")
