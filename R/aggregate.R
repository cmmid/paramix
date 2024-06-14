
#' @title Transform Parameter Function into Values by Age Bin
#'
#' @description
#' `aggregate_param` integrates a functional relationship between some model
#' feature and age, for example case fatality ratio, into static parameter
#' values for use in a lower-resolution age-binned compartmental model.
#'
#' @param f_age a function, `f(a, ...)` which takes age `a` as the first argument
#' and optionally any other `...`. `f` should be vectorized in `a`
#'
#' @param target_bins a vector, the cutoffs for each age bin. For example, a vector
#' `ages <- c(0, 5, 20, 100)` would correspond to intervals of `[0, 5)`, `[5, 20)`,
#' and `[20, 100]`. Note the first value is a start (rather than cutoff) and
#' that upper limits are exclusive, *except* for the final value.
#'
#' @param population a data.frame, the population by age.
#'
#' @return a vector,
#'
#' @export
aggregate_param <- function(
  f_age,
  target_bins,
  population,
  ...
) {
  
  if(!0 %in% target_bins){target_bins <- c(0, target_bins)}
  population[, mid := (lower+upper+1)/2]
  population[, f_age_val := f_age(mid)]
  population[, age_group := 0]
  for(i in 1:length(target_bins)){
    population[, age_group := age_group + (upper>target_bins[i])]
  }
  
  out <- population[,.(agg_value = weighted.mean(f_age_val, w=pop)), by='age_group']
  
  return(out)

}

#' @title Disaggregate a Compartmental Model Age-Based Outcome
#'
#' @description
#' `disaggregate_value` takes a low-age resolution outcome, for example deaths,
#' and proportionally distributes that outcome into a higher age resolution for
#' use in subsequent analyses like years-life-lost style calculations.
#'
#' @param value a numeric vector, the model output by age group
#'
#' @param f_age the relative rate of the subject outcome, by high
#' resolution age. Note, this should be the same `f_age` as in [aggregate_param()]
#' for parameter / outcome pairs - for example, when the outcome is death and
#' the parameter function is the age-specific case fatality ratio (which would be
#' aggregated to give single lower age resolution compartmental rates).
#'
#' @inheritParams aggregate_param
#'
disaggregate_value <- function(
  value, f_age, target_bins, population, ...
) {
  
  if(!0 %in% target_bins){target_bins <- c(0, target_bins)}
  population[, interval := 1 + upper - lower]
  population <- population[,lapply(.SD, rep, times=interval)]
  population[, pop:=pop/interval]
  population[, interval:=NULL]
  population[, age:=(min(population$lower)):(max(population$upper))]
  population[, f_age_val := f_age(age)]
  population[, f_tot := f_age_val*pop]
  population[, age_group := 0]
  for(i in 1:length(target_bins)){
    population[, age_group := age_group + (upper>target_bins[i])]
  }
  value_dt <- data.table(age_group = sort(unique(population$age_group)),
                         vals = value)
  population <- population[value_dt, on='age_group']
  pop_sum <- population[, .(age_grp_f = sum(f_tot)), by=age_group]
  population <- population[pop_sum, on='age_group']
  population[, disagg_value := vals*f_tot/age_grp_f]
  
  return(population[, c('age','disagg_value')])

}
