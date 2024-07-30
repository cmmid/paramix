
require(odin)

.args <- if (interactive()) c(
  file.path("input", "odin.rda")
) else commandArgs(trailingOnly = TRUE)

sir_odin <- odin::odin({

  # Number of groups
  no_groups <- user()

  # INITIAL CONDITIONS
  pop[] <- user() # Population size by age/risk group
  V0[] <- user() # Start vaccinated by age/risk group
  VE[] <- user() # Age-specific vaccination efficacy
  I0[] <- user() # Initial infection by age/risk group

  # MODEL PARAMETERS
  trans <- user() # Transmissibility
  gamma1 <- user() # Latent period
  gamma2 <- user() # Infectious period
  cij[,] <- user() # Contact matrix

  sij[,] <- cij[i,j] * I[j] / sum(pop[]) # Transmission matrix
  lambda[] <- trans * sum(sij[i,]) # Force of infection
  newInf[] <- lambda[i] * S[i] # Newly infected
  onsets[] <- gamma1 * E[i]
  removal[] <- gamma2 * I[i]

  # DERIVATIVES
  deriv(S[]) <- -newInf[i]
  deriv(E[]) <- +newInf[i] - onsets[i]
  deriv(I[]) <- +onsets[i] - removal[i]
  deriv(R[]) <- removal[i]

  # Cumulative infections
  deriv(cumI[]) <- newInf[i]

  # Initial values
  initial(S[]) <- pop[i] - VE[i]*V0[i] - I0[i]
  initial(E[]) <- 0
  initial(I[]) <- I0[i]
  initial(R[]) <- VE[i]*V0[i]
  initial(cumI[]) <- 0

  # Dimensions
  dim(pop) <- no_groups
  dim(I0) <- no_groups
  dim(V0) <- no_groups
  dim(VE) <- no_groups
  dim(lambda) <- no_groups
  dim(newInf) <- no_groups
  dim(onsets) <- no_groups
  dim(removal) <- no_groups
  dim(cij) <- c(no_groups, no_groups)
  dim(sij) <- c(no_groups, no_groups)

  dim(S) <- no_groups
  dim(E) <- no_groups
  dim(I) <- no_groups
  dim(R) <- no_groups
  dim(cumI) <- no_groups

})

epidemic_run <- function(
  init_infected,
  transmissibility,
  demography_input,
  contacts,
  init_vaccinated,
  efficacy, infection_delays,
  duration, t0 = 0, interval = 1
){

  # define model timings
  t <- as.numeric(seq(t0, duration, interval))

  # specify the model
  mod <- sir_odin$new(
    no_groups = length(demography_input),
    cij = contacts,
    trans = transmissibility,
    pop = demography_input,
    I0 = init_infected,
    V0 = init_vaccinated,
    VE = efficacy,
    gamma1 = 1/infection_delays[1],
    gamma2 = 1/infection_delays[2]
  )

  # run the model
  y_run <- mod$run(t) |> as.data.table() |> melt.data.table(id.vars = "t")

  return(
    y_run[
      variable %like% "cumI"
    ][
      order(t), .(time = t, value = c(value[1], diff(value))), by = variable
    ][,
      age_group := gsub(".*\\[(\\d+)\\]", "\\1", variable) |> as.integer()
    ][, .(age_group, time, value)] |> setkey(age_group, time)
  )

}

save(sir_odin, epidemic_run, file = tail(.args, 1))
