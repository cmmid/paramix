
.args <- if (interactive()) c(
  file.path("input", "disease_pars.rda"),
  file.path("input", "param_GBR_FLU.rda"),
  file.path("input", "param_GBR_SC2.rda"),
  file.path("input", "param_AFG_FLU.rda"),
  file.path("input", "param_AFG_SC2.rda")
) else commandArgs(trailingOnly = TRUE)

load(.args[1])
load(.args[2])

r0 <- eigen(disease_pars$FLU$infection_delays[2]*
                   disease_pars$FLU$transmissibility*
                   cmij)$values[1]
r0

r0 <- eigen(disease_pars$SC2$infection_delays[2]*
              disease_pars$SC2$transmissibility*
              cmij)$values[1]
r0

load(.args[4])

r0 <- eigen(disease_pars$FLU$infection_delays[2]*
              disease_pars$FLU$transmissibility*
              cmij)$values[1]
r0

load(.args[5])

r0 <- eigen(disease_pars$SC2$infection_delays[2]*
              disease_pars$SC2$transmissibility*
              cmij)$values[1]
r0

load("input/param_AFG_SC2.rda")
print(cmij)
load("input/param_GBR_SC2.rda")
print(cmij)
