
###### DEFAULT TARGET: CHECK localdef ##########################################
default: localdef

###### SUPPORT DEFINITIONS #####################################################

# if need to override directories e.g.
-include local.makefile

# convenience make definitions
R = $(strip Rscript $^ $(1) $@)

# analysis directories + build rules
DATDIR ?= input
OUTDIR ?= output
FIGDIR ?= figure

dat = $(addprefix ${DATDIR}/,$(1))

localdef: allfigs

${DATDIR} ${OUTDIR} ${FIGDIR}:
	mkdir -p $@

RENV = .Rprofile

# build renv/library & other renv infrastructure
${RENV}: install.R
	 Rscript --vanilla $^

clean:
	rm -rf ${RENV}
	rm -rf ${DATDIR}
	rm -rf ${OUTDIR}
	rm -rf ${FIGDIR}

##### INPUTS ###################################################################
# population data, life expectancy tables, IFR curves,
# other natural history data, and model definition

# scenarios: iso3c codes + shorthand for pathogens
POPSCN ?= AFG GBR
IFRSCN ?= SC2 FLU

# create all combinations of scenarios
$(foreach pop,${POPSCN},$(foreach ifr,${IFRSCN},$(call $(eval ALLSCN += ${pop}_${ifr}))))

${DATDIR}/population.rds: R/population.R | ${RENV} ${DATDIR}
	$(call R)

${DATDIR}/lex.rds: R/lex.R | ${RENV} ${DATDIR}
	$(call R)

${DATDIR}/odin.rda: R/odin.R | ${RENV} ${DATDIR}
	$(call R)

# n.b. for additional pathogens, this would need to be expanded
${DATDIR}/disease_pars.rda: R/disease_pars.R $(call dat,population.rds lex.rds)
	$(call R)

${DATDIR}/param_%.rda: R/param.R $(call dat,population.rds disease_pars.rda)
	$(call R,$(subst _, ,$*))

allinputs: $(call dat,population.rds lex.rds odin.rda disease_pars.rda $(patsubst %,param_%.rda,${ALLSCN}))

##### OUTPUTS ##################################################################
# simulation time series, distillations, ylls - by various methods

${OUTDIR}/sim_%.rds: R/simulate.R $(call dat,population.rds odin.rda param_%.rda) | ${OUTDIR}
	$(call R,$(firstword $(subst _, ,$*)))

ALLSIM := $(patsubst %,${OUTDIR}/sim_%.rds,${ALLSCN})
allsim: ${ALLSIM}

# output distilled results - use paramix::alembic/distill
${OUTDIR}/distill_%.rds: R/distill.R $(call dat,population.rds param_%.rda) ${OUTDIR}/sim_%.rds
	$(call R,$(firstword $(subst _, ,$*)))

alldistill: $(patsubst %,${OUTDIR}/distill_%.rds,${ALLSCN})

# output computed YLLs
${OUTDIR}/yll_%.rds: R/yll.R ${DATDIR}/lex.rds ${OUTDIR}/distill_%.rds
	$(call R,$(firstword $(subst _, ,$*)))

allylls: $(patsubst %,${OUTDIR}/yll_%.rds,${ALLSCN})

${OUTDIR}/consolidated.rds: R/consolidate.R $(patsubst %,${OUTDIR}/yll_%.rds,${ALLSCN})
	$(call R)

allouts: ${OUTDIR}/consolidated.rds

##### FIGURES ##################################################################

UTIL := ${FIGDIR}/fig_utilities.rda

${UTIL}: R/fig_utilities.R | ${FIGDIR} ${RENV}
	$(call R)

# TODO what figs?
#${FIGDIR}/fig1.png: R/fig1.R ${OUTDIR}/consolidated.rds | ${FIGDIR}
#	$(call R)

${FIGDIR}/incidence.png: R/fig_incidence.R ${UTIL} ${ALLSIM}
	$(call R)

allfigs: ${FIGDIR}/incidence.png
