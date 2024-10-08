
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

FIGEXT ?= png

dat = $(addprefix ${DATDIR}/,$(1))
fig = $(addprefix ${FIGDIR}/,$(patsubst %,%.${FIGEXT},$(1)))

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

${DATDIR}/population.rds: scripts/population.R | ${RENV} ${DATDIR}
	$(call R)

${DATDIR}/lex.rds: scripts/lex.R | ${RENV} ${DATDIR}
	$(call R)

${DATDIR}/odin.rda: scripts/odin.R | ${RENV} ${DATDIR}
	$(call R)

# n.b. for additional pathogens, this would need to be expanded
${DATDIR}/disease_pars.rda: scripts/disease_pars.R $(call dat,population.rds lex.rds)
	$(call R)

${DATDIR}/param_%.rda: scripts/param.R $(call dat,population.rds disease_pars.rda)
	$(call R,$(subst _, ,$*))

allinputs: $(call dat,population.rds lex.rds odin.rda disease_pars.rda $(patsubst %,param_%.rda,${ALLSCN}))

##### OUTPUTS ##################################################################
# simulation time series, distillations, ylls - by various methods

${OUTDIR}/sim_%.rds: scripts/simulate.R $(call dat,population.rds odin.rda param_%.rda) | ${OUTDIR}
	$(call R,$(firstword $(subst _, ,$*)))

ALLSIM := $(patsubst %,${OUTDIR}/sim_%.rds,${ALLSCN})
allsim: ${ALLSIM}

${OUTDIR}/distill_%.rds: scripts/distill.R $(call dat,population.rds param_%.rda) ${OUTDIR}/sim_%.rds
	$(call R,$(firstword $(subst _, ,$*)))

alldistill: $(patsubst %,${OUTDIR}/distill_%.rds,${ALLSCN})

${OUTDIR}/yll_%.rds: scripts/yll.R ${DATDIR}/lex.rds ${OUTDIR}/distill_%.rds
	$(call R,$(firstword $(subst _, ,$*)))

allylls: $(patsubst %,${OUTDIR}/yll_%.rds,${ALLSCN})

# ${OUTDIR}/vaxthreshold_%.rds: $(call dat,population.rds odin.rda param_%.rda) | ${OUTDIR} scripts/yll.R ${DATDIR}/lex.rds ${OUTDIR}/distill_%.rds
#	$(call R,$(firstword $(subst _, ,$*)))

${OUTDIR}/incidence.rds: scripts/incidence.R $(patsubst %,${OUTDIR}/sim_%.rds,${ALLSCN})
	$(call R)

${OUTDIR}/consolidated.rds: scripts/consolidate.R $(patsubst %,${OUTDIR}/yll_%.rds,${ALLSCN})
	$(call R)

allouts: ${OUTDIR}/consolidated.rds ${OUTDIR}/incidence.rds

##### FIGURES ##################################################################

UTIL := ${FIGDIR}/fig_utilities.rda

${UTIL}: scripts/fig_utilities.R | ${FIGDIR} ${RENV}
	$(call R)

# TODO what figs?
#${FIGDIR}/fig1.png: scripts/fig1.R ${OUTDIR}/consolidated.rds | ${FIGDIR}
#	$(call R)

${FIGDIR}/summary.png: scripts/fig_fullsummary.R ${UTIL} \
$(call dat,population.rds lex.rds disease_pars.rda) ${OUTDIR}/incidence.rds
	$(call R)

${FIGDIR}/incidence.png: scripts/fig_incidence.R ${UTIL} ${OUTDIR}/incidence.rds
	$(call R)

${FIGDIR}/averted.png: scripts/fig_averted.R ${UTIL} ${OUTDIR}/incidence.rds
	$(call R)

${FIGDIR}/yll.png: scripts/fig_yll.R ${UTIL} ${OUTDIR}/consolidated.rds
	$(call R)

allfigs: $(call fig,incidence averted yll summary)
